#' Build the package website
#'
#' Builds the package website into `docs/` with [Quarto](https://quarto.org/).
#'
#' `build_site()` assembles a Quarto website project in `_quarto/` from the
#' package sources and `_quartodown/_quartodown.yml`, then renders it. It uses
#' `README.md` (or, without one, `DESCRIPTION`), `man/`, the vignettes (including those in subfolders),
#' `NEWS.md`, `LICENSE`/`LICENSE.md`, and `CODE_OF_CONDUCT.md`,
#' `CONTRIBUTING.md` and `SUPPORT.md` (at the top level or in `.github/`). See
#' `vignette("quartodown")` for what the site contains.
#'
#' While it builds, the environment variable `IN_QUARTODOWN` is `"true"`, so
#' the package's examples and vignettes can check
#' `identical(Sys.getenv("IN_QUARTODOWN"), "true")`. `IN_QUARTODOWN_PKG` is
#' the name of the package whose site it is, which code shared between
#' packages can check.
#'
#' @section Configuration:
#' `_quartodown/_quartodown.yml` is optional; [use_quartodown()] creates
#' one. It can contain:
#'
#' * `url`: the site's address, if it isn't the first `URL` in `DESCRIPTION`
#'   that isn't a source repository.
#' * `reference`: the sections of the function reference index and the topics
#'   each lists, by name or with selectors such as `starts_with()`. The format
#'   is the same as pkgdown's.
#' * `navbar`: the items on the navbar's `left` and `right`: keywords such as
#'   `reference` and `articles`, vignettes, and Quarto navbar items and menus.
#' * `home`: `file`, the `.md` or `.qmd` file the home page shows instead of
#'   `README.md`, and `sidebar: false` to show it as written, without the
#'   sidebar.
#' * `quarto`: settings merged over the generated Quarto configuration.
#'
#' `_quartodown/extra.scss` and `_quartodown/extra.css`, if they exist, style
#' every page, on top of quartodown's theme. The whole `_quartodown/` folder is
#' copied into the Quarto project, so `quarto` settings can refer to other
#' files in it by the same path.
#'
#' See `vignette("customizing", package = "quartodown")` for the details.
#'
#' @param path Path to the package root.
#' @param tooltips Logical. Link references to the package's own functions
#'   (in code blocks and inline code) to their reference page, with a hover
#'   tooltip showing the function's usage and the first sentence of its
#'   description?
#' @param freeze Logical. Reuse the results of code that ran in an earlier
#'   build, for pages whose source hasn't changed since? Uses Quarto's
#'   [freeze](https://quarto.org/docs/projects/code-execution.html#freeze),
#'   which keeps them in `_quarto/_freeze/`.
#' @param quiet Logical. Hide Quarto's rendering output?
#' @param devel Logical. By default, the examples and vignettes run against
#'   the package installed from `path` into a temporary library. With
#'   `devel = TRUE`, they run against it loaded with [pkgload::load_all()]
#'   instead, which is faster, but code that starts other R processes (e.g.
#'   with callr or parallel workers) won't find the package there.
#' @param preview Logical. Open the built site in a web browser with
#'   [preview_site()]? By default, only in interactive sessions.
#' @return Invisibly, the path to the built site.
#' @export
#'
#' @examples
#' \dontrun{
#' use_quartodown()
#' build_site()
#' }
build_site <- function(
  path = ".",
  tooltips = TRUE,
  freeze = FALSE,
  quiet = TRUE,
  devel = FALSE,
  preview = interactive()
) {
  config <- read_config(path)
  check_quarto()

  # Lets code (`\Sexpr`, `@examplesIf`, examples, vignettes) tell it is
  # building the site, and whose. Quarto's R processes inherit them.
  local_envvars(c(
    IN_QUARTODOWN = "true",
    IN_QUARTODOWN_PKG = desc::desc_get_field("Package", file = path)
  ))

  # The package is installed first: build-stage `\Sexpr` macros in the Rd
  # files, such as Rdpack's `\insertRef{}`, need it, as in `R CMD build`.
  scratch <- tempfile("quartodown-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  envvars <- if (devel) {
    load_all_envvars(path, scratch)
  } else {
    install_envvars(path, scratch)
  }

  reset_project(path, freeze = freeze)
  copy_vignettes(path)
  copy_figures(path)
  with_libpaths(envvars, write_man_pages(path, config))
  import_markdown_files(path)
  copy_theme_files(path)
  copy_site_files(path)
  write_home(path, config)
  build_reference_qmd(path, config)
  build_authors_qmd(path)
  settings <- write_quarto_config(path, config, freeze = freeze)
  write_404(path, settings$website$`site-url`)

  render_project(path, envvars, quiet = quiet)
  write_pkgdown_yml(path, settings$website$`site-url`)
  protect_ansi(path)
  link_code(path)
  ansi_to_html(path)

  if (isTRUE(tooltips)) {
    add_tooltips(path)
  }

  if (isTRUE(preview) && !identical(Sys.getenv("TESTTHAT"), "true")) {
    if (requireNamespace("servr", quietly = TRUE)) {
      serve_site(path, reopen = FALSE)
    } else {
      cli::cli_inform(c(
        "i" = "Install {.pkg servr} to preview the site in your browser."
      ))
    }
  }

  invisible(file.path(path, "docs"))
}

theme_files <- c(
  "quartodown.scss",
  "quartodown-header.html",
  "quartodown-after-body.html",
  "orcid.svg",
  "ror.svg"
)

# The package's site files: _quartodown.yml, plus anything _quartodown.yml
# refers to and extra.scss/extra.css, which style every page. The folder is
# copied into _quarto/ as is, so paths in it are the same from the package root
# and from the project. Quarto doesn't render `_` folders, so nothing in it
# becomes a page.
config_dir <- "_quartodown"
config_file <- file.path(config_dir, "_quartodown.yml")

read_config <- function(path) {
  if (file.exists(file.path(path, "_quartodown.yml"))) {
    cli::cli_warn(c(
      "{.file _quartodown.yml} at the package root is ignored.",
      "i" = "Move it to {.file {config_file}}."
    ))
  }
  config_path <- file.path(path, config_file)
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path)
  if (is.null(config)) {
    return(list())
  }
  warn_unknown_fields(
    names(config),
    c("url", "reference", "navbar", "home", "quarto")
  )
  if (is.list(config$home)) {
    warn_unknown_fields(names(config$home), c("file", "sidebar"), "home")
  }
  config
}

# Warns about fields of _quartodown.yml (or of its `parent:`) that quartodown
# doesn't know, such as typos or pkgdown settings, which are ignored.
warn_unknown_fields <- function(fields, known, parent = NULL) {
  where <- if (is.null(parent)) "" else paste0(" under `", parent, ":`")
  for (field in setdiff(fields, known)) {
    distance <- utils::adist(field, known)[1, ]
    hint <- if (min(distance) <= 2) {
      c("i" = "Did you mean {.field {known[which.min(distance)]}}?")
    } else {
      c("i" = "Known fields: {.field {known}}.")
    }
    cli::cli_warn(c(
      "_quartodown.yml has an unknown field {.field {field}}{where}, which is ignored.",
      hint
    ))
  }
}

copy_theme_files <- function(path) {
  src <- system.file(
    "quarto",
    theme_files,
    package = "quartodown",
    mustWork = TRUE
  )
  file.copy(src, file.path(project_dir(path), theme_files), overwrite = TRUE)
  write(
    ansi_scss_rules(),
    file.path(project_dir(path), "quartodown.scss"),
    append = TRUE
  )
  invisible()
}

# ---- Quarto configuration -----------------------------------------------------

quarto_config <- function(path, config = list(), freeze = FALSE) {
  pkg_name <- desc::desc_get_field("Package", file = path)
  version <- desc::desc_get_field("Version", default = "", file = path)

  navbar <- list(search = TRUE)
  items <- navbar_items(path, config)
  for (side in names(items)) {
    if (length(items[[side]]) > 0) navbar[[side]] <- items[[side]]
  }

  settings <- list(
    project = list(
      type = "website",
      `output-dir` = "../docs/"
    ),
    language = list(`search-text-placeholder` = " Search for"),
    website = list(
      # after-body script splits the version into its own <small>.
      title = trimws(paste(pkg_name, version)),
      search = list(location = "navbar", type = "textbox"),
      navbar = navbar,
      `page-footer` = list(
        left = data_developed_by(path),
        right = paste(
          "Site built with [quartodown](https://github.com/BjarkeHautop/quartodown)",
          "and [Quarto](https://quarto.org/)."
        )
      )
    ),
    # As in the reference examples, output goes in the code's block, each
    # line prefixed with `#>`.
    knitr = list(opts_chunk = list(collapse = TRUE, comment = "#>")),
    format = list(
      html = list(
        theme = list(
          light = c("default", "quartodown.scss"),
          dark = c("darkly", "quartodown.scss")
        ),
        toc = TRUE,
        `include-in-header` = "quartodown-header.html",
        `include-after-body` = "quartodown-after-body.html"
      )
    )
  )

  # After quartodown.scss, so they override it.
  extra_scss <- file.path(config_dir, "extra.scss")
  if (file.exists(file.path(path, extra_scss))) {
    theme <- settings$format$html$theme
    settings$format$html$theme <- list(
      light = c(theme$light, extra_scss),
      dark = c(theme$dark, extra_scss)
    )
  }
  extra_css <- file.path(config_dir, "extra.css")
  if (file.exists(file.path(path, extra_css))) {
    settings$format$html$css <- extra_css
  }

  # For the sitemap, among others.
  url <- site_url(path, config)
  if (!is.na(url)) {
    settings$website$`site-url` <- url
  }
  settings$website <- c(settings$website, social_cards(path))
  logos <- c("man/figures/logo.png", "man/figures/logo.svg")
  logos <- logos[file.exists(file.path(path, logos))]
  if (length(logos) > 0) {
    settings$website$favicon <- logos[[1]]
  }

  if (isTRUE(freeze)) {
    # Reruns a page's code only when its source changed.
    settings$execute <- list(freeze = "auto")
  }

  if (!is.null(config$quarto)) {
    settings <- merge_quarto_config(settings, config$quarto)
  }
  settings
}

# The preview of a link to the site on social media and chat apps: the
# package's title and, as a small square card, its logo. They don't show SVGs.
social_cards <- function(path) {
  title <- desc::desc_get_field("Title", default = NA, file = path)
  logo <- "man/figures/logo.png"
  c(
    list(`open-graph` = TRUE, `twitter-card` = list(`card-style` = "summary")),
    if (!is.na(title)) list(description = trimws(gsub("\\s+", " ", title))),
    # From the site's root, not each page's folder.
    if (file.exists(file.path(path, logo))) list(image = paste0("/", logo))
  )
}

# Like utils::modifyList(), but unnamed lists (e.g. the navbar's `left:`
# items) are appended to. Other values are replaced, and `~` (NULL) removes.
merge_quarto_config <- function(x, val) {
  for (name in names(val)) {
    if (is.list(x[[name]]) && is.list(val[[name]])) {
      x[[name]] <- if (is.null(names(val[[name]]))) {
        c(x[[name]], val[[name]])
      } else {
        merge_quarto_config(x[[name]], val[[name]])
      }
    } else {
      x[[name]] <- val[[name]] # NULL removes it
    }
  }
  x
}

# Returns, invisibly, the settings written.
write_quarto_config <- function(path, config = list(), freeze = FALSE) {
  settings <- quarto_config(path, config, freeze = freeze)
  yaml::write_yaml(
    settings,
    file.path(project_dir(path), "_quarto.yml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
  invisible(settings)
}

# ---- home page ----------------------------------------------------------------

# The home page is `home: file:` from _quartodown.yml, else README.md, else
# made from DESCRIPTION; with a sidebar unless `home: sidebar: false`.
write_home <- function(path, config = list()) {
  file <- config$home$file
  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1) {
      cli::cli_abort(
        "_quartodown.yml `home: file:` must be a single file path."
      )
    }
    if (!file.exists(file.path(path, file))) {
      cli::cli_abort(
        "_quartodown.yml `home: file:` is {.path {file}}, which doesn't exist."
      )
    }
  } else if (file.exists(file.path(path, "README.md"))) {
    file <- "README.md"
  }
  sidebar <- if (is.null(config$home$sidebar)) TRUE else config$home$sidebar
  if (!isTRUE(sidebar) && !isFALSE(sidebar)) {
    cli::cli_abort("_quartodown.yml `home: sidebar:` must be true or false.")
  }
  lines <- if (is.null(file)) {
    description_home(path)
  } else {
    readLines(file.path(path, file), warn = FALSE, encoding = "UTF-8")
  }
  writeLines(
    enc2utf8(build_website_readme(lines, path, sidebar = sidebar)),
    file.path(project_dir(path), "index.qmd"),
    useBytes = TRUE
  )
}

# The page GitHub Pages (and Netlify) show for a URL that doesn't exist, with
# the navbar and search to find the way back. It can be shown at any depth, so
# needs absolute links, which Quarto makes from the `site_url`; without one,
# the host's own page is better than an unstyled one.
write_404 <- function(path, site_url) {
  if (is.null(site_url)) {
    return(invisible())
  }
  writeLines(
    c(
      "---",
      "title: Page not found",
      "toc: false",
      "search: false",
      "---",
      "",
      "Sorry, there is no page at this address. It may have moved, or the link",
      "may be mistyped. Try the search, or go to the [home page](index.qmd)."
    ),
    file.path(project_dir(path), "404.qmd")
  )
}

# A home page for a package without a README: its name and description.
description_home <- function(path) {
  pkg <- desc::desc_get_field("Package", file = path)
  description <- desc::desc_get_field("Description", default = "", file = path)
  c(paste("#", pkg), "", trimws(gsub("\\s+", " ", description)))
}

# With `sidebar = FALSE`, the page is `lines` as written, only made to render
# like on GitHub.
build_website_readme <- function(lines, path = ".", sidebar = TRUE) {
  # A .qmd's front matter stays at the top.
  front_matter <- character()
  ends <- which(lines == "---")
  if (length(lines) > 0 && lines[[1]] == "---" && length(ends) >= 2) {
    front_matter <- lines[seq_len(ends[[2]])]
    lines <- lines[-seq_len(ends[[2]])]
  }
  lines <- github_markdown_to_quarto(lines, mermaid = TRUE)
  lines <- theme_pictures_to_images(lines)
  if (sidebar) {
    lines <- home_page_with_sidebar(lines, path)
  }
  c(front_matter, lines)
}

home_page_with_sidebar <- function(lines, path) {
  badges <- extract_readme_badges(lines)
  lines <- remove_marker_block(
    lines,
    "<!-- badges: start -->",
    "<!-- badges: end -->"
  )

  # Not a `# comment` in a code block.
  is_fence <- grepl("^\\s*(```|~~~)", lines)
  in_code <- cumsum(is_fence) %% 2 == 1 | is_fence
  title_idx <- which(grepl("^# ", lines) & !in_code)[1]
  if (is.na(title_idx)) {
    title <- character()
  } else {
    title <- lines[title_idx]
    lines <- lines[-seq_len(title_idx)]
  }
  first_nonblank <- which(nzchar(trimws(lines)))[1]
  body <- if (is.na(first_nonblank)) {
    character()
  } else {
    lines[seq(first_nonblank, length(lines))]
  }

  sidebar <- build_sidebar_markdown(path, badges = badges)
  if (length(sidebar) == 0) {
    c(title, "", body)
  } else {
    # One grid item for title and body, so text can wrap around a logo
    # floated right in the title.
    c("::::: {.quartodown-home}", title, "", body, ":::::", "", sidebar)
  }
}

extract_readme_badges <- function(lines) {
  start <- which(lines == "<!-- badges: start -->")
  end <- which(lines == "<!-- badges: end -->")
  if (length(start) != 1 || length(end) != 1 || end <= start) {
    return(character())
  }
  badges <- lines[seq_len(end - start - 1) + start]
  badges[nzchar(trimws(badges))]
}

remove_marker_block <- function(lines, start_marker, end_marker) {
  start <- which(lines == start_marker)
  end <- which(lines == end_marker)
  if (length(start) != 1 || length(end) != 1 || end <= start) {
    return(lines)
  }
  lines[-seq(start, end)]
}

# ---- light/dark images --------------------------------------------------------
#
# A README `<picture>` with a `prefers-color-scheme` `<source>` would follow
# the OS rather than the site's toggle, and Quarto doesn't copy `srcset`
# images. Each becomes one `.light-content`/`.dark-content` `<img>` instead.

theme_pictures_to_images <- function(lines) {
  if (!any(grepl("<picture", lines, fixed = TRUE))) {
    return(lines)
  }
  text <- paste(lines, collapse = "\n")
  m <- gregexpr("(?s)<picture\\b.*?</picture>", text, perl = TRUE)
  regmatches(text, m) <- lapply(regmatches(text, m), function(pictures) {
    vapply(pictures, theme_picture_to_images, character(1), USE.NAMES = FALSE)
  })
  # The sentinel keeps trailing empty lines, which strsplit() would drop.
  utils::head(strsplit(paste0(text, "\n."), "\n", fixed = TRUE)[[1]], -1)
}

theme_picture_to_images <- function(picture) {
  img <- regmatches(picture, regexpr("<img\\b[^>]*>", picture, perl = TRUE))
  if (length(img) == 0) {
    return(picture)
  }
  sources <- regmatches(
    picture,
    gregexpr("<source\\b[^>]*>", picture, perl = TRUE)
  )[[1]]
  media <- vapply(
    sources,
    html_attr,
    character(1),
    name = "media",
    USE.NAMES = FALSE
  )
  scheme_pattern <- "^.*prefers-color-scheme\\s*:\\s*(light|dark).*$"
  is_scheme <- !is.na(media) & grepl(scheme_pattern, media, perl = TRUE)
  if (!any(is_scheme)) {
    return(picture)
  }
  schemes <- sub(scheme_pattern, "\\1", media[is_scheme], perl = TRUE)
  sources <- sources[is_scheme][!duplicated(schemes)]
  schemes <- unique(schemes)

  images <- vapply(
    seq_along(sources),
    function(i) {
      srcset <- html_attr(sources[i], "srcset")
      # The first candidate's URL, without a width or density descriptor.
      src <- sub(
        "\\s.*$",
        "",
        trimws(strsplit(srcset, ",", fixed = TRUE)[[1]][1])
      )
      with_theme_class(set_html_attr(img, "src", src), schemes[i])
    },
    character(1)
  )
  # The <img> is the fallback, so it covers the scheme no <source> names.
  fallback <- setdiff(c("light", "dark"), schemes)
  if (length(fallback) == 1) {
    images <- c(with_theme_class(img, fallback), images)
  }
  paste(images, collapse = "")
}

with_theme_class <- function(tag, scheme) {
  class <- html_attr(tag, "class")
  class <- if (is.na(class)) "" else class
  set_html_attr(tag, "class", trimws(paste(class, paste0(scheme, "-content"))))
}

html_attr_pattern <- function(name) {
  sprintf("(?i)\\s%s\\s*=\\s*(\"[^\"]*\"|'[^']*')", name)
}

# The value of attribute `name` in the HTML start tag `tag`, or NA.
html_attr <- function(tag, name) {
  m <- regmatches(tag, regexec(html_attr_pattern(name), tag, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(NA_character_)
  }
  substr(m[2], 2, nchar(m[2]) - 1)
}

set_html_attr <- function(tag, name, value) {
  new <- sprintf(" %s=\"%s\"", name, value)
  m <- regexpr(html_attr_pattern(name), tag, perl = TRUE)
  if (m == -1) {
    m <- regexpr("\\s*/?>$", tag, perl = TRUE)
    regmatches(tag, m) <- paste0(new, regmatches(tag, m))
  } else {
    regmatches(tag, m) <- new
  }
  tag
}
