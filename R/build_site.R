#' Build the package website
#'
#' Builds the package website into `docs/` with [Quarto](https://quarto.org/).
#'
#' `build_site()` assembles a Quarto website project in `_quarto/` from the
#' package sources and `_quartodown.yml`, then renders it. It uses
#' `README.md`, `man/`, the vignettes (including those in subfolders),
#' `NEWS.md`, `LICENSE`/`LICENSE.md`, and `CODE_OF_CONDUCT.md`,
#' `CONTRIBUTING.md` and `SUPPORT.md` (at the top level or in `.github/`). See
#' `vignette("quartodown")` for what the site contains.
#'
#' @section Configuration:
#' `_quartodown.yml`, at the package root, is optional; [use_quartodown()]
#' creates one. It can contain:
#'
#' * `reference`: the sections of the function reference index and the topics
#'   each lists, by name or with selectors such as `starts_with()`. The format
#'   is the same as pkgdown's.
#' * `navbar`: the items on the navbar's `left` and `right`: keywords such as
#'   `reference` and `articles`, vignettes, and Quarto navbar items and menus.
#' * `quarto`: settings merged over the generated Quarto configuration.
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
  write_home(path)
  build_reference_qmd(path, config)
  build_authors_qmd(path)
  write_quarto_config(path, config, freeze = freeze)

  render_project(path, envvars, quiet = quiet)
  write_pkgdown_yml(path)
  link_code(path)

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

config_file <- "_quartodown.yml"

read_config <- function(path) {
  config_path <- file.path(path, config_file)
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path)
  if (is.null(config)) list() else config
}

copy_theme_files <- function(path) {
  src <- system.file(
    "quarto",
    theme_files,
    package = "quartodown",
    mustWork = TRUE
  )
  file.copy(src, file.path(project_dir(path), theme_files), overwrite = TRUE)
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

  if (isTRUE(freeze)) {
    # Reruns a page's code only when its source changed.
    settings$execute <- list(freeze = "auto")
  }

  if (!is.null(config$quarto)) {
    settings <- merge_quarto_config(settings, config$quarto)
  }
  settings
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

write_quarto_config <- function(path, config = list(), freeze = FALSE) {
  yaml::write_yaml(
    quarto_config(path, config, freeze = freeze),
    file.path(project_dir(path), "_quarto.yml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
}

# ---- home page ----------------------------------------------------------------

write_home <- function(path) {
  readme <- readLines(
    file.path(path, "README.md"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  writeLines(
    enc2utf8(build_website_readme(readme, path)),
    file.path(project_dir(path), "index.md"),
    useBytes = TRUE
  )
}

build_website_readme <- function(lines, path = ".") {
  lines <- theme_pictures_to_images(lines)
  badges <- extract_readme_badges(lines)
  lines <- remove_marker_block(
    lines,
    "<!-- badges: start -->",
    "<!-- badges: end -->"
  )

  title_idx <- grep("^# ", lines)[1]
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
