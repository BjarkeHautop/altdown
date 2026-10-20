#' Build the package website
#'
#' Builds the package website into `docs/` with [Quarto](https://quarto.org/),
#' styled to mirror pkgdown.
#'
#' `build_site()` assembles a Quarto website project in `_quarto/` from the
#' package sources and `_quartodown.yml`, then renders it:
#'
#' * a reference page per `man/*.Rd` file, except `\keyword{internal}` ones,
#'   with its examples run;
#' * the vignettes, including those in `vignettes/articles/`;
#' * `NEWS.md` as the changelog, with issues (`#123`) and GitHub users
#'   (`@user`) linked;
#' * `LICENSE.md`, the `LICENSE` file, and `CODE_OF_CONDUCT.md`,
#'   `CONTRIBUTING.md` and `SUPPORT.md` (at the top level or in `.github/`);
#' * the navbar, with a "Get started" link, the function reference, an
#'   "Articles" menu and the changelog;
#' * the home page, built from `README.md` plus a pkgdown-style sidebar
#'   (links, license, citation, developers and the README's badges). An image
#'   with light and dark versions, given as a `<picture>` with a
#'   `<source media="(prefers-color-scheme: dark)">` as on GitHub, follows the
#'   site's light/dark toggle;
#' * the grouped function reference index;
#' * the "Authors and Citation" page.
#'
#' @section Configuration:
#' `_quartodown.yml`, at the package root, is optional; [use_quartodown()]
#' creates one. It can contain:
#'
#' * `reference`: groups the function reference index, in the same shape as
#'   pkgdown's `reference:` field. `contents` entries can be a topic alias or
#'   a selector: `starts_with()`, `ends_with()`, `contains()`, `matches()`,
#'   `has_concept()`, `has_keyword()`, `-` to exclude, and `c()` to combine.
#'   Without it, every topic is listed in one section.
#' * `articles`: the order of the "Articles" navbar menu, as vignette paths
#'   relative to `vignettes/` without extension (e.g. `articles/details`).
#'   Without it, every vignette other than the "Get started" one is listed,
#'   top-level vignettes first, then those in `vignettes/articles/`.
#' * `quarto`: settings merged over the generated Quarto configuration.
#'   Settings are merged key by key: a list of entries (e.g. the navbar's
#'   `left:` or `right:` items) is added to, any other value is replaced, and
#'   `~` removes a setting. For example, to add a navbar link:
#'
#'   ```yaml
#'   quarto:
#'     website:
#'       navbar:
#'         right:
#'           - text: Blog
#'             href: https://example.com
#'   ```
#'
#' The "Get started" link points to the vignette named after the package, or
#' to the only top-level vignette if there is just one.
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
#' @return Invisibly, the path to the built site.
#' @export
#'
#' @examples
#' \dontrun{
#' use_quartodown()
#' build_site()
#' preview_site()
#' }
build_site <- function(
  path = ".",
  tooltips = TRUE,
  freeze = FALSE,
  quiet = TRUE
) {
  config <- read_config(path)
  check_quarto()

  reset_project(path, freeze = freeze)
  copy_vignettes(path)
  copy_figures(path)
  write_man_pages(path)
  import_markdown_files(path)
  copy_theme_files(path)
  write_home(path)
  build_reference_qmd(path, config)
  build_authors_qmd(path)
  write_quarto_config(path, config, freeze = freeze)

  render_project(path, quiet = quiet)
  write_pkgdown_yml(path)

  if (isTRUE(tooltips)) {
    add_tooltips(path)
  }

  invisible(file.path(path, "docs"))
}

theme_files <- c(
  "quartodown.scss",
  "quartodown-header.html",
  "quartodown-after-body.html"
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
  gh_url <- pkg_github_url(path)

  navbar <- list(search = TRUE, left = navbar_left(path, config))
  if (!is.na(gh_url)) {
    navbar$right <- list(list(
      icon = "github",
      href = gh_url,
      `aria-label` = "GitHub"
    ))
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
    format = list(
      html = list(
        theme = list(
          light = c("default", "quartodown.scss"),
          dark = c("darkly", "quartodown.scss")
        ),
        `code-link` = TRUE,
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

# Like utils::modifyList(), but a list of entries (an unnamed list, e.g. the
# navbar's `left:` items) is appended to, where modifyList() would silently
# ignore it. A plain value is replaced, as is a sequence of plain values,
# which yaml reads as an atomic vector, indistinguishable from one value.
# `~` (NULL) removes the setting.
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

navbar_left <- function(path, config = list()) {
  vignettes <- find_vignettes(path)
  getting_started <- find_getting_started_vignette(path, vignettes)
  articles <- select_articles(
    setdiff(vignettes, getting_started),
    config$articles
  )

  left <- list()
  if (!is.null(getting_started)) {
    left <- c(
      left,
      list(list(
        text = "Get started",
        file = file.path("vignettes", getting_started)
      ))
    )
  }
  left <- c(left, list(list(text = "Reference", file = "reference/index.qmd")))
  if (length(articles) > 0) {
    menu <- lapply(articles, function(a) {
      list(
        text = vignette_title(file.path(path, "vignettes", a)),
        file = file.path("vignettes", a)
      )
    })
    left <- c(left, list(list(text = "Articles", menu = menu)))
  }
  if (file.exists(file.path(path, "NEWS.md"))) {
    left <- c(left, list(list(text = "Changelog", file = "NEWS.md")))
  }
  left
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
    # Every direct child of `.page-columns` is its own grid item, and a
    # float can't reach across grid items - so a logo floated right in the
    # title (pkgdown-style) would stretch the heading instead of letting
    # the text wrap beside it. Wrapping title and body in one div makes
    # them a single grid item. None of the items get an explicit
    # `grid-row`, so sparse auto-placement puts that div in row 1 and the
    # sidebar, which comes next in the DOM and sits in a column to its
    # right, in row 1 beside it. On small screens, where the grid collapses
    # to one column, the same DOM order stacks the sidebar below the body,
    # as in pkgdown.
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
# GitHub's way to give a README image a light and a dark version is a
# `<picture>` with a `<source media="(prefers-color-scheme: dark)">`. On the
# site that follows the OS setting rather than the site's own light/dark
# toggle, and Quarto doesn't copy an image referenced only by `srcset`. So
# each such `<picture>` becomes one `<img>` per scheme, with Quarto's
# `.light-content` / `.dark-content` class to show it only in that theme.

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
