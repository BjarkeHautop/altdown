#' Build an altdown-themed altdoc website
#'
#' Builds the package website into `docs/` with
#' [altdoc::render_docs()], styled to mirror pkgdown. Before rendering it
#' generates, from the package sources and `altdoc/altdown.yml`:
#'
#' * the navbar, with a "Get started" link, the function reference, an
#'   "Articles" menu and the changelog;
#' * the home page, built from `README.md` plus a pkgdown-style sidebar
#'   (links, license, citation, developers and the README's badges);
#' * the grouped function reference index;
#' * the "Authors and Citation" page.
#'
#' These generated files are written to `altdoc/` (which altdoc copies into
#' its Quarto project), so they're gitignored by [use_altdown()]. `README.md`
#' itself is never modified.
#'
#' @section Configuration:
#' `altdoc/altdown.yml` (created by [use_altdown()]) can contain:
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
#' * `quarto`: settings merged (with [utils::modifyList()]) over the
#'   generated Quarto configuration, e.g. to add navbar entries or extra
#'   SCSS files.
#'
#' The "Get started" link points to the vignette named after the package, or
#' to the only top-level vignette if there is just one.
#'
#' @param path Path to the package root.
#' @param tooltips Logical. Add hover tooltips to references to the
#'   package's own functions with [reftip::add_tooltips()]? Defaults to
#'   `TRUE` when reftip is installed.
#' @param ... Passed on to [altdoc::render_docs()], e.g. `freeze` or
#'   `parallel`.
#' @return Invisibly, the path to the built site.
#' @export
build_site <- function(
  path = ".",
  tooltips = requireNamespace("reftip", quietly = TRUE),
  ...
) {
  config <- read_config(path)
  warn_legacy_files(path)

  copy_theme_files(path)
  write_home(path)
  build_reference_qmd(path, config)
  build_authors_qmd(path)
  write_quarto_config(path, config)

  altdoc::render_docs(path = path, ...)
  # altdoc copies altdoc/ into docs/ after rendering; drop altdown's sources.
  unlink(file.path(path, "docs", c("altdown.yml", generated_files)))
  fix_man_usage_blocks(path)

  if (isTRUE(tooltips)) {
    if (!requireNamespace("reftip", quietly = TRUE)) {
      cli::cli_abort(
        "{.arg tooltips} is {.val TRUE} but the {.pkg reftip} package isn't installed."
      )
    }
    reftip::add_tooltips(path = path)
  }

  invisible(file.path(path, "docs"))
}

# Files build_site() writes into altdoc/ on every build. use_altdown()
# gitignores them.
generated_files <- c(
  "quarto_website.yml",
  "index.md",
  "reference.qmd",
  "authors.qmd",
  "altdown.scss",
  "altdown-header.html",
  "altdown-after-body.html"
)

theme_files <- c("altdown.scss", "altdown-header.html", "altdown-after-body.html")

read_config <- function(path) {
  config_path <- file.path(path, "altdoc", "altdown.yml")
  if (!file.exists(config_path)) {
    cli::cli_abort(c(
      "No {.path altdoc/altdown.yml} found in {.path {path}}.",
      "i" = "Run {.run altdown::use_altdown()} to set up the package."
    ))
  }
  config <- yaml::read_yaml(config_path)
  if (is.null(config)) list() else config
}

# Files from altdown's earlier copy-the-template setup. altdoc would still
# copy them into the Quarto project, but nothing reads them any more.
warn_legacy_files <- function(path) {
  legacy <- c("build-site.R", "quarto_website_static.yml", "reference.yml")
  found <- legacy[file.exists(file.path(path, "altdoc", legacy))]
  if (length(found) > 0) {
    cli::cli_warn(c(
      "{.path altdoc/{found}} {?is/are} no longer used by altdown and can be deleted.",
      "i" = "The reference index is now configured in {.path altdoc/altdown.yml}."
    ))
  }
}

copy_theme_files <- function(path) {
  src <- system.file("quarto", theme_files, package = "altdown", mustWork = TRUE)
  file.copy(src, file.path(path, "altdoc", theme_files), overwrite = TRUE)
  invisible()
}

# ---- Quarto configuration -----------------------------------------------------
#
# altdoc reads altdoc/quarto_website.yml, substitutes its `$ALTDOC_*`
# variables and writes the result to _quarto/_quarto.yml. Everything is
# filled in here instead, so the file has no variables left for it to
# substitute.

quarto_config <- function(path, config = list()) {
  pkg_name <- desc::desc_get_field("Package", file = path)
  version <- desc::desc_get_field("Version", default = "", file = path)
  gh_url <- pkg_github_url(path)

  navbar <- list(search = TRUE, left = navbar_left(path, config))
  if (!is.na(gh_url)) {
    navbar$right <- list(list(icon = "github", href = gh_url, `aria-label` = "GitHub"))
  }

  settings <- list(
    project = list(type = "website", `output-dir` = "../docs/"),
    language = list(`search-text-placeholder` = " Search for"),
    website = list(
      # after-body script splits the version into its own <small>.
      title = trimws(paste(pkg_name, version)),
      search = list(location = "navbar", type = "textbox"),
      navbar = navbar,
      `page-footer` = list(
        left = data_developed_by(path),
        right = paste(
          "Site built with [altdoc](https://altdoc.etiennebacher.com/) and",
          "[Quarto](https://quarto.org/), using the",
          "[altdown](https://github.com/BjarkeHautop/altdown) theme."
        )
      )
    ),
    format = list(
      html = list(
        theme = list(
          light = c("default", "altdown.scss"),
          dark = c("darkly", "altdown.scss")
        ),
        `code-link` = TRUE,
        toc = TRUE,
        `include-in-header` = "altdown-header.html",
        `include-after-body` = "altdown-after-body.html"
      )
    )
  )

  if (!is.null(config$quarto)) {
    settings <- utils::modifyList(settings, config$quarto)
  }
  settings
}

write_quarto_config <- function(path, config = list()) {
  yaml::write_yaml(
    quarto_config(path, config),
    file.path(path, "altdoc", "quarto_website.yml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
}

navbar_left <- function(path, config = list()) {
  vignettes <- find_vignettes(path)
  getting_started <- find_getting_started_vignette(path, vignettes)
  articles <- select_articles(setdiff(vignettes, getting_started), config$articles)

  left <- list()
  if (!is.null(getting_started)) {
    left <- c(left, list(list(
      text = "Get started",
      file = file.path("vignettes", getting_started)
    )))
  }
  left <- c(left, list(list(text = "Reference", file = "reference.qmd")))
  if (length(articles) > 0) {
    menu <- lapply(articles, function(a) {
      list(
        text = vignette_title(file.path(path, "vignettes", a)),
        file = file.path("vignettes", a)
      )
    })
    left <- c(left, list(list(text = "Articles", menu = menu)))
  }
  news <- news_file(path)
  if (!is.null(news)) {
    left <- c(left, list(list(text = "Changelog", file = news)))
  }
  left
}

# altdoc imports the first of these it finds as NEWS.md (or CHANGELOG.md),
# see altdoc:::.import_basic().
news_file <- function(path) {
  for (name in c("NEWS", "CHANGELOG")) {
    candidates <- c(
      paste0(name, c(".md", ".txt", "", ".Rd")),
      file.path("inst", paste0(name, c(".md", ".txt", "", ".Rd")))
    )
    if (any(file.exists(file.path(path, candidates)))) {
      return(paste0(name, ".md"))
    }
  }
  NULL
}

# ---- home page ----------------------------------------------------------------
#
# altdoc's home page is an index.md that includes README.md. altdoc copies
# altdoc/ into its Quarto project after importing the README, so writing
# altdoc/index.md replaces that page without touching README.md.

write_home <- function(path) {
  readme <- readLines(file.path(path, "README.md"), warn = FALSE, encoding = "UTF-8")
  writeLines(
    enc2utf8(build_website_readme(readme, path)),
    file.path(path, "altdoc", "index.md"),
    useBytes = TRUE
  )
}

build_website_readme <- function(lines, path = ".") {
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
    # `.column-margin` must come *first* here: none of `.page-columns`'s
    # direct children get an explicit `grid-row`, so the browser falls
    # back to sparse row auto-placement, which assigns rows strictly by
    # DOM order and never revisits an earlier row. Putting the sidebar
    # first is what puts it in row 1, where its (taller) content simply
    # overflows down the page next to the rest of the body - that
    # overflow is the entire mechanism that makes it look like a
    # full-height sidebar on desktop. Reordering the DOM to move it
    # below the body on small screens is handled in CSS instead (see
    # `.column-margin`'s `order` in altdown.scss), since that also
    # reorders grid auto-placement without disturbing this.
    c(title, "", sidebar, "", body)
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
