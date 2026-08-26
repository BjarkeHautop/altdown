# Pre-processing script for the altdoc/Quarto build.
#
# Usage:
#   Rscript altdoc/build-site.R
# or, from an R session:
#   source("altdoc/build-site.R")

pkg_path <- "."

# ---- shared helpers --------------------------------------------------------

role_lookup <- function(abbr) {
  roles <- c(
    aut = "author",
    com = "compiler",
    ctr = "contractor",
    ctb = "contributor",
    cph = "copyright holder",
    cre = "maintainer",
    dtc = "data contributor",
    fnd = "funder",
    rev = "reviewer",
    ths = "thesis advisor",
    trl = "translator"
  )
  unname(roles[abbr])
}

# Default roles shown in the sidebar/footer, matching pkgdown's
# `default_roles()`.
default_roles <- function() c("aut", "cre", "fnd")

pkg_authors <- function(path = pkg_path, roles = NULL) {
  authors <- unclass(desc::desc_get_authors(path))
  if (is.null(roles)) {
    return(authors)
  }
  Filter(function(a) any(a$role %in% roles), authors)
}

author_name <- function(x) paste(trimws(paste(x$given, collapse = " ")), x$family)

author_roles_text <- function(x) {
  roles <- paste0(role_lookup(x$role), collapse = ", ")
  substr(roles, 1, 1) <- toupper(substr(roles, 1, 1))
  roles
}

# ---- sidebar section builders (mirrors pkgdown's data_home_sidebar_*()) ---

# Wraps a heading + bullet list, matching pkgdown's `sidebar_section()`
# (R/build-home-index.R): a section with no bullets simply isn't rendered.
sidebar_section <- function(heading, bullets) {
  if (length(bullets) == 0) {
    return(character())
  }
  c(
    paste0("#### ", heading),
    "",
    paste(bullets, collapse = "\n\n")
  )
}

data_sidebar_links <- function(path = pkg_path) {
  gh_url <- tryCatch(
    Filter(function(u) grepl("github\\.com", u), desc::desc_get_urls(path)),
    error = function(e) character()
  )
  bug_reports <- tryCatch(
    desc::desc_get_field("BugReports", default = NA, file = path),
    error = function(e) NA
  )

  links <- character()
  if (length(gh_url) > 0) {
    links <- c(links, sprintf("[Browse source code](%s)", gh_url[[1]]))
  }
  if (!is.na(bug_reports)) {
    links <- c(links, sprintf("[Report a bug](%s)", bug_reports))
  }

  sidebar_section("Links", links)
}

# Badges live between `<!-- badges: start -->`/`<!-- badges: end -->` in
# README.md (the usethis/pkgdown convention).
data_sidebar_devstatus <- function(badges) {
  bullets <- if (length(badges) == 0) character() else paste(badges, collapse = "\n")
  sidebar_section("Dev status", bullets)
}

# License abbreviation -> link table shipped with every R installation
# (`R.home("share")/licenses/license.db`). Same thing pkgdown reads in
# `licenses_db()` (R/build-home-license.R).
autolink_license <- function(license_field, has_license_file) {
  db_path <- file.path(R.home("share"), "licenses", "license.db")
  db <- as.data.frame(read.dcf(db_path), stringsAsFactors = FALSE)

  needs_sss <- !is.na(db$Abbrev) & !is.na(db$Version) & is.na(db$SSS)
  db$SSS[needs_sss] <- paste0(db$Abbrev[needs_sss], "-", db$Version[needs_sss])

  abbr <- ifelse(is.na(db$SSS), db$Abbrev, db$SSS)
  url <- db$URL
  if (has_license_file) {
    abbr <- c(abbr, "LICENSE", "LICENCE")
    url <- c(url, "LICENSE.html", "LICENSE.html")
  }
  keep <- !is.na(abbr)
  abbr <- abbr[keep]
  url <- url[keep]

  x <- license_field
  for (i in seq_along(abbr)) {
    pattern <- paste0("\\b", abbr[i], "\\b")
    replacement <- sprintf("[%s](%s)", abbr[i], url[i])
    x <- gsub(pattern, replacement, x, perl = TRUE)
  }
  x
}

data_sidebar_license <- function(path = pkg_path) {
  license_field <- tryCatch(
    desc::desc_get_field("License", default = NA, file = path),
    error = function(e) NA
  )
  if (is.na(license_field)) {
    return(character())
  }

  has_license_md <- file.exists(file.path(path, "LICENSE.md")) ||
    file.exists(file.path(path, "LICENCE.md"))
  has_license_file <- file.exists(file.path(path, "LICENSE")) ||
    file.exists(file.path(path, "LICENCE"))

  link <- autolink_license(license_field, has_license_file)
  if (has_license_md) {
    link <- c(
      "[Full license](LICENSE.html)",
      sprintf("<small>%s</small>", link)
    )
  }

  sidebar_section("License", link)
}

data_sidebar_community <- function(path = pkg_path) {
  has_file <- function(...) any(file.exists(file.path(path, ...)))

  links <- character()
  if (has_file("CONTRIBUTING.md") || has_file(".github", "CONTRIBUTING.md")) {
    links <- c(links, "[Contributing guide](.github/CONTRIBUTING.md)")
  }
  if (has_file("CODE_OF_CONDUCT.md") || has_file(".github", "CODE_OF_CONDUCT.md")) {
    links <- c(links, "[Code of conduct](CODE_OF_CONDUCT.html)")
  }
  if (has_file("SUPPORT.md") || has_file(".github", "SUPPORT.md")) {
    links <- c(links, "[Getting help](SUPPORT.html)")
  }

  sidebar_section("Community", links)
}

data_sidebar_citation <- function(path = pkg_path) {
  pkg_name <- desc::desc_get_field("Package", file = path)
  sidebar_section("Citation", sprintf("[Citing %s](CITATION.html)", pkg_name))
}

data_sidebar_authors <- function(path = pkg_path, roles = default_roles()) {
  authors <- pkg_authors(path, roles)
  if (length(authors) == 0) {
    return(character())
  }

  bullets <- vapply(
    authors,
    function(x) {
      sprintf(
        "%s\\\n<small>%s</small>",
        author_name(x),
        author_roles_text(x)
      )
    },
    character(1)
  )

  sidebar_section("Developers", bullets)
}

build_sidebar_markdown <- function(path = pkg_path, badges = character()) {
  sections <- list(
    data_sidebar_links(path),
    data_sidebar_license(path),
    data_sidebar_community(path),
    data_sidebar_citation(path),
    data_sidebar_authors(path),
    data_sidebar_devstatus(badges)
  )
  sections <- Filter(length, sections)
  if (length(sections) == 0) {
    return(character())
  }

  body <- unlist(lapply(sections, function(s) c(s, "")))
  body <- body[-length(body)] # drop trailing blank line
  c("::: {.column-margin}", body, ":::")
}

# ---- footer (mirrors pkgdown's footnote_components(), R/build-footer.R) ---

data_developed_by <- function(path = pkg_path, roles = default_roles()) {
  authors <- pkg_authors(path, roles)
  names <- vapply(authors, author_name, character(1))
  sprintf("Developed by %s.", paste(names, collapse = ", "))
}

# ---- README.md -> website-home transform -----------------------------------

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

build_website_readme <- function(lines, path = pkg_path) {
  badges <- extract_readme_badges(lines)
  lines <- remove_marker_block(lines, "<!-- badges: start -->", "<!-- badges: end -->")

  title <- lines[1]
  body <- lines[-1]
  first_nonblank <- which(nzchar(trimws(body)))[1]
  body <- if (is.na(first_nonblank)) character() else body[seq(first_nonblank, length(body))]

  sidebar <- build_sidebar_markdown(path, badges = badges)
  if (length(sidebar) == 0) {
    c(title, "", body)
  } else {
    c(title, "", sidebar, "", body)
  }
}

# ---- altdoc/reference.qmd generation ---------------------------------------
#
# altdoc has no pkgdown-style `reference:` field for grouping/ordering the
# function index (see https://github.com/etiennebacher/altdoc/issues/326),
# so we build one ourselves: `altdoc/reference.yml` uses the same shape as
# pkgdown's `_pkgdown.yml` `reference:` field, and this reads it plus each
# man/*.Rd's \title{} to regenerate `altdoc/reference.qmd` (same pattern as
# `update_quarto_settings()` below, which regenerates
# `altdoc/quarto_website.yml` from a source file before every build).

# Maps every \name{}/\alias{} in man/*.Rd to that Rd file's basename (the
# name .render_one_man() uses for the rendered man/<basename>.qmd target)
# and its \title{}, so reference.yml entries can refer to a function by any
# of its aliases, not just its Rd filename.
rd_topic_index <- function(path = pkg_path) {
  rd_files <- fs::dir_ls(file.path(path, "man"), regexp = "\\.Rd$")

  index <- list()
  for (rd_file in rd_files) {
    rd <- tools::parse_Rd(rd_file)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    aliases <- vapply(rd[tags == "\\alias"], function(x) as.character(x[[1]]), character(1))
    title <- trimws(paste(unlist(rd[tags == "\\title"]), collapse = ""))
    basename <- fs::path_ext_remove(basename(rd_file))

    for (alias in aliases) {
      index[[alias]] <- list(basename = basename, title = title)
    }
  }
  index
}

build_reference_qmd <- function(path = pkg_path) {
  yml_path <- file.path(path, "altdoc", "reference.yml")
  out_path <- file.path(path, "altdoc", "reference.qmd")

  config <- yaml::yaml.load_file(yml_path)
  topics <- rd_topic_index(path)

  listed <- unlist(lapply(config$reference, function(block) block$contents))
  missing_rd <- setdiff(listed, names(topics))
  if (length(missing_rd) > 0) {
    cli::cli_abort(
      "altdoc/reference.yml lists {.val {missing_rd}}, which {?has/have} no matching \\name{{}}/\\alias{{}} in man/*.Rd."
    )
  }
  unlisted <- setdiff(names(topics), listed)
  if (length(unlisted) > 0) {
    cli::cli_warn(
      "man/*.Rd defines {.val {unlisted}}, which {?is/are} not listed in altdoc/reference.yml and won't appear on the reference page."
    )
  }

  sections <- lapply(config$reference, function(block) {
    entries <- vapply(block$contents, function(name) {
      topic <- topics[[name]]
      sprintf(
        '<dt><code><a href="man/%s.qmd">%s()</a></code></dt>\n<dd>%s</dd>',
        topic$basename, name, topic$title
      )
    }, character(1))
    c(
      paste0("## ", block$title), "",
      '<dl class="ref-index">',
      paste(entries, collapse = "\n\n"),
      "</dl>", ""
    )
  })

  lines <- c('---', 'title: "Function reference"', '---', "", unlist(sections))
  lines <- lines[-length(lines)] # drop trailing blank line
  writeLines(lines, out_path)
}

# ---- altdoc/quarto_website.yml generation ----------------------------------

update_quarto_settings <- function(path = pkg_path) {
  static_path <- file.path(path, "altdoc", "quarto_website_static.yml")
  out_path <- file.path(path, "altdoc", "quarto_website.yml")

  settings <- readLines(static_path, warn = FALSE)
  settings <- gsub(
    "$ALTDOC_DEVELOPED_BY",
    data_developed_by(path),
    settings,
    fixed = TRUE
  )
  writeLines(settings, out_path)
  invisible(settings)
}

# ---- entry point ------------------------------------------------------------

render_readme_qmd <- function(path = pkg_path) {
  readme_qmd <- file.path(path, "README.qmd")
  if (!file.exists(readme_qmd)) {
    return(invisible())
  }
  quarto::quarto_render(readme_qmd, output_format = "gfm", quiet = TRUE)
  invisible()
}

build_site <- function(path = pkg_path, ...) {
  render_readme_qmd(path)

  readme_path <- file.path(path, "README.md")
  original_readme <- readLines(readme_path, warn = FALSE)
  on.exit(writeLines(original_readme, readme_path), add = TRUE)

  writeLines(build_website_readme(original_readme, path = path), readme_path)
  update_quarto_settings(path)
  build_reference_qmd(path)
  altdoc::render_docs(path = path, ...)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  build_site()
}
