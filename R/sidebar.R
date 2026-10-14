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

pkg_authors <- function(path, roles = NULL) {
  authors <- unclass(desc::desc_get_authors(path))
  if (is.null(roles)) {
    return(authors)
  }
  Filter(function(a) any(a$role %in% roles), authors)
}

author_name <- function(x) {
  paste(trimws(paste(x$given, collapse = " ")), x$family)
}

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

cran_url_if_available <- function(pkg_name) {
  if (is.na(pkg_name) || !requireNamespace("httr2", quietly = TRUE)) {
    return(NA_character_)
  }
  cran_url <- paste0("https://cloud.r-project.org/package=", pkg_name)
  resp <- tryCatch(
    httr2::req_perform(httr2::req_error(
      httr2::request(cran_url),
      function(resp) FALSE
    )),
    error = function(e) NULL
  )
  if (!is.null(resp) && !httr2::resp_is_error(resp)) {
    cran_url
  } else {
    NA_character_
  }
}

pkg_github_url <- function(path) {
  gh_url <- tryCatch(
    Filter(function(u) grepl("github\\.com", u), desc::desc_get_urls(path)),
    error = function(e) character()
  )
  if (length(gh_url) == 0) NA_character_ else sub("/+$", "", gh_url[[1]])
}

data_sidebar_links <- function(path) {
  gh_url <- pkg_github_url(path)
  gh_url <- if (is.na(gh_url)) character() else gh_url
  bug_reports <- tryCatch(
    desc::desc_get_field("BugReports", default = NA, file = path),
    error = function(e) NA
  )
  pkg_name <- tryCatch(
    desc::desc_get_field("Package", default = NA, file = path),
    error = function(e) NA
  )

  links <- character()
  cran_url <- cran_url_if_available(pkg_name)
  if (!is.na(cran_url)) {
    links <- c(links, sprintf("[View on CRAN](%s)", cran_url))
  }
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
  bullets <- if (length(badges) == 0) {
    character()
  } else {
    paste(badges, collapse = "\n")
  }
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

data_sidebar_license <- function(path) {
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

data_sidebar_community <- function(path) {
  has_file <- function(...) any(file.exists(file.path(path, ...)))

  links <- character()
  if (has_file("CONTRIBUTING.md") || has_file(".github", "CONTRIBUTING.md")) {
    contributing_rel <- if (file.exists(file.path(path, "CONTRIBUTING.md"))) {
      "CONTRIBUTING.md"
    } else {
      ".github/CONTRIBUTING.md"
    }
    gh_url <- pkg_github_url(path)
    contributing_url <- if (is.na(gh_url)) {
      contributing_rel
    } else {
      sprintf("%s/blob/HEAD/%s", gh_url, contributing_rel)
    }
    links <- c(
      links,
      sprintf("[Contributing guide](%s)", contributing_url)
    )
  }
  if (
    has_file("CODE_OF_CONDUCT.md") || has_file(".github", "CODE_OF_CONDUCT.md")
  ) {
    links <- c(links, "[Code of conduct](CODE_OF_CONDUCT.html)")
  }
  if (has_file("SUPPORT.md") || has_file(".github", "SUPPORT.md")) {
    links <- c(links, "[Getting help](SUPPORT.html)")
  }

  sidebar_section("Community", links)
}

data_sidebar_citation <- function(path) {
  pkg_name <- desc::desc_get_field("Package", file = path)
  sidebar_section(
    "Citation",
    sprintf("[Citing %s](authors.html#citation)", pkg_name)
  )
}

data_sidebar_authors <- function(path, roles = default_roles()) {
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

  if (length(pkg_authors(path)) != length(authors)) {
    bullets <- c(bullets, "[More about authors...](authors.html)")
  }

  sidebar_section("Developers", bullets)
}

build_sidebar_markdown <- function(path, badges = character()) {
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

data_developed_by <- function(path, roles = default_roles()) {
  authors <- pkg_authors(path, roles)
  names <- vapply(authors, author_name, character(1))
  sprintf("Developed by %s.", paste(names, collapse = ", "))
}

