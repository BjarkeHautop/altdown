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

pkg_github_url <- function(path = pkg_path) {
  gh_url <- tryCatch(
    Filter(function(u) grepl("github\\.com", u), desc::desc_get_urls(path)),
    error = function(e) character()
  )
  if (length(gh_url) == 0) NA_character_ else sub("/+$", "", gh_url[[1]])
}

data_sidebar_links <- function(path = pkg_path) {
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

data_sidebar_citation <- function(path = pkg_path) {
  pkg_name <- desc::desc_get_field("Package", file = path)
  sidebar_section(
    "Citation",
    sprintf("[Citing %s](authors.html#citation)", pkg_name)
  )
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

  if (length(pkg_authors(path)) != length(authors)) {
    bullets <- c(bullets, "[More about authors...](authors.html)")
  }

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
  lines <- remove_marker_block(
    lines,
    "<!-- badges: start -->",
    "<!-- badges: end -->"
  )

  title <- lines[1]
  body <- lines[-1]
  first_nonblank <- which(nzchar(trimws(body)))[1]
  body <- if (is.na(first_nonblank)) {
    character()
  } else {
    body[seq(first_nonblank, length(body))]
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

# ---- altdoc/reference.qmd generation ---------------------------------------
#
# altdoc has no pkgdown-style `reference:` field for grouping/ordering the
# function index (see https://github.com/etiennebacher/altdoc/issues/326),
# so we build one ourselves: `altdoc/reference.yml` uses the same shape as
# pkgdown's `_pkgdown.yml` `reference:` field, and this reads it plus each
# man/*.Rd's \title{}/\alias{}/\concept{}/\keyword{} to regenerate
# `altdoc/reference.qmd` (same pattern as `update_quarto_settings()` below,
# which regenerates `altdoc/quarto_website.yml` from a source file before
# every build).
#
# `contents:` entries support the same selectors pkgdown does: a plain alias
# name, `starts_with()`/`ends_with()`/`contains()`/`matches()` matched
# against every \alias{}, `has_concept()`/`has_keyword()` matched against
# \concept{}/\keyword{} tags, `-` to exclude, and `c()` to combine several
# selectors on one line.

# Reads every man/*.Rd into a topic record - basename (the name
# .render_one_man() uses for the rendered man/<basename>.qmd target),
# \title{}, and every \alias{}/\concept{}/\keyword{} tag - so reference.yml
# entries can select a function by any of its aliases, and selector
# functions have something to match against.
rd_topics <- function(path = pkg_path) {
  rd_files <- fs::dir_ls(file.path(path, "man"), regexp = "\\.Rd$")

  lapply(rd_files, function(rd_file) {
    rd <- tools::parse_Rd(rd_file)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    tag_text <- function(tag) {
      trimws(vapply(rd[tags == tag], function(x) as.character(x[[1]]), character(1)))
    }

    list(
      basename = fs::path_ext_remove(basename(rd_file)),
      title = trimws(paste(unlist(rd[tags == "\\title"]), collapse = "")),
      alias = tag_text("\\alias"),
      concepts = tag_text("\\concept"),
      keywords = tag_text("\\keyword")
    )
  })
}

# Builds the environment `contents:` entries are evaluated in: every
# \alias{} bound to its topic's integer position, plus the selector
# functions, mirroring pkgdown's `match_env()`.
match_env <- function(topics) {
  env <- new.env(parent = emptyenv())
  assign("-", function(x) -x, envir = env)
  assign("c", function(...) c(...), envir = env)

  alias_vecs <- lapply(topics, `[[`, "alias")
  for (i in seq_along(topics)) {
    for (alias in alias_vecs[[i]]) {
      assign(alias, i, envir = env)
    }
  }

  any_alias <- function(f) {
    which(vapply(alias_vecs, function(x) any(f(x)), logical(1)))
  }

  assign("starts_with", function(x) any_alias(function(a) grepl(paste0("^", x), a)), envir = env)
  assign("ends_with", function(x) any_alias(function(a) grepl(paste0(x, "$"), a)), envir = env)
  assign("contains", function(x) any_alias(function(a) grepl(x, a, fixed = TRUE)), envir = env)
  assign("matches", function(x) any_alias(function(a) grepl(x, a)), envir = env)
  assign(
    "has_concept",
    function(x) which(vapply(topics, function(t) any(t$concepts == x), logical(1))),
    envir = env
  )
  assign(
    "has_keyword",
    function(x) which(vapply(topics, function(t) any(t$keywords %in% x), logical(1))),
    envir = env
  )

  env
}

match_eval <- function(string, env) {
  # Early return in case `string` is already a known alias verbatim.
  literal <- mget(string, envir = env, ifnotfound = list(NULL))[[1]]
  if (is.numeric(literal)) {
    return(as.integer(literal))
  }

  expr <- tryCatch(str2lang(string), error = function(e) NULL)
  if (is.null(expr)) {
    cli::cli_abort(
      "altdoc/reference.yml entry {.val {string}} must be valid R code."
    )
  }

  if (is.symbol(expr)) {
    val <- mget(as.character(expr), envir = env, ifnotfound = list(NULL))[[1]]
    if (!is.numeric(val)) {
      cli::cli_abort(
        "altdoc/reference.yml entry {.val {string}} must be a known \\alias{{}} or selector."
      )
    }
    as.integer(val)
  } else {
    tryCatch(
      as.integer(eval(expr, envir = env)),
      error = function(e) {
        cli::cli_abort(
          "altdoc/reference.yml entry {.val {string}} failed to evaluate.",
          parent = e
        )
      }
    )
  }
}

select_topics <- function(match_strings, topics) {
  if (length(match_strings) == 0) {
    return(integer())
  }

  env <- match_env(topics)
  indexes <- lapply(match_strings, match_eval, env = env)

  all_sign <- function(x, text) {
    if (all(x > 0)) {
      return("+")
    }
    if (all(x < 0)) {
      return("-")
    }
    cli::cli_abort(
      "altdoc/reference.yml entry {.val {text}} must be all positive or all negative."
    )
  }

  sign1 <- all_sign(indexes[[1]], match_strings[[1]])
  sel <- switch(sign1, "+" = integer(), "-" = seq_along(topics))

  for (i in seq_along(indexes)) {
    sign <- all_sign(indexes[[i]], match_strings[[i]])
    sel <- switch(
      sign,
      "+" = union(sel, indexes[[i]]),
      "-" = setdiff(sel, -indexes[[i]])
    )
  }
  sel
}

build_reference_qmd <- function(path = pkg_path) {
  yml_path <- file.path(path, "altdoc", "reference.yml")
  out_path <- file.path(path, "altdoc", "reference.qmd")

  config <- yaml::yaml.load_file(yml_path)
  topics <- rd_topics(path)

  block_indexes <- lapply(
    config$reference,
    function(block) select_topics(as.character(block$contents), topics)
  )

  unlisted <- setdiff(seq_along(topics), unique(unlist(block_indexes)))
  if (length(unlisted) > 0) {
    unlisted_names <- vapply(topics[unlisted], function(t) t$alias[[1]], character(1))
    cli::cli_warn(
      "man/*.Rd defines {.val {unlisted_names}}, which {?is/are} not listed in altdoc/reference.yml and won't appear on the reference page."
    )
  }

  sections <- Map(
    function(block, idx) {
      entries <- vapply(
        idx,
        function(i) {
          topic <- topics[[i]]
          sprintf(
            '<dt><code><a href="man/%s.qmd">%s()</a></code></dt>\n<dd>%s</dd>',
            topic$basename,
            topic$alias[[1]],
            topic$title
          )
        },
        character(1)
      )
      c(
        paste0("## ", block$title),
        "",
        if (!is.null(block$desc)) c(trimws(block$desc), ""),
        '<dl class="ref-index">',
        paste(entries, collapse = "\n\n"),
        "</dl>",
        ""
      )
    },
    config$reference,
    block_indexes
  )

  lines <- c('---', 'title: "Function reference"', '---', "", unlist(sections))
  lines <- lines[-length(lines)] # drop trailing blank line
  writeLines(lines, out_path)
}

# Built straight from the source tree rather
# than `utils::citation(pkg_name)`, which reads an installed copy of the
# package that may be stale.
pkg_citation_meta <- function(path = pkg_path) {
  desc <- desc::description$new(path)
  meta <- as.list(desc$get(desc$fields()))
  if (is.null(meta[["Date/Publication"]])) {
    meta[["Date/Publication"]] <- Sys.time()
  }
  if (!is.null(meta$Title)) {
    meta$Title <- trimws(gsub("\\s+", " ", meta$Title))
  }
  meta
}

has_pkg_citation_file <- function(path = pkg_path) {
  file.exists(file.path(path, "inst", "CITATION"))
}

pkg_citation <- function(path = pkg_path) {
  meta <- pkg_citation_meta(path)
  if (has_pkg_citation_file(path)) {
    utils::readCitationFile(file.path(path, "inst", "CITATION"), meta = meta)
  } else {
    utils::citation(auto = meta)
  }
}

citation_source_note <- function(path = pkg_path) {
  rel <- if (has_pkg_citation_file(path)) "inst/CITATION" else "DESCRIPTION"
  gh_url <- pkg_github_url(path)
  href <- if (is.na(gh_url)) rel else sprintf("%s/blob/HEAD/%s", gh_url, rel)
  sprintf(
    '<p><small class="dont-index">Source: <a href="%s"><code>%s</code></a></small></p>',
    href,
    rel
  )
}

build_authors_qmd <- function(path = pkg_path) {
  author_items <- vapply(
    pkg_authors(path),
    function(x) {
      sprintf(
        "<li><p><strong>%s</strong>. %s.</p></li>",
        author_name(x),
        author_roles_text(x)
      )
    },
    character(1)
  )

  cit <- pkg_citation(path)

  lines <- c(
    '---',
    'title: "Authors and Citation"',
    '---',
    "",
    "## Authors",
    "",
    '<ul class="list-unstyled">',
    author_items,
    "</ul>",
    "",
    "## Citation {#citation}",
    "",
    citation_source_note(path),
    "",
    format(cit, style = "html"),
    "",
    "```bibtex",
    format(cit, style = "bibtex"),
    "```"
  )

  writeLines(lines, file.path(path, "altdoc", "authors.qmd"))
  invisible()
}

# ---- altdoc/quarto_website.yml generation ----------------------------------
vignette_title <- function(qmd_path) {
  fallback <- fs::path_ext_remove(basename(qmd_path))

  lines <- readLines(qmd_path, warn = FALSE, n = 40)
  fence <- which(lines == "---")
  if (length(fence) < 2) {
    return(fallback)
  }

  header <- lines[(fence[1] + 1):(fence[2] - 1)]
  title_line <- grep("^title:", header, value = TRUE)
  if (length(title_line) == 0) {
    return(fallback)
  }

  gsub('^title:\\s*"?|"?\\s*$', "", trimws(title_line[1]))
}

# The single top-level *.qmd in vignettes/ becomes the "Get started" page.
# With more than one, the one named after the package wins;
# otherwise you'll need to disambiguate by hand.
find_getting_started_vignette <- function(path = pkg_path) {
  vig_dir <- file.path(path, "vignettes")
  if (!dir.exists(vig_dir)) {
    cli::cli_abort(
      "No {.path vignettes/} directory found - add a top-level *.qmd vignette to use as the \"Get started\" page."
    )
  }

  candidates <- fs::dir_ls(vig_dir, regexp = "\\.qmd$", recurse = FALSE)
  if (length(candidates) == 0) {
    cli::cli_abort(
      "No top-level *.qmd file found in {.path vignettes/} to use as the \"Get started\" page."
    )
  }
  if (length(candidates) == 1) {
    return(candidates[[1]])
  }

  pkg_name <- desc::desc_get_field("Package", file = path)
  named <- candidates[fs::path_ext_remove(basename(candidates)) == pkg_name]
  if (length(named) == 1) {
    return(named[[1]])
  }

  cli::cli_abort(c(
    "Found multiple top-level vignettes in {.path vignettes/}: {.val {basename(candidates)}}.",
    "i" = "Name the one that should be the \"Get started\" page {.val {paste0(pkg_name, '.qmd')}}, or edit the navbar in altdoc/quarto_website_static.yml by hand."
  ))
}

# Every *.qmd under vignettes/articles/ becomes an "Articles" entry.
find_articles <- function(path = pkg_path) {
  articles_dir <- file.path(path, "vignettes", "articles")
  if (!dir.exists(articles_dir)) {
    return(character())
  }
  sort(fs::dir_ls(articles_dir, regexp = "\\.qmd$"))
}

build_articles_nav <- function(path = pkg_path, indent = "      ") {
  articles <- find_articles(path)
  if (length(articles) == 0) {
    return(character())
  }

  titles <- vapply(articles, vignette_title, character(1))
  rel <- file.path("vignettes/articles", basename(articles))

  item_lines <- unlist(lapply(seq_along(articles), function(i) {
    c(
      sprintf("%s    - text: %s", indent, titles[i]),
      sprintf("%s      file: %s", indent, rel[i])
    )
  }))

  c(sprintf("%s- text: Articles", indent), sprintf("%s  menu:", indent), item_lines)
}

replace_placeholder_line <- function(lines, placeholder, replacement) {
  idx <- which(trimws(lines) == placeholder)
  if (length(idx) == 0) {
    return(lines)
  }
  idx <- idx[1]
  before <- if (idx > 1) lines[seq_len(idx - 1)] else character()
  after <- if (idx < length(lines)) lines[(idx + 1):length(lines)] else character()
  c(before, replacement, after)
}

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

  getting_started <- find_getting_started_vignette(path)
  settings <- gsub(
    "$ALTDOC_GETTING_STARTED",
    file.path("vignettes", basename(getting_started)),
    settings,
    fixed = TRUE
  )

  settings <- replace_placeholder_line(
    settings,
    "$ALTDOC_ARTICLES_NAV",
    build_articles_nav(path)
  )

  writeLines(settings, out_path)
  invisible(settings)
}

# ---- man-page Usage syntax highlighting -------------------------------------
#
# `altdoc:::.rd2qmd()` re-wraps each Rd's Examples section into an
# executable ```` ```{r} ```` fenced chunk, so Quarto syntax-highlights it and
# gives it a copy button - but it leaves the Usage section alone. We make it into
# a ``` r ``` block so it gets syntax highlight+copy.
usage_block_pattern <- "^<pre><code class=['\"]language-[Rr]['\"]>"

# Undoes the HTML-escaping `tools::Rd2HTML()` applies
unescape_rd2html <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  x <- gsub("\\$", "$", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE) # must come last
  x
}

# Replaces the first raw Usage `<pre><code>` block in `lines` (a man/*.qmd's
# content) with a fenced one. Returns NULL if there's no such block, so
# callers can tell "nothing to do" apart from "already fenced".
fence_usage_block <- function(lines) {
  start <- grep(usage_block_pattern, lines)[1]
  if (is.na(start)) {
    return(NULL)
  }

  first <- sub(usage_block_pattern, "", lines[start])
  if (grepl("</code></pre>$", first)) {
    end <- start
    code <- sub("</code></pre>$", "", first)
  } else {
    rest <- lines[seq(start + 1, length(lines))]
    close_at <- which(trimws(rest) == "</code></pre>")[1]
    if (is.na(close_at)) {
      return(NULL)
    }
    end <- start + close_at
    middle <- if (close_at > 1) rest[seq_len(close_at - 1)] else character()
    code <- c(first, middle)
  }

  before <- if (start > 1) lines[seq_len(start - 1)] else character()
  after <- if (end < length(lines)) lines[seq(end + 1, length(lines))] else character()
  c(before, "```r", unescape_rd2html(code), "```", after)
}

# Fences the Usage block in every rendered man/*.qmd, then re-renders just
# the files that changed so the swap actually reaches `docs/`.
fix_man_usage_blocks <- function(path = pkg_path) {
  man_dir <- file.path(path, "_quarto", "man")
  if (!dir.exists(man_dir)) {
    return(invisible())
  }

  for (f in fs::dir_ls(man_dir, regexp = "\\.qmd$")) {
    lines <- readLines(f, warn = FALSE)
    fenced <- fence_usage_block(lines)
    if (is.null(fenced)) {
      next
    }
    writeLines(fenced, f)
    quarto::quarto_render(input = f, quiet = TRUE)
  }
  invisible()
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
  build_authors_qmd(path)
  altdoc::render_docs(path = path, ...)
  fix_man_usage_blocks(path)
}

if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  build_site()
}
