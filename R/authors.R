# Built straight from the source tree rather
# than `utils::citation(pkg_name)`, which reads an installed copy of the
# package that may be stale.
pkg_citation_meta <- function(path) {
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

has_pkg_citation_file <- function(path) {
  file.exists(file.path(path, "inst", "CITATION"))
}

pkg_citation <- function(path) {
  meta <- pkg_citation_meta(path)
  if (has_pkg_citation_file(path)) {
    utils::readCitationFile(file.path(path, "inst", "CITATION"), meta = meta)
  } else {
    utils::citation(auto = meta)
  }
}

citation_source_note <- function(path) {
  rel <- if (has_pkg_citation_file(path)) "inst/CITATION" else "DESCRIPTION"
  gh_url <- pkg_github_url(path)
  # The file isn't part of the site, so there's only a link to the repository.
  source <- if (is.na(gh_url)) {
    sprintf("<code>%s</code>", rel)
  } else {
    sprintf('<a href="%s/blob/HEAD/%s"><code>%s</code></a>', gh_url, rel, rel)
  }
  sprintf('<p><small class="dont-index">Source: %s</small></p>', source)
}

build_authors_qmd <- function(path) {
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

  writeLines(lines, file.path(project_dir(path), "authors.qmd"))
  invisible()
}
