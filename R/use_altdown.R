#' Set up a package to use altdown
#'
#' Writes `altdoc/altdown.yml`, the configuration file [build_site()] reads.
#' It starts with a `reference:` field listing every function currently
#' documented in `<path>/man/*.Rd` in one "All functions" section. Also adds the `.gitignore`/`.Rbuildignore` entries the build
#' needs (see Details) if they're not already there.
#'
#' An existing `altdoc/altdown.yml` is never overwritten. If the package was
#' set up with an earlier version of altdown, the `reference:` field is
#' taken over from its `altdoc/reference.yml`; that file, and the
#' `altdoc/build-site.R` and `altdoc/quarto_website_static.yml` copies, are
#' no longer used and can be deleted.
#'
#' @details
#' `use_altdown()` adds the following lines to `.gitignore` and
#' `.Rbuildignore` (only the ones not already present in each file):
#'
#' ```
#' # .gitignore
#' _quarto/*
#' !_quarto/_freeze/
#' altdoc/freeze.rds
#' altdoc/pkgdown.yml
#' altdoc/quarto_website.yml
#' altdoc/index.md
#' altdoc/reference.qmd
#' altdoc/authors.qmd
#' altdoc/altdown.scss
#' altdoc/altdown-header.html
#' altdoc/altdown-after-body.html
#' altdoc/altdown-pre-render.R
#'
#' # .Rbuildignore
#' ^docs$
#' ^altdoc$
#' ^_quarto$
#' ```
#'
#' The first four mirror what `altdoc::setup_docs(tool = "quarto_website")`
#' adds, plus `altdoc/pkgdown.yml`, which `altdoc::render_docs()` rewrites
#' on every build. The rest are the files [build_site()] generates.
#'
#' @param path Path to the target package root. Defaults to the current
#'   directory.
#' @return Invisibly, the path to the `altdoc/` directory.
#' @export
#'
#' @examples
#' pkg <- tempfile("examplepkg")
#' dir.create(pkg)
#' write(
#'   "Package: examplepkg\nVersion: 0.0.0.9000",
#'   file.path(pkg, "DESCRIPTION")
#' )
#' use_altdown(pkg)
#' list.files(file.path(pkg, "altdoc"))
#' unlink(pkg, recursive = TRUE)
use_altdown <- function(path = ".") {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop(
      sprintf("No DESCRIPTION file found at '%s' - is this a package root?", path),
      call. = FALSE
    )
  }

  out_dir <- file.path(path, "altdoc")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  config_dest <- file.path(out_dir, "altdown.yml")
  if (file.exists(config_dest)) {
    message(sprintf("Skipping '%s': already exists.", config_dest))
  } else {
    writeLines(.starter_config_yml(path), config_dest)
    message(sprintf("Wrote '%s' (edit this to group your functions).", config_dest))
  }

  .add_ignore_lines(
    path,
    ".gitignore",
    c(
      "_quarto/*",
      "!_quarto/_freeze/",
      "altdoc/freeze.rds",
      "altdoc/pkgdown.yml",
      file.path("altdoc", generated_files)
    )
  )
  .add_ignore_lines(
    path,
    ".Rbuildignore",
    c("^docs$", "^altdoc$", "^_quarto$")
  )

  legacy <- c("build-site.R", "quarto_website_static.yml", "reference.yml")
  legacy <- legacy[file.exists(file.path(out_dir, legacy))]
  if (length(legacy) > 0) {
    message(
      "\nThese files from an earlier altdown setup are no longer used and can be deleted:\n",
      paste0("  altdoc/", legacy, collapse = "\n")
    )
  }

  message(
    "\nNext steps:\n",
    "  1. Edit altdoc/altdown.yml to group your functions the way you want.\n",
    "  2. Run: altdown::build_site()\n",
    "  3. Preview site: altdoc::preview_docs()"
  )

  invisible(out_dir)
}

# Appends any of `lines` not already present (as a whole line) in
# `<path>/<file_name>`, creating the file first if needed. Mirrors what
# altdoc's own `.add_gitignore()`/`.add_rbuildignore()` do.
.add_ignore_lines <- function(path, file_name, lines) {
  dest <- file.path(path, file_name)
  existing <- if (file.exists(dest)) readLines(dest, warn = FALSE) else character()

  missing <- lines[!lines %in% existing]
  if (length(missing) == 0) {
    return(invisible())
  }

  writeLines(c(existing, missing), dest)
  message(sprintf(
    "Added %s to '%s'.",
    paste(sprintf("'%s'", missing), collapse = ", "),
    dest
  ))
  invisible()
}

.starter_config_yml <- function(path) {
  c(
    "# Configuration for altdown::build_site(). See ?altdown::build_site for",
    "# all fields, including `articles:` (navbar order) and `quarto:` (extra",
    "# Quarto settings).",
    "",
    "# Groups the function reference index, in the same shape as pkgdown's",
    "# `reference:` field.",
    .starter_reference_yml(path)
  )
}

# The `reference:` field of an earlier altdown setup's altdoc/reference.yml
# if there is one, otherwise one \name{} per man/*.Rd, sorted, as a single
# "All functions" section, leaving any grouping to the user.
.starter_reference_yml <- function(path) {
  legacy <- file.path(path, "altdoc", "reference.yml")
  if (file.exists(legacy)) {
    lines <- readLines(legacy, warn = FALSE)
    start <- grep("^reference:", lines)[1]
    if (!is.na(start)) {
      return(lines[seq(start, length(lines))])
    }
  }

  man_dir <- file.path(path, "man")
  rd_files <- if (dir.exists(man_dir)) {
    sort(list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE))
  } else {
    character()
  }

  names <- vapply(rd_files, function(rd_file) {
    rd <- tools::parse_Rd(rd_file)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    as.character(rd[tags == "\\name"][[1]][[1]])
  }, character(1))

  c(
    "reference:",
    "  - title: All functions",
    "    contents:",
    if (length(names) == 0) "      []" else paste0("      - ", names)
  )
}
