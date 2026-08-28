#' Set up altdown's altdoc template in a package
#'
#' Copies altdown's `altdoc/` template (`build-site.R`, `altdown.scss`,
#' `quarto_website_static.yml`) into `<path>/altdoc/`, and writes a starter
#' `altdoc/reference.yml` listing every function currently documented in
#' `<path>/man/*.Rd`, in one "All functions" section. Also adds the
#' `.gitignore`/`.Rbuildignore` entries the build needs (see Details) if
#' they're not already there.
#'
#' @details
#' `use_altdown()` adds the following lines to `.gitignore` and
#' `.Rbuildignore` (only the ones not already present in each file):
#'
#' ```
#' # .gitignore
#' altdoc/freeze.rds
#' altdoc/pkgdown.yml
#' _quarto/*
#' !_quarto/_freeze/
#'
#' # .Rbuildignore
#' ^docs$
#' ^altdoc$
#' ^_quarto$
#' ```
#'
#' These mirror what `altdoc::setup_docs(tool = "quarto_website")` itself
#' adds, plus `altdoc/pkgdown.yml`: `altdoc::render_docs()` rewrites that
#' file with a fresh `last_built` timestamp on every build.
#'
#' @param path Path to the target package root. Defaults to the current
#'   directory.
#' @param overwrite Logical. Overwrite the three template files
#'   (`build-site.R`, `altdown.scss`, `quarto_website_static.yml`) if
#'   already present in `<path>/altdoc/`? Default `FALSE`.
#' @return Invisibly, the path to the created `altdoc/` directory.
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
use_altdown <- function(path = ".", overwrite = FALSE) {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop(
      sprintf("No DESCRIPTION file found at '%s' - is this a package root?", path),
      call. = FALSE
    )
  }

  template_dir <- system.file("altdoc-template", package = "altdown")
  if (!nzchar(template_dir)) {
    stop("Could not find altdown's altdoc template - is altdown installed correctly?", call. = FALSE)
  }

  out_dir <- file.path(path, "altdoc")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  template_files <- c("build-site.R", "altdown.scss", "quarto_website_static.yml")
  for (f in template_files) {
    .copy_template_file(file.path(template_dir, f), file.path(out_dir, f), overwrite)
  }

  reference_dest <- file.path(out_dir, "reference.yml")
  if (file.exists(reference_dest)) {
    message(sprintf("Skipping '%s': already exists (delete it to regenerate).", reference_dest))
  } else {
    writeLines(.starter_reference_yml(path), reference_dest)
    message(sprintf("Wrote '%s' (edit this to group your functions).", reference_dest))
  }

  .add_ignore_lines(
    path,
    ".gitignore",
    c(
      "altdoc/freeze.rds",
      "altdoc/pkgdown.yml",
      "_quarto/*",
      "!_quarto/_freeze/"
    )
  )
  .add_ignore_lines(
    path,
    ".Rbuildignore",
    c("^docs$", "^altdoc$", "^_quarto$")
  )

  message(
    "\nNext steps:\n",
    "  1. Edit altdoc/reference.yml to group your functions the way you want.\n",
    "  2. Run: source(\"altdoc/build-site.R\"); build_site()\n",
    "  3. Preview site: altdoc::preview_docs()"
  )

  invisible(out_dir)
}

.copy_template_file <- function(src, dest, overwrite) {
  if (file.exists(dest) && !overwrite) {
    message(sprintf("Skipping '%s': already exists (use overwrite = TRUE to replace).", dest))
    return(invisible())
  }
  file.copy(src, dest, overwrite = TRUE)
  message(sprintf("Wrote '%s'.", dest))
  invisible()
}

# Appends any of `lines` not already present (as a whole line) in
# `<path>/<file_name>`, creating the file first if needed. Mirrors what
# altdoc's own `.add_gitignore()`/`.add_rbuildignore()` do, kept dependency
# -free here (base R only) since altdown doesn't otherwise need fs/cli at
# runtime.
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

# One \name{} per man/*.Rd, sorted, as a single starter "All functions"
# section - same shape as build-site.R's altdoc/reference.yml, minus any
# grouping, which is left for the user to add by hand.
.starter_reference_yml <- function(path) {
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
