#' Set up altdown's altdoc template in a package
#'
#' Copies altdown's `altdoc/` template (`build-site.R`, `altdown.scss`,
#' `quarto_website_static.yml`) into `<path>/altdoc/`, and writes a starter
#' `altdoc/reference.yml` listing every function currently documented in
#' `<path>/man/*.Rd`, in one "All functions" section.
#'
#' The three template files are meant to be used as-is; re-run with
#' `overwrite = TRUE` to pick up altdown updates (this will discard any local
#' edits to them). `reference.yml` is only a starting point - edit it to
#' group the reference index the way you'd like.
#'
#' @param path Path to the target package root. Defaults to the current
#'   directory.
#' @param overwrite Logical. Overwrite files already present in
#'   `<path>/altdoc/`? Default `FALSE`.
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
  if (file.exists(reference_dest) && !overwrite) {
    message(sprintf("Skipping '%s': already exists (use overwrite = TRUE to replace).", reference_dest))
  } else {
    writeLines(.starter_reference_yml(path), reference_dest)
    message(sprintf("Wrote '%s' (edit this to group your functions).", reference_dest))
  }

  message(
    "\nNext steps:\n",
    "  1. Add vignettes/getting-started.qmd (see altdown's own for an example).\n",
    "  2. Edit altdoc/reference.yml to group your functions the way you want.\n",
    "  3. Adjust the navbar links/vignettes in altdoc/quarto_website_static.yml if needed.\n",
    "  4. Run: source(\"altdoc/build-site.R\"); build_site()"
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
