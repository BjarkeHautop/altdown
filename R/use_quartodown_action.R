#' Set up a GitHub Actions workflow that publishes the site
#'
#' Writes `.github/workflows/quartodown.yaml`, a GitHub Actions workflow that
#' builds the site and deploys it to the `gh-pages` branch. It runs
#' [build_site()] on every push, pull request and release, but only deploys
#' when it isn't a pull request. Also adds `^\.github$` to `.Rbuildignore` if
#' it's not already there.
#'
#' Once the workflow's first run has created the `gh-pages` branch, go to the
#' *Pages* section of the repository's settings on GitHub and choose to deploy
#' from that branch.
#'
#' @inheritParams use_quartodown
#' @param overwrite Logical. Replace an existing
#'   `.github/workflows/quartodown.yaml`? By default it's left as is.
#' @return Invisibly, the path to the workflow file.
#' @export
#'
#' @examples
#' pkg <- tempfile("examplepkg")
#' dir.create(pkg)
#' write(
#'   "Package: examplepkg\nVersion: 0.0.0.9000",
#'   file.path(pkg, "DESCRIPTION")
#' )
#' use_quartodown_action(pkg)
#' list.files(pkg, recursive = TRUE, all.files = TRUE)
#' unlink(pkg, recursive = TRUE)
use_quartodown_action <- function(path = ".", overwrite = FALSE) {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    stop(
      sprintf(
        "No DESCRIPTION file found at '%s' - is this a package root?",
        path
      ),
      call. = FALSE
    )
  }

  dest <- file.path(path, ".github", "workflows", "quartodown.yaml")
  if (file.exists(dest) && !overwrite) {
    message(sprintf(
      "Skipping '%s': already exists. Use `overwrite = TRUE` to replace it.",
      dest
    ))
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    src <- system.file(
      "templates",
      "quartodown.yaml",
      package = "quartodown",
      mustWork = TRUE
    )
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("Wrote '%s'.", dest))
  }

  .add_ignore_lines(path, ".Rbuildignore", "^\\.github$")

  message(
    "\nNext steps:\n",
    "  1. Commit and push the workflow.\n",
    "  2. Once its first run has created the gh-pages branch, go to the\n",
    "     Pages section of the repository's settings on GitHub and choose\n",
    "     to deploy from that branch."
  )

  invisible(dest)
}
