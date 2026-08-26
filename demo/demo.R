# Toy API that exists only to give altdown's own vignette
# (altdoc/demo/demo.qmd) real code to run and document. altdoc/build-site.R
# stages this file into R/, its generated man pages into man/, and demo.qmd
# into vignettes/, reinstalls the package so the vignette can actually call
# these functions, then removes everything staged and reinstalls the
# package clean again - all only for the duration of the build. None of it
# ships in the package or lingers in the working tree between builds.

#' Greet someone
#'
#' A toy function that returns a greeting message. Used only to give this
#' example package something to document.
#'
#' @param name A character string with the name to greet.
#' @param loud Logical. If `TRUE`, the greeting is shouted (all caps).
#'
#' @return A character string with the greeting.
#' @export
#'
#' @examples
#' greet("world")
#' greet("world", loud = TRUE)
greet <- function(name = "world", loud = FALSE) {
  msg <- paste0("Hello, ", name, "!")
  if (loud) {
    msg <- toupper(msg)
  }
  msg
}

#' Add two numbers
#'
#' @param x A numeric value.
#' @param y A numeric value.
#'
#' @return The sum of `x` and `y`.
#' @export
#'
#' @examples
#' add(1, 2)
add <- function(x, y) {
  x + y
}
