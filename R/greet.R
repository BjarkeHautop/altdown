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
