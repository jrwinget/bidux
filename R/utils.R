#' Null Coalescing Operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the right-hand side.
#'
#' @param a The left-hand side value.
#' @param b The right-hand side value.
#' @return a if it is not NULL, otherwise b.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
