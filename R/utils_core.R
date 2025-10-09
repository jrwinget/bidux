# ==============================================================================
# CORE UTILITIES
# ==============================================================================
#
# Fundamental utility functions used throughout the bidux package.
#

#' Null Coalescing Operator
#'
#' Returns the left-hand side if it is not NULL, otherwise returns the
#' right-hand side.
#'
#' @param a The left-hand side value.
#' @param b The right-hand side value.
#'
#' @return a if it is not NULL, otherwise b.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Check if input is NULL, NA, or an empty string
#'
#' @param x The value to check
#'
#' @return TRUE if x is NULL, NA, or an empty string, FALSE otherwise
#'
#' @keywords internal
#' @noRd
is_empty <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (all(is.na(x))) {
    return(TRUE)
  }
  if (is.character(x) && all(nchar(trimws(x)) == 0)) {
    return(TRUE)
  }
  return(FALSE)
}

#' Validate that required parameters are not missing
#'
#' @param ... Named parameters to check
#'
#' @return NULL invisibly if all checks pass, otherwise stops with an error
#'
#' @keywords internal
#' @noRd
validate_required_params <- function(...) {
  args <- list(...)

  for (param_name in names(args)) {
    val <- args[[param_name]]
    if (is.null(val) || (is.character(val) && nchar(trimws(val)) == 0)) {
      cli::cli_abort(c(
        "x" = glue::glue("Required parameter '{param_name}' is missing or empty"),
        "i" = "This parameter must be provided and cannot be an empty string"
      ))
    }
  }

  invisible(NULL)
}

#' Time wrapper for test stubbing
#'
#' @return Current system time
#'
#' @keywords internal
#' @noRd
.now <- function() {
  Sys.time()
}
