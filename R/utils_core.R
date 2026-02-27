#' @importFrom rlang %||%
#' @keywords internal
#' @noRd
NULL

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

# note: validate_required_params() is defined in utils_validation.R (canonical version)

#' Time wrapper for test stubbing
#'
#' @return Current system time
#'
#' @keywords internal
#' @noRd
.now <- function() {
  Sys.time()
}
