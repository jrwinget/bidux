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
#' 
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
#' 
#' @noRd
is_empty <- function(x) {
  is.null(x) || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
}

#' Standardize error messages
#'
#' @param type The type of error: "missing_param", "invalid_param",
#'        "invalid_stage"
#' @param param_name The name of the parameter (if applicable)
#' @param expected The expected value or type (if applicable)
#' @param actual The actual value or type (if applicable)
#' 
#' @return A standardized error message
#' 
#' @keywords internal
#' 
#' @noRd
standard_error_msg <- function(
    type,
    param_name = NULL,
    expected = NULL,
    actual = NULL) {
  switch(
    type,
    missing_param = paste0(
      "Required parameter",
      if (!is.null(param_name)) paste0(" '", param_name, "'"),
      " must be provided."
    ),
    invalid_param = paste0(
      "Parameter",
      if (!is.null(param_name)) paste0(" '", param_name, "'"), 
      " is invalid.",
      if (!is.null(expected) && !is.null(actual)) 
        paste0(" Expected: ", expected, ", Actual: ", actual, ".")
    ),
    invalid_stage = paste0(
      "Expected previous_stage from '", 
      expected, 
      "', but got '", 
      actual, 
      "'. Please ensure you're following the BID framework stages in order."
    ),
    paste0("An error occurred in the implementation of the BID framework.")
  )
}

#' Validate that required parameters are not missing
#'
#' @param ... Named parameters to check
#' 
#' @return NULL invisibly if all checks pass, otherwise stops with an error
#' 
#' @keywords internal
#' 
#' @noRd
validate_required_params <- function(...) {
  args <- list(...)

  for (param_name in names(args)) {
    if (is_empty(args[[param_name]])) {
      stop(standard_error_msg("missing_param", param_name))
    }
  }
  
  invisible(NULL)
}

#' Validate previous stage is from expected function
#'
#' @param previous_stage The previous stage tibble
#' @param expected_stage The expected stage name
#'
#' @return NULL invisibly if check passes, otherwise stops with an error
#'
#' @keywords internal
#'
#' @noRd
validate_previous_stage <- function(previous_stage, expected_stage) {
  if (!tibble::is_tibble(previous_stage) || 
      !("stage" %in% names(previous_stage)) ||
      previous_stage$stage[1] != expected_stage) {
    
    actual_stage <- if (
      tibble::is_tibble(previous_stage) && "stage" %in% names(previous_stage)
    ) {
      previous_stage$stage[1]
    } else {
      "Unknown"
    }
    
    stop(
      standard_error_msg(
        "invalid_stage",
        expected = expected_stage,
        actual = actual_stage
      )
    )
  }
  
  invisible(NULL)
}

#' Create standardized message output
#'
#' @param title The title or heading for the message
#' @param ... Character strings to include as bullet points
#'
#' @return NULL invisibly, used for side effect of printing message
#'
#' @keywords internal
#'
#' @noRd
bid_message <- function(title, ...) {
  bullet_points <- unlist(list(...))
  msg <- paste0(title, "\n", paste0("  - ", bullet_points, collapse = "\n"))
  message(msg)
  invisible(NULL)
}
