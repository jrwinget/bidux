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
  switch(type,
    missing_param = paste0(
      "Required parameter",
      if (!is.null(param_name)) paste0(" '", param_name, "'"),
      " must be provided."
    ),
    invalid_param = paste0(
      "Parameter",
      if (!is.null(param_name)) paste0(" '", param_name, "'"),
      " is invalid.",
      if (!is.null(expected) && !is.null(actual)) {
        paste0(" Expected: ", expected, ", Actual: ", actual, ".")
      }
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

#' Validate previous stage follows BID framework flow
#'
#' @param previous_stage The previous stage tibble
#' @param current_stage The current stage name

#' @return NULL invisibly if check passes, otherwise stops with an error

#' @keywords internal

#' @noRd
validate_previous_stage <- function(previous_stage, current_stage) {
  if (
    !tibble::is_tibble(previous_stage) || !("stage" %in% names(previous_stage))
  ) {
    stop(
      standard_error_msg(
        "invalid_param",
        "previous_stage",
        "a tibble with a 'stage' column",
        "invalid input"
      )
    )
  }

  prev_stage_name <- previous_stage$stage[1]

  valid_prev_stages <- switch(current_stage,
    "Notice" = c(character(0), "Validate"),
    "Interpret" = c("Notice", "Structure", "Anticipate", "Validate"),
    "Structure" = c("Notice", "Interpret", "Anticipate"),
    "Anticipate" = c("Notice", "Interpret", "Structure"),
    "Validate" = c("Interpret", "Structure", "Anticipate"),
    character(0)
  )

  if (
    length(valid_prev_stages) > 0 && !(prev_stage_name %in% valid_prev_stages)
  ) {
    stop(
      paste0(
        "Invalid previous stage. For the '",
        current_stage,
        "' stage, the previous stage must be one of: ",
        paste(valid_prev_stages, collapse = ", "),
        ". Got '",
        prev_stage_name,
        "' instead."
      )
    )
  }

  invisible(NULL)
}

#' Safe conditional checking utilities
#'
#' These functions provide safe ways to check conditions without getting
#' "missing value where TRUE/FALSE needed" errors.
#'
#' @param obj Object to check
#' @param condition_func Optional function to apply additional conditions
#' @param df A data frame to check (for `safe_df_check`)
#' @param min_rows Minimum number of rows for data frames
#' @param column_name Name of column to access
#' @param default Default value to return if column access fails
#' @param lst A list or vector to check (for `safe_list_access`)
#' @param index Index or name of the element to access in `lst` (for
#'        `safe_list_access`)
#' @param str A character string to check (for `safe_string_check`)
#' @param min_length Minimum number of characters required in `str` (for
#'        `safe_string_check`) 
#'
#' @name safe-utilities
NULL

#' @describeIn safe-utilities Safe conditional checking
safe_check <- function(obj, condition_func = NULL) {
  if (is.null(obj)) {
    return(FALSE)
  }

  if (length(obj) == 0) {
    return(FALSE)
  }

  if (all(is.na(obj))) {
    return(FALSE)
  }

  if (is.null(condition_func)) {
    return(TRUE)
  }

  tryCatch(
    {
      result <- condition_func(obj)
      if (length(result) == 0) {
        return(FALSE)
      }
      if (all(is.na(result))) {
        return(FALSE)
      }
      return(all(result))
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

#' @describeIn safe-utilities Safe data frame checking
safe_df_check <- function(df, min_rows = 1) {
  safe_check(df, function(x) {
    is.data.frame(x) && nrow(x) >= min_rows
  })
}

#' @describeIn safe-utilities Safe column access
safe_column_access <- function(df, column_name, default = NA) {
  if (!safe_df_check(df) || !column_name %in% names(df)) {
    return(default)
  }

  col_value <- df[[column_name]]
  if (length(col_value) == 0) {
    return(default)
  }
  if (all(is.na(col_value))) {
    return(default)
  }

  return(col_value[1])
}

#' @describeIn safe-utilities Safe list/vector access
safe_list_access <- function(lst, index, default = NA) {
  if (is.null(lst) || length(lst) == 0) {
    return(default)
  }
  if (is.numeric(index) && (index < 1 || index > length(lst))) {
    return(default)
  }
  if (is.character(index) && !index %in% names(lst)) {
    return(default)
  }

  tryCatch(
    {
      value <- lst[[index]]
      if (is.null(value) || (length(value) == 1 && is.na(value))) {
        return(default)
      }
      return(value)
    },
    error = function(e) {
      return(default)
    }
  )
}

#' @describeIn safe-utilities Safe string checking
safe_string_check <- function(str, min_length = 1) {
  safe_check(str, function(x) {
    is.character(x) && all(nchar(trimws(x)) >= min_length)
  })
}

#' Function for truncating text in messages
#'
#' @param text Object to check
#' @param max_length Optional function to apply additional conditions
#'
#' @noRd
truncate_text <- function(text, max_length) {
  if (is.null(text) || is.na(text)) {
    return("Not specified")
  }
  if (nchar(text) > max_length) {
    return(paste0(substring(text, 1, max_length), "..."))
  }
  return(text)
}
