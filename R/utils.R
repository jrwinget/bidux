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

#' Create standardized message output
#'
#' @param title The title or heading for the message
#' @param ... Character strings to include as bullet points
#'
#' @return NULL invisibly, used for side effect of printing message
#'
#' @keywords internal
#' @noRd
bid_message <- function(title, ...) {
  # If title is NULL or empty, do nothing
  if (is.null(title) || (is.character(title) && nchar(trimws(title)) == 0)) {
    return(invisible(NULL))
  }

  bullet_points <- unlist(list(...), use.names = FALSE)

  # Filter out bullet points that are NULL, NA, or zero‐length
  valid_bullets <- bullet_points[
    !vapply(bullet_points, is.null, logical(1)) &
      !vapply(bullet_points, function(x) all(is.na(x)), logical(1)) &
      (nchar(trimws(as.character(bullet_points))) > 0)
  ]

  if (length(valid_bullets) == 0) {
    return(invisible(NULL))
  }

  msg <- paste0(
    title,
    "\n",
    paste0("  - ", valid_bullets, collapse = "\n")
  )
  cat(msg, "\n")
  invisible(NULL)
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
#' @noRd
standard_error_msg <- function(
  type,
  param_name = NULL,
  expected = NULL,
  actual = NULL
) {
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
      if (!is.null(expected) && !is.null(actual)) {
        paste0(" Expected: ", expected, ", Actual: ", actual, ".")
      } else {
        ""
      }
    ),
    invalid_stage = paste0(
      "Invalid stage: ",
      actual,
      ". Must be one of: ",
      paste(expected, collapse = ", "),
      "."
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
#' @param previous_stage The previous stage, either a bid_stage/tibble with a 'stage' column,
#'        or a single character string naming the stage, or NULL if fresh start.
#' @param current_stage The current stage name (character)
#'
#' @return NULL invisibly if check passes, otherwise stops or warns
#'
#' @keywords internal
#' @noRd
validate_previous_stage <- function(previous_stage = NULL, current_stage) {
  # Define the five valid stage names and their immediate predecessor
  valid_stages <- c(
    "Notice",
    "Interpret",
    "Structure",
    "Anticipate",
    "Validate"
  )
  stage_order <- valid_stages

  # 1) Check that current_stage is exactly one of the five
  if (
    !(is.character(current_stage) &&
      length(current_stage) == 1 &&
      current_stage %in% valid_stages)
  ) {
    stop(standard_error_msg(
      "invalid_stage",
      actual = current_stage,
      expected = valid_stages
    ))
  }

  # 2) If previous_stage is NULL:
  #    - Only allow silently if current_stage == "Notice"
  if (is.null(previous_stage)) {
    if (current_stage == "Notice") {
      return(invisible(NULL))
    } else {
      # Not Notice but no previous provided → issue warning about unusual progression
      warning(
        paste0(
          "Unusual stage progression: (none) -> ",
          current_stage
        ),
        call. = FALSE
      )
      return(invisible(NULL))
    }
  }

  # 3) Coerce previous_stage into a single character stage name
  if (inherits(previous_stage, "bid_stage")) {
    prev_stage_name <- attr(previous_stage, "stage")
  } else if (
    tibble::is_tibble(previous_stage) && "stage" %in% names(previous_stage)
  ) {
    prev_stage_name <- previous_stage$stage[1]
  } else if (is.character(previous_stage) && length(previous_stage) == 1) {
    prev_stage_name <- previous_stage
  } else {
    stop(standard_error_msg(
      "invalid_stage",
      actual = as.character(previous_stage),
      expected = valid_stages
    ))
  }

  # 4) Ensure prev_stage_name is one of the five valid stages
  if (!(prev_stage_name %in% valid_stages)) {
    stop(standard_error_msg(
      "invalid_stage",
      actual = prev_stage_name,
      expected = valid_stages
    ))
  }

  # 5) Determine the *immediate* predecessor in the linear order
  idx_current <- match(current_stage, stage_order)
  idx_prev <- match(prev_stage_name, stage_order)

  # If current_stage is "Notice", any non-NULL previous is unusual
  if (idx_current == 1) {
    warning(
      paste0(
        "Unusual stage progression: ",
        prev_stage_name,
        " -> ",
        current_stage
      ),
      call. = FALSE
    )
    return(invisible(NULL))
  }

  # For any other stage, check if prev_stage_name == the stage immediately before it
  expected_prev <- stage_order[idx_current - 1]
  if (prev_stage_name != expected_prev) {
    warning(
      paste0(
        "Unusual stage progression: ",
        prev_stage_name,
        " -> ",
        current_stage
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Safe conditional checking utilities
#'
#' These functions provide safe ways to check conditions without getting
#' "missing value where TRUE/FALSE needed" errors.
#'
#' @name safe-utilities
#' @keywords internal
#' @noRd
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
      all(result)
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
  col_value[1]
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
      value
    },
    error = function(e) {
      default
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
#' @param max_length Maximum length before truncation
#'
#' @noRd
truncate_text <- function(text, max_length) {
  if (is.null(text) || is.na(text)) {
    return("")
  }

  text_str <- as.character(text)
  if (nchar(text_str) <= max_length) {
    return(text_str)
  }

  # Truncate to (max_length - 3), then append "..."
  if (max_length <= 3) {
    return(strrep(".", min(3, max_length)))
  }
  truncated <- substr(text_str, 1, max_length - 3)
  paste0(truncated, "...")
}
