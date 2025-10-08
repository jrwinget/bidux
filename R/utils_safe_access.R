# ==============================================================================
# SAFE ACCESS UTILITIES
# ==============================================================================
#
# Defensive programming utilities for safe data access with defaults.
#

#' Safe conditional checking
#'
#' @param obj The object to check
#' @param condition_func Optional function to apply for checking
#'
#' @return TRUE if object passes checks, FALSE otherwise
#'
#' @keywords internal
#' @noRd
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

#' Safe data frame checking
#'
#' @param df Data frame to check
#' @param min_rows Minimum number of rows required
#'
#' @return TRUE if data frame meets criteria, FALSE otherwise
#'
#' @keywords internal
#' @noRd
safe_df_check <- function(df, min_rows = 1) {
  safe_check(df, function(x) {
    is.data.frame(x) && nrow(x) >= min_rows
  })
}

#' Safe column access from data frame
#'
#' @param df Data frame to access
#' @param column_name Name of column to access
#' @param default Default value to return if column doesn't exist or is empty
#'
#' @return First value from column or default
#'
#' @keywords internal
#' @noRd
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

#' Safe list or vector access
#'
#' @param lst List or vector to access
#' @param index Index (numeric or character) to access
#' @param default Default value to return if index doesn't exist
#'
#' @return Value at index or default
#'
#' @keywords internal
#' @noRd
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

#' Safe string checking
#'
#' @param str String or vector to check
#' @param min_length Minimum length required for strings
#'
#' @return TRUE if strings meet length criteria, FALSE otherwise
#'
#' @keywords internal
#' @noRd
safe_string_check <- function(str, min_length = 1) {
  safe_check(str, function(x) {
    is.character(x) && all(nchar(trimws(x)) >= min_length)
  })
}

#' Safe access to data story elements
#'
#' @param data_story Data story object (list or bid_data_story S3 class)
#' @param element Element name to access
#'
#' @return Element value or NA_character_
#'
#' @keywords internal
#' @noRd
safe_data_story_access <- function(data_story, element) {
  if (is.null(data_story)) {
    return(NA_character_)
  }

  # handle new bid_data_story S3 class
  if (inherits(data_story, "bid_data_story")) {
    # for new S3 class, map elements to the appropriate structure
    value <- switch(element,
      "hook" = safe_list_access(data_story$variables, "hook", NA_character_),
      "context" = data_story$context %||% NA_character_,
      "tension" = safe_list_access(data_story$variables, "tension", NA_character_),
      "resolution" = safe_list_access(data_story$relationships, "resolution", NA_character_),
      "audience" = safe_list_access(data_story$metadata, "audience", NA_character_),
      "metrics" = safe_list_access(data_story$metadata, "metrics", NA_character_),
      "visual_approach" = safe_list_access(data_story$metadata, "visual_approach", NA_character_),
      NA_character_
    )
  } else if (is.list(data_story) && element %in% names(data_story)) {
    # handle legacy list format
    value <- data_story[[element]]
  } else {
    return(NA_character_)
  }

  # validate and return the value
  if (
    !is.null(value) &&
      !is.na(value) &&
      nchar(trimws(as.character(value))) > 0
  ) {
    return(as.character(value))
  }

  return(NA_character_)
}
