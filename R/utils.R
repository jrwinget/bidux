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

#' Validate character parameter with length and content checks
#'
#' @param value The value to validate
#' @param param_name Name of the parameter
#' @param min_length Minimum length after trimming
#' @param allow_null Whether NULL values are allowed
#'
#' @return NULL invisibly if valid, stops with error otherwise
#'
#' @keywords internal
#' @noRd
validate_character_param <- function(value, param_name, min_length = 1, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) return(invisible(NULL))
    stop(paste0("'", param_name, "' cannot be NULL"), call. = FALSE)
  }
  
  if (!is.character(value) || length(value) != 1) {
    stop(paste0("'", param_name, "' must be a single character string"), call. = FALSE)
  }
  
  clean_value <- trimws(value)
  if (nchar(clean_value) < min_length) {
    stop(paste0("'", param_name, "' cannot be empty or contain only whitespace"), call. = FALSE)
  }
  
  invisible(NULL)
}

#' Validate list parameter structure
#'
#' @param value The list to validate
#' @param param_name Name of the parameter
#' @param required_names Required list element names
#' @param allow_null Whether NULL values are allowed
#'
#' @return NULL invisibly if valid, stops with error otherwise
#'
#' @keywords internal
#' @noRd
validate_list_param <- function(value, param_name, required_names = NULL, allow_null = TRUE) {
  if (is.null(value)) {
    if (allow_null) return(invisible(NULL))
    stop(paste0("'", param_name, "' cannot be NULL"), call. = FALSE)
  }
  
  if (!is.list(value)) {
    stop(paste0("'", param_name, "' must be a list"), call. = FALSE)
  }
  
  if (!is.null(required_names)) {
    missing_names <- setdiff(required_names, names(value))
    if (length(missing_names) > 0) {
      stop(paste0("'", param_name, "' is missing required elements: ", 
                  paste(missing_names, collapse = ", ")), call. = FALSE)
    }
  }
  
  invisible(NULL)
}

#' Standardized bid stage parameter validation
#'
#' @param previous_stage Previous stage object
#' @param current_stage Current stage name
#' @param additional_params List of additional parameters to validate
#'
#' @return NULL invisibly if valid, stops with error otherwise
#'
#' @keywords internal
#' @noRd
validate_bid_stage_params <- function(previous_stage, current_stage, additional_params = list()) {
  # validate previous stage
  validate_required_params(previous_stage = previous_stage)
  validate_previous_stage(previous_stage, current_stage)
  
  # validate additional parameters
  for (param_name in names(additional_params)) {
    param_config <- additional_params[[param_name]]
    param_value <- param_config$value
    
    if (param_config$type == "character") {
      validate_character_param(
        param_value, 
        param_name, 
        param_config$min_length %||% 1,
        param_config$allow_null %||% FALSE
      )
    } else if (param_config$type == "list") {
      validate_list_param(
        param_value,
        param_name,
        param_config$required_names %||% NULL,
        param_config$allow_null %||% TRUE
      )
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

#' Extract stage data safely from previous stage object
#'
#' @param previous_stage Previous stage object (bid_stage or tibble)
#' @param columns Character vector of column names to extract
#' @param default_values Named list of default values for each column
#'
#' @return Named list with extracted values or defaults
#'
#' @keywords internal
#' @noRd
extract_stage_data <- function(previous_stage, columns, default_values = list()) {
  result <- list()
  
  for (col in columns) {
    default_val <- if (col %in% names(default_values)) {
      default_values[[col]]
    } else {
      NA_character_
    }
    result[[col]] <- safe_column_access(previous_stage, col, default_val)
  }
  
  return(result)
}

#' Get stage metadata with defaults
#'
#' @param stage_number Current stage number (1-5)
#' @param custom_metadata Additional metadata to include
#'
#' @return List with standardized metadata
#'
#' @keywords internal
#' @noRd
get_stage_metadata <- function(stage_number, custom_metadata = list()) {
  base_metadata <- list(
    stage_number = stage_number,
    total_stages = 5,
    validation_status = "completed"
  )
  
  return(c(base_metadata, custom_metadata))
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

  # truncate to (max_length - 3), then append "..."
  if (max_length <= 3) {
    return(strrep(".", min(3, max_length)))
  }
  truncated <- substr(text_str, 1, max_length - 3)
  paste0(truncated, "...")
}

#' Generate contextual suggestions based on stage and content
#'
#' @param stage_name Current BID stage name
#' @param context_data Named list with stage-specific context
#' @param suggestion_rules Optional custom suggestion rules
#'
#' @return Character string with consolidated suggestions
#'
#' @keywords internal
#' @noRd
generate_stage_suggestions <- function(stage_name, context_data, suggestion_rules = NULL) {
  suggestions <- character(0)
  
  # use custom rules if provided, otherwise use defaults
  rules <- suggestion_rules %||% get_default_suggestion_rules()
  stage_rules <- rules[[stage_name]]
  
  if (is.null(stage_rules)) {
    return("Consider following BID framework best practices for this stage.")
  }
  
  # apply rules based on context
  # ensure stage_rules is a proper list structure
  if (!is.list(stage_rules)) {
    return("Consider following BID framework best practices for this stage.")
  }
  
  # separate default from rules
  default_suggestion <- stage_rules$default
  rule_list <- stage_rules[names(stage_rules) != "default"]
  
  # process each rule
  for (rule in rule_list) {
    # ensure rule is properly structured
    if (is.list(rule) && !is.null(rule$condition) && !is.null(rule$suggestion)) {
      if (evaluate_suggestion_condition(rule$condition, context_data)) {
        suggestions <- c(suggestions, rule$suggestion)
      }
    }
  }
  
  # add default suggestions if none match
  if (length(suggestions) == 0 && !is.null(default_suggestion)) {
    suggestions <- default_suggestion
  }
  
  return(paste(suggestions, collapse = " "))
}

#' Get default suggestion rules for all stages
#'
#' @return List of suggestion rules by stage
#'
#' @keywords internal
#' @noRd
get_default_suggestion_rules <- function() {
  list(
    Notice = list(
      list(
        condition = function(ctx) nchar(ctx$problem %||% "") < 10,
        suggestion = "Consider providing more detail in problem description."
      ),
      list(
        condition = function(ctx) nchar(ctx$evidence %||% "") < 10,
        suggestion = "Consider adding quantitative metrics to strengthen evidence."
      ),
      list(
        condition = function(ctx) is.null(ctx$target_audience) || is.na(ctx$target_audience),
        suggestion = "Define specific target audience to better focus design solutions."
      ),
      list(
        condition = function(ctx) grepl("too many|overwhelm|choice", tolower(ctx$problem %||% "")),
        suggestion = "Consider progressive disclosure or categorization to reduce choice complexity."
      ),
      default = "Problem clearly identified. Consider gathering additional quantitative evidence."
    ),
    Interpret = list(
      list(
        condition = function(ctx) (ctx$story_completeness %||% 0) < 0.5,
        suggestion = "Your data story is incomplete. Consider adding missing narrative elements."
      ),
      list(
        condition = function(ctx) (ctx$personas_count %||% 0) == 0,
        suggestion = "Consider defining specific user personas to better target your design."
      ),
      list(
        condition = function(ctx) nchar(ctx$central_question %||% "") > 100,
        suggestion = "Consider simplifying your central question for more focus."
      ),
      default = "Focus on making each story component compelling and relevant."
    )
  )
}

#' Evaluate suggestion condition against context data
#'
#' @param condition Function that evaluates context
#' @param context_data Named list with context
#'
#' @return Logical indicating if condition is met
#'
#' @keywords internal
#' @noRd
evaluate_suggestion_condition <- function(condition, context_data) {
  if (!is.function(condition)) {
    warning("Condition is not a function, skipping", call. = FALSE)
    return(FALSE)
  }
  
  if (!is.list(context_data) && !is.null(context_data)) {
    warning("Context data is not a list or NULL, attempting to coerce", call. = FALSE)
    context_data <- list(context_data)
  }
  
  tryCatch({
    result <- condition(context_data)
    if (!is.logical(result) || length(result) != 1) {
      warning("Condition function returned non-logical or multi-value result", call. = FALSE)
      return(FALSE)
    }
    return(result)
  }, error = function(e) {
    warning("Error evaluating suggestion condition: ", e$message, call. = FALSE)
    FALSE
  })
}
