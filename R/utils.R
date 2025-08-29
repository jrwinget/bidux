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
    if (allow_null) {
      return(invisible(NULL))
    }
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
    if (allow_null) {
      return(invisible(NULL))
    }
    stop(paste0("'", param_name, "' cannot be NULL"), call. = FALSE)
  }

  if (!is.list(value)) {
    stop(paste0("'", param_name, "' must be a list"), call. = FALSE)
  }

  if (!is.null(required_names)) {
    missing_names <- setdiff(required_names, names(value))
    if (length(missing_names) > 0) {
      stop(paste0(
        "'", param_name, "' is missing required elements: ",
        paste(missing_names, collapse = ", ")
      ), call. = FALSE)
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

  tryCatch(
    {
      result <- condition(context_data)
      if (!is.logical(result) || length(result) != 1) {
        warning("Condition function returned non-logical or multi-value result", call. = FALSE)
        return(FALSE)
      }
      return(result)
    },
    error = function(e) {
      warning("Error evaluating suggestion condition: ", e$message, call. = FALSE)
      FALSE
    }
  )
}

# Safe access to data story elements
safe_data_story_access <- function(data_story, element) {
  if (!is.null(data_story) && element %in% names(data_story)) {
    value <- data_story[[element]]
    if (
      !is.null(value) &&
        !is.na(value) &&
        nchar(trimws(as.character(value))) > 0
    ) {
      return(as.character(value))
    }
  }
  return(NA_character_)
}

# Generic helper to validate user personas structure
validate_user_personas <- function(user_personas) {
  if (!is.list(user_personas)) {
    cli::cli_abort(c(
      "The user_personas parameter must be a list",
      "i" = "You provided {.cls {class(user_personas)}}"
    ))
  }

  for (i in seq_along(user_personas)) {
    persona <- user_personas[[i]]

    if (!is.list(persona)) {
      cli::cli_abort(c(
        "Each persona in user_personas must be a list",
        "x" = paste0("Persona at position ", i, " is ", class(persona)[1])
      ))
    }

    if (!"name" %in% names(persona)) {
      cli::cli_abort(c(
        "Each persona must have at least a 'name' field",
        "x" = paste0(
          "Persona at position ",
          i,
          " is missing the required 'name' field"
        )
      ))
    }

    recommended_fields <- c("goals", "pain_points", "technical_level")
    missing_recommended <- recommended_fields[
      !recommended_fields %in% names(persona)
    ]

    if (length(missing_recommended) > 0) {
      cli::cli_warn(c(
        paste0(
          "Recommended fields are missing from persona '",
          persona$name,
          "'"
        ),
        "i" = paste0(
          "Consider adding: ",
          paste(missing_recommended, collapse = ", ")
        )
      ))
    }
  }

  return(TRUE)
}

# Generic fuzzy matching function for concepts
find_best_concept_match <- function(concept, d_all_concepts) {
  if (is.na(concept) || nchar(trimws(concept)) == 0) {
    return(NULL)
  }

  normalized_concept <- tolower(gsub("[_-]", " ", trimws(concept)))
  normalized_concept <- gsub("\\s+", " ", normalized_concept)

  # exact match
  exact_matches <- d_all_concepts[
    tolower(d_all_concepts$concept) == normalized_concept,
  ]
  if (nrow(exact_matches) > 0) {
    return(exact_matches$concept[1])
  }

  # specific test cases, handle common variations
  if (grepl("vizual", normalized_concept)) {
    normalized_concept <- gsub("vizual", "visual", normalized_concept)
  }
  if (grepl("principal", normalized_concept)) {
    normalized_concept <- gsub("principal", "principle", normalized_concept)
  }

  # exact match again after corrections
  exact_matches <- d_all_concepts[
    tolower(d_all_concepts$concept) == normalized_concept,
  ]
  if (nrow(exact_matches) > 0) {
    return(exact_matches$concept[1])
  }

  # contains match (both ways)
  contains_matches <- d_all_concepts[
    grepl(normalized_concept, tolower(d_all_concepts$concept)),
  ]
  if (nrow(contains_matches) > 0) {
    return(contains_matches$concept[1])
  }

  # reverse contains for partial matches
  reverse_contains <- d_all_concepts[
    grepl(
      paste(strsplit(normalized_concept, " ")[[1]], collapse = ".*"),
      tolower(d_all_concepts$concept)
    ),
  ]
  if (nrow(reverse_contains) > 0) {
    return(reverse_contains$concept[1])
  }

  # word matching with priority for key words
  words <- strsplit(normalized_concept, "\\s+")[[1]]
  if (length(words) > 0) {
    key_words <- c("visual", "hierarchy", "proximity", "cognitive", "load")
    priority_words <- words[words %in% key_words]
    other_words <- words[!words %in% key_words]
    ordered_words <- c(priority_words, other_words)

    for (word in ordered_words) {
      if (nchar(word) >= 3) {
        word_pattern <- paste0("\\b", word, "\\b")
        word_matches <- d_all_concepts[
          grepl(word_pattern, tolower(d_all_concepts$concept)),
        ]
        if (nrow(word_matches) > 0) {
          return(word_matches$concept[1])
        }
      }
    }
  }

  # fuzzy matching
  if (requireNamespace("stringdist", quietly = TRUE)) {
    distances <- stringdist::stringdistmatrix(
      normalized_concept,
      tolower(d_all_concepts$concept),
      method = "jw"
    )

    best_match_idx <- which.min(distances)
    best_score <- 1 - distances[best_match_idx]

    if (best_score > 0.6) {
      matched_concept <- d_all_concepts$concept[best_match_idx]
      cli::cli_alert_info(paste0(
        "Fuzzy matched '",
        concept,
        "' to '",
        matched_concept,
        "' (similarity: ",
        round(best_score * 100),
        "%)"
      ))
      return(matched_concept)
    }
  }

  return(NULL)
}

# Generic text analysis for concept detection
detect_concepts_from_text <- function(text, source_type = "general") {
  if (is.na(text) || nchar(trimws(text)) == 0) {
    return(character(0))
  }

  text_lower <- tolower(trimws(text))
  detected_concepts <- character(0)

  concept_keywords <- list(
    "Visual Hierarchy" = c(
      "focus",
      "attention",
      "important",
      "priority",
      "hierarchy",
      "prominence"
    ),
    "Principle of Proximity" = c(
      "group",
      "related",
      "together",
      "proximity",
      "association",
      "arrange"
    ),
    "Dual-Processing Theory" = c(
      "overview",
      "detail",
      "quick",
      "depth",
      "glance",
      "dig"
    ),
    "Breathable Layouts" = c(
      "space",
      "clean",
      "clear",
      "simple",
      "uncluttered",
      "whitespace"
    ),
    "Progressive Disclosure" = c(
      "gradually",
      "reveal",
      "step",
      "complexity",
      "details",
      "level"
    ),
    "Default Effect" = c(
      "default",
      "preset",
      "initial",
      "automatic",
      "standard",
      "starting"
    ),
    "Information Hierarchy" = c(
      "organize",
      "structure",
      "arrange",
      "categorize",
      "classify"
    )
  )

  for (concept_name in names(concept_keywords)) {
    keywords <- concept_keywords[[concept_name]]
    if (any(sapply(keywords, function(k) grepl(k, text_lower)))) {
      detected_concepts <- c(detected_concepts, concept_name)
    }
  }

  if (length(detected_concepts) > 0) {
    cli::cli_alert_info(paste0(
      "Detected ",
      length(detected_concepts),
      " concepts from ",
      source_type,
      " description: ",
      paste(detected_concepts, collapse = ", ")
    ))
  }

  return(unique(detected_concepts))
}

# Generic formatting function for accessibility storage
format_accessibility_for_storage <- function(accessibility) {
  if (!is.null(accessibility)) {
    if (is.list(accessibility)) {
      jsonlite::toJSON(accessibility, auto_unbox = TRUE)
    } else {
      as.character(accessibility)
    }
  } else {
    NA_character_
  }
}

# normalize previous stage to use canonical field names
normalize_previous_stage <- function(previous_stage) {
  if (is.null(previous_stage)) {
    return(NULL)
  }
  
  # convert to tibble if needed
  if (inherits(previous_stage, "bid_stage")) {
    stage_data <- as.data.frame(previous_stage)
  } else if (is.data.frame(previous_stage)) {
    stage_data <- previous_stage
  } else {
    return(previous_stage) # return as-is if not recognizable
  }
  
  # rename legacy field names to canonical ones
  if ("previous_question" %in% names(stage_data)) {
    stage_data$previous_central_question <- stage_data$previous_question
    stage_data$previous_question <- NULL
  }
  
  if ("previous_story_hook" %in% names(stage_data)) {
    stage_data$previous_hook <- stage_data$previous_story_hook
    stage_data$previous_story_hook <- NULL
  }
  
  # coalesce audience fields if needed
  if ("audience" %in% names(stage_data) && 
      (is.na(stage_data$audience[1]) || is.null(stage_data$audience[1]))) {
    if ("previous_audience" %in% names(stage_data) && 
        !is.na(stage_data$previous_audience[1])) {
      stage_data$audience[1] <- stage_data$previous_audience[1]
    }
  }
  
  return(tibble::as_tibble(stage_data))
}

# generic helper to get audience from previous stage
get_audience_from_previous <- function(previous_stage) {
  # normalize first
  normalized_stage <- normalize_previous_stage(previous_stage)
  
  audience_fields <- c("audience", "target_audience", "previous_audience")
  for (field in audience_fields) {
    value <- safe_column_access(normalized_stage, field)
    if (!is.na(value) && nchar(trimws(value)) > 0) {
      return(value)
    }
  }
  return(NA_character_)
}

# generic helper to get personas from previous stage
get_personas_from_previous <- function(previous_stage) {
  # normalize first
  normalized_stage <- normalize_previous_stage(previous_stage)
  
  persona_fields <- c("user_personas", "previous_personas", "personas")
  for (field in persona_fields) {
    value <- safe_column_access(normalized_stage, field)
    if (!is.na(value) && nchar(trimws(value)) > 0) {
      return(value)
    }
  }
  return(NA_character_)
}

# time wrapper for test stubbing
.now <- function() {
  Sys.time()
}

# generic next steps formatting
format_next_steps <- function(next_steps) {
  if (is.null(next_steps)) {
    return(NA_character_)
  }

  if (is.character(next_steps)) {
    if (length(next_steps) == 1) {
      if (grepl(";", next_steps)) {
        return(next_steps)
      } else {
        return(next_steps)
      }
    } else {
      return(paste(next_steps, collapse = "; "))
    }
  }

  next_steps_char <- as.character(next_steps)
  return(paste(next_steps_char, collapse = "; "))
}

# Generic next steps parsing
parse_next_steps <- function(next_steps_formatted) {
  if (is.na(next_steps_formatted) || is.null(next_steps_formatted)) {
    return(character(0))
  }

  if (grepl(";", next_steps_formatted)) {
    return(trimws(unlist(strsplit(next_steps_formatted, ";"))))
  } else {
    return(next_steps_formatted)
  }
}
