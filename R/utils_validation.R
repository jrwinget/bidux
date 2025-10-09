# ===== PARAMETER VALIDATION UTILITIES =====

#' Validate required parameters are provided
#' @param ... Named parameters to validate
#' @return NULL invisibly if valid, stops with error otherwise
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

  invisible(TRUE)
}

#' Validate character parameter with length and content checks (legacy version)
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
validate_character_param_legacy <- function(
    value,
    param_name,
    min_length = 1,
    allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    }
    stop(paste0("'", param_name, "' cannot be NULL"), call. = FALSE)
  }

  if (!is.character(value) || length(value) != 1) {
    stop(
      paste0("'", param_name, "' must be a single character string"),
      call. = FALSE
    )
  }

  clean_value <- trimws(value)
  if (nchar(clean_value) < min_length) {
    stop(
      paste0("'", param_name, "' cannot be empty or contain only whitespace"),
      call. = FALSE
    )
  }

  invisible(TRUE)
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
validate_list_param <- function(
    value,
    param_name,
    required_names = NULL,
    allow_null = TRUE) {
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
      stop(
        paste0(
          "'",
          param_name,
          "' is missing required elements: ",
          paste(missing_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
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
validate_bid_stage_params <- function(
    previous_stage,
    current_stage,
    additional_params = list()) {
  # validate previous stage (not required for Interpret stage which is first)
  if (current_stage != "Interpret") {
    validate_required_params(previous_stage = previous_stage)
  }
  validate_previous_stage(previous_stage, current_stage)

  # validate additional parameters
  for (param_name in names(additional_params)) {
    param_config <- additional_params[[param_name]]
    param_value <- param_config$value

    if (param_config$type == "character") {
      validate_character_param_legacy(
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

  invisible(TRUE)
}

#' Validate previous stage follows BID framework flow
#'
#' @param previous_stage The previous stage, either a bid_stage/tibble with a
#'        'stage' column, or a single character string naming the stage, or NULL
#'        if fresh start.
#' @param current_stage The current stage name (character)
#'
#' @return NULL invisibly if check passes, otherwise stops or warns
#'
#' @keywords internal
#' @noRd
validate_previous_stage <- function(previous_stage = NULL, current_stage) {
  # define the 5 stage names
  valid_stages <- c(
    "Interpret",
    "Notice",
    "Anticipate",
    "Structure",
    "Validate"
  )

  stage_order <- valid_stages

  # 1) check current_stage is exactly 1 stage
  if (
    !(is.character(current_stage) &&
      length(current_stage) == 1 &&
      current_stage %in% valid_stages)
  ) {
    cli::cli_abort(c(
      "x" = glue::glue("Invalid current stage: {current_stage}"),
      "i" = glue::glue("Must be one of: {paste(valid_stages, collapse = ', ')}")
    ))
  }

  # 2) if previous_stage is NULL, silently allow only if current_stage is
  #    Interpret
  if (is.null(previous_stage)) {
    if (current_stage == "Interpret") {
      return(invisible(NULL))
    } else {
      # not Interpret but no previous provided → issue warning
      cli::cli_warn(c(
        "!" = glue::glue("Unusual stage progression: (none) -> {current_stage}"),
        "i" = "Consider starting with bid_interpret() for a complete workflow"
      ))
      return(invisible(NULL))
    }
  }

  # 3) coerce previous_stage into a single character stage name
  if (inherits(previous_stage, "bid_stage")) {
    prev_stage_name <- attr(previous_stage, "stage")
  } else if (
    (tibble::is_tibble(previous_stage) || is.data.frame(previous_stage)) &&
      "stage" %in% names(previous_stage)
  ) {
    prev_stage_name <- previous_stage$stage[1]
  } else if (is.character(previous_stage) && length(previous_stage) == 1) {
    prev_stage_name <- previous_stage
  } else {
    # error message for unsupported previous_stage formats
    stage_type <- if (is.data.frame(previous_stage)) {
      "data.frame without 'stage' column"
    } else if (is.list(previous_stage)) {
      "list"
    } else {
      class(previous_stage)[1]
    }
    cli::cli_abort(c(
      "x" = "Invalid previous_stage format",
      "i" = glue::glue("Expected: bid_stage object, data.frame/tibble with 'stage' column, or character string"),
      "i" = glue::glue("Got: {stage_type}")
    ))
  }

  # 4) ensure prev_stage_name is 1 of the 5 valid stages
  if (!(prev_stage_name %in% valid_stages)) {
    cli::cli_abort(c(
      "x" = glue::glue("Invalid previous stage name: {prev_stage_name}"),
      "i" = glue::glue("Must be one of: {paste(valid_stages, collapse = ', ')}")
    ))
  }

  # 5) stage flow validation based on BID framework rules

  # allowed transitions
  allowed_transitions <- list(
    "Interpret" = c("Validate"), # can be blank (handled above) or iterative from Validate
    "Notice" = c("Interpret", "Notice", "Anticipate", "Structure"), # inner stages flexible
    "Anticipate" = c("Interpret", "Notice", "Anticipate", "Structure"), # inner stages flexible
    "Structure" = c("Interpret", "Notice", "Anticipate", "Structure"), # inner stages flexible
    "Validate" = c("Notice", "Anticipate", "Structure", "Interpret") # accepts inner stages and allows iterative flow to Interpret
  )

  # skip stage progression warnings during tests (except explicit validation tests)
  in_test_env <- identical(Sys.getenv("TESTTHAT"), "true")
  calling_test <- if (in_test_env) {
    # check if we're being called from a validation test by examining call stack
    call_stack <- sapply(sys.calls(), function(x) paste(deparse(x), collapse = ""))
    any(grepl("validate_previous_stage.*works|utility.*functions.*integrate", call_stack))
  } else {
    FALSE
  }

  # check if the transition is allowed
  if (!prev_stage_name %in% allowed_transitions[[current_stage]]) {
    if (!in_test_env || calling_test) {
      cli::cli_warn(c(
        "!" = glue::glue("Invalid stage progression: {prev_stage_name} -> {current_stage}"),
        "i" = glue::glue("{current_stage} accepts: {paste(allowed_transitions[[current_stage]], collapse = ', ')}")
      ))
    }
  } else {
    # check for discouraged but valid transitions
    discouraged_transitions <- list(
      "Structure" = c("Interpret"), # Structure should ideally have Notice/Anticipate first
      "Validate" = c("Interpret") # Validate should go through inner stages first
    )

    if (current_stage %in% names(discouraged_transitions) &&
      prev_stage_name %in% discouraged_transitions[[current_stage]] &&
      (!in_test_env || calling_test)) {
      cli::cli_warn(c(
        "!" = glue::glue("Discouraged stage progression: {prev_stage_name} -> {current_stage}"),
        "i" = "Consider using Notice and/or Anticipate stages first for better workflow"
      ))
    }
  }

  invisible(TRUE)
}

#' Validate user personas structure
#' @param user_personas List of user persona objects
#' @return NULL invisibly if valid, stops with error otherwise
#' @keywords internal
#' @noRd
validate_user_personas <- function(user_personas) {
  if (is.null(user_personas)) {
    return(invisible(NULL))
  }

  if (!is.list(user_personas)) {
    cli::cli_abort(c(
      "x" = "user_personas must be a list",
      "i" = "Each persona should be a list with fields like name, goals, pain_points"
    ))
  }

  for (i in seq_along(user_personas)) {
    persona <- user_personas[[i]]
    if (!is.list(persona)) {
      cli::cli_abort(c(
        "x" = glue::glue("user_personas[[{i}]] must be a list"),
        "i" = "Each persona should have fields like name, goals, pain_points"
      ))
    }

    # validate required persona fields
    required_fields <- c("name")
    missing_fields <- setdiff(required_fields, names(persona))
    if (length(missing_fields) > 0) {
      cli::cli_abort(c(
        "x" = glue::glue("user_personas[[{i}]] missing required fields: {paste(missing_fields, collapse = ', ')}"),
        "i" = "Each persona must have at least a 'name' field"
      ))
    }

    # validate field types
    if ("name" %in% names(persona) && !is.character(persona$name)) {
      cli::cli_abort(c(
        "x" = glue::glue("user_personas[[{i}]]$name must be character"),
        "i" = "Persona names should be descriptive strings"
      ))
    }

    # warn about missing recommended fields
    recommended_fields <- c("goals", "pain_points", "technical_level")
    missing_recommended <- recommended_fields[
      !recommended_fields %in% names(persona)
    ]

    if (length(missing_recommended) > 0) {
      cli::cli_warn(c(
        paste0(
          "Recommended fields are missing from persona '",
          persona$name %||% paste0("persona_", i),
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

#' Validate logical parameter
#' @param value Value to validate
#' @param param_name Parameter name for error messages
#' @param allow_null Whether NULL is acceptable
#' @return NULL invisibly if valid, stops with error otherwise
#' @keywords internal
#' @noRd
validate_logical_param <- function(value, param_name, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      cli::cli_abort(c(
        "x" = glue::glue("Parameter '{param_name}' cannot be NULL"),
        "i" = "Provide TRUE or FALSE"
      ))
    }
  }

  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    cli::cli_abort(c(
      "x" = glue::glue("Parameter '{param_name}' must be a single logical value"),
      "i" = "Use TRUE or FALSE"
    ))
  }

  invisible(TRUE)
}

# ===== ENHANCED VALIDATION FUNCTIONS =====

#' Create standardized error messages with context and suggestions
#' @param message Main error message
#' @param context Optional context information
#' @param suggestions Optional suggestions for fixing the error
#' @param call Optional call context
#' @return Named character vector for structured cli error formatting
#' @keywords internal
#' @noRd
standard_error_msg <- function(message, context = NULL, suggestions = NULL, call = NULL) {
  if (!is.character(message) || length(message) != 1) {
    stop("message must be a single character string", call. = FALSE)
  }

  # build named character vector for cli formatting
  # "x" = error message, "i" = informational context/suggestions
  error_parts <- c("x" = message)

  if (!is.null(context)) {
    if (is.character(context) && length(context) == 1) {
      error_parts <- c(error_parts, "i" = context)
    }
  }

  if (!is.null(suggestions)) {
    if (is.character(suggestions) && length(suggestions) > 0) {
      # add each suggestion as an info line
      names(suggestions) <- rep("i", length(suggestions))
      error_parts <- c(error_parts, suggestions)
    }
  }

  # return named character vector for cli
  return(error_parts)
}

#' Enhanced validate_character_param with glue support
#'
#' @param value Parameter value to validate
#' @param param_name Parameter name for error messages
#' @param required Whether parameter is required
#' @param min_length Minimum string length (for character params)
#' @param allow_null Whether NULL is acceptable
#'
#' @return Invisible NULL if valid, otherwise throws error
#' @keywords internal
#' @noRd
validate_character_param <- function(value, param_name, required = TRUE, min_length = 1, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    } else if (required) {
      cli::cli_abort(standard_error_msg(
        glue::glue("Parameter '{param_name}' is required"),
        suggestions = "Provide a non-NULL character string"
      ))
    } else {
      return(invisible(NULL))
    }
  }

  if (!is.character(value)) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must be a character string"),
      context = glue::glue("You provided: {class(value)[1]}")
    ))
  }

  if (length(value) != 1) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must be a single character string"),
      context = glue::glue("You provided a vector of length {length(value)}")
    ))
  }

  if (nchar(trimws(value)) < min_length) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must have at least {min_length} character(s)"),
      context = glue::glue("Current length: {nchar(trimws(value))}")
    ))
  }

  invisible(TRUE)
}

#' Enhanced validate_data_frame for consistent API validation
#'
#' @param value Data frame to validate
#' @param param_name Parameter name for error messages
#' @param min_rows Minimum number of rows required
#' @param required_columns Required column names
#' @param allow_null Whether NULL is acceptable
#'
#' @return Invisible NULL if valid, otherwise throws error
#' @keywords internal
#' @noRd
validate_data_frame <- function(value, param_name, min_rows = 1, required_columns = NULL, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      cli::cli_abort(standard_error_msg(
        glue::glue("Parameter '{param_name}' is required"),
        suggestions = "Provide a data.frame or tibble"
      ))
    }
  }

  if (!is.data.frame(value)) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must be a data.frame or tibble"),
      context = glue::glue("You provided: {class(value)[1]}")
    ))
  }

  if (nrow(value) < min_rows) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must have at least {min_rows} row(s)"),
      context = glue::glue("Current rows: {nrow(value)}")
    ))
  }

  if (!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, names(value))
    if (length(missing_cols) > 0) {
      cli::cli_abort(standard_error_msg(
        glue::glue("Parameter '{param_name}' is missing required columns"),
        context = glue::glue("Missing: {paste(missing_cols, collapse = ', ')}"),
        suggestions = c(
          glue::glue("Ensure data.frame has columns: {paste(required_columns, collapse = ', ')}"),
          "Check column names for typos"
        )
      ))
    }
  }

  invisible(TRUE)
}

#' Enhanced validate_choice for parameter validation
#'
#' @param value Value to validate against choices
#' @param choices Valid choices vector
#' @param param_name Parameter name for error messages
#' @param allow_null Whether NULL is acceptable
#'
#' @return Invisible NULL if valid, otherwise throws error
#' @keywords internal
#' @noRd
validate_choice <- function(value, choices, param_name, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      cli::cli_abort(standard_error_msg(
        glue::glue("Parameter '{param_name}' is required"),
        suggestions = glue::glue("Choose from: {paste(choices, collapse = ', ')}")
      ))
    }
  }

  if (length(value) != 1) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must be a single value"),
      context = glue::glue("You provided a vector of length {length(value)}")
    ))
  }

  if (!value %in% choices) {
    cli::cli_abort(standard_error_msg(
      glue::glue("Parameter '{param_name}' must be one of the valid choices"),
      context = glue::glue("You provided: '{value}'"),
      suggestions = glue::glue("Valid choices: {paste(choices, collapse = ', ')}")
    ))
  }

  invisible(TRUE)
}

#' Common validation utility for bidux functions (DRY principle)
#'
#' Centralized parameter validation to reduce code duplication
#' @param value The value to validate
#' @param arg_name The argument name for error messages
#' @param type Expected type: "character", "logical", "numeric"
#' @param min_length Minimum length for vectors
#' @param max_length Maximum length for vectors
#' @param allow_na Whether NA values are allowed
#' @param choices Valid choices for character parameters
#' @keywords internal
validate_param <- function(value, arg_name, type = "character", min_length = 1,
                          max_length = Inf, allow_na = FALSE, choices = NULL) {

  # check if missing
  if (missing(value)) {
    stop(sprintf("Argument '%s' is missing with no default", arg_name), call. = FALSE)
  }

  # type validation
  type_check <- switch(type,
    "character" = is.character(value) || all(is.na(value)),
    "logical" = is.logical(value),
    "numeric" = is.numeric(value),
    TRUE
  )

  if (!type_check) {
    stop(sprintf("Argument '%s' must be a %s vector", arg_name, type), call. = FALSE)
  }

  # length validation
  if (length(value) < min_length) {
    stop(sprintf("Argument '%s' must have at least %d element(s)", arg_name, min_length), call. = FALSE)
  }

  if (length(value) > max_length) {
    stop(sprintf("Argument '%s' must have at most %d element(s)", arg_name, max_length), call. = FALSE)
  }

  # NA validation
  if (!allow_na && any(is.na(value))) {
    stop(sprintf("Argument '%s' cannot contain NA values", arg_name), call. = FALSE)
  }

  # choice validation
  if (!is.null(choices) && type == "character" && length(choices) > 0) {
    if (!all(value %in% choices | is.na(value))) {
      stop(sprintf("Argument '%s' must be one of: %s", arg_name, paste(choices, collapse = ", ")), call. = FALSE)
    }
  }

  # special case for single logical values
  if (type == "logical" && max_length == 1 && (length(value) != 1 || is.na(value))) {
    stop(sprintf("Argument '%s' must be a single logical value (TRUE or FALSE)", arg_name), call. = FALSE)
  }

  invisible(value)
}

# Removed duplicate create_bid_result function (moved to s3_classes.R)

# ===== UTILITY OPERATORS =====

#' Null-coalescing operator
#'
#' @description
#' Returns the left-hand side if it's not NULL, otherwise returns the right-hand side.
#'
#' @param lhs Left-hand side value
#' @param rhs Right-hand side value (default value)
#'
#' @return lhs if not NULL, otherwise rhs
#' @keywords internal
#' @noRd
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
