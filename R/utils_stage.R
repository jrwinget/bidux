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
extract_stage_data <- function(
    previous_stage,
    columns,
    default_values = list()) {
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

#' Normalize previous stage to use canonical field names
#'
#' @param previous_stage Previous stage object
#'
#' @return Normalized tibble or NULL
#'
#' @keywords internal
#' @noRd
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
  if (
    "audience" %in%
      names(stage_data) &&
      (is.na(stage_data$audience[1]) || is.null(stage_data$audience[1]))
  ) {
    if (
      "previous_audience" %in%
        names(stage_data) &&
        !is.na(stage_data$previous_audience[1])
    ) {
      stage_data$audience[1] <- stage_data$previous_audience[1]
    }
  }

  return(tibble::as_tibble(stage_data))
}

#' Get audience from previous stage
#'
#' @param previous_stage Previous stage object
#'
#' @return Audience value or NA_character_
#'
#' @keywords internal
#' @noRd
get_audience_from_previous <- function(previous_stage) {
  # normalize first
  normalized_stage <- normalize_previous_stage(previous_stage)

  # defensive check - return early if normalization failed
  if (is.null(normalized_stage)) {
    return(NA_character_)
  }

  audience_fields <- c("audience", "target_audience", "previous_audience")
  for (field in audience_fields) {
    # ensure field variable is properly defined before use
    if (is.character(field) && nchar(field) > 0) {
      value <- safe_column_access(normalized_stage, field)
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(as.character(value))
      }
    }
  }
  return(NA_character_)
}

#' Get personas from previous stage
#'
#' @param previous_stage Previous stage object
#'
#' @return Personas value or NA_character_
#'
#' @keywords internal
#' @noRd
get_personas_from_previous <- function(previous_stage) {
  # normalize first
  normalized_stage <- normalize_previous_stage(previous_stage)

  # defensive check - return early if normalization failed
  if (is.null(normalized_stage)) {
    return(NA_character_)
  }

  persona_fields <- c("user_personas", "previous_personas", "personas")
  for (field in persona_fields) {
    # ensure field variable is properly defined before use
    if (is.character(field) && nchar(field) > 0) {
      value <- safe_column_access(normalized_stage, field)
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(as.character(value))
      }
    }
  }
  return(NA_character_)
}

#' Session-level migration notice for stage numbering change (0.3.1)
#'
#' @return NULL invisibly
#'
#' @keywords internal
#' @noRd
.show_stage_numbering_notice <- function() {
  # use a simple environment variable to track if notice was shown this session
  notice_var <- "BIDUX_STAGE_NUMBERING_NOTICE_SHOWN"

  if (is.null(getOption(notice_var))) {
    cli::cli_inform(c(
      "i" = "Stage numbering has been corrected in bidux 0.3.1:",
      " " = "Anticipate is now Stage 3, Structure is now Stage 4",
      " " = "This change improves logical workflow progression",
      " " = "All existing code remains backward compatible"
    ))

    # set option to prevent showing again this session
    options(structure(list(TRUE), names = notice_var))
  }

  invisible(NULL)
}

#' Get accessibility advice based on layout context
#'
#' @param layout_context Layout type
#'
#' @return Accessibility advice string
#'
#' @keywords internal
#' @noRd
get_accessibility_advice <- function(layout_context) {
  if (is.na(layout_context) || is.null(layout_context)) {
    layout_context <- "general"
  }

  switch(layout_context,
    "tabs" = "ensure keyboard navigation between tabs and screen reader announcements",
    "grid" = "provide proper row/column headers and cell relationships for screen readers",
    "card" = "ensure cards have descriptive labels and proper focus management",
    "dual_process" = "maintain accessibility across both summary and detail views",
    "breathable" = "use sufficient color contrast and focus indicators in spacious layouts",
    "provide clear focus indicators, sufficient color contrast, and screen reader support"
  )
}

#' Extract and consolidate previous stage information (DRY helper)
#'
#' @param previous_stage Previous stage object (normalized)
#' @param fields Character vector of fields to extract
#'
#' @return Named list with extracted information
#'
#' @keywords internal
#' @noRd
extract_previous_stage_fields <- function(previous_stage, fields) {
  if (is.null(previous_stage)) {
    return(stats::setNames(as.list(rep(NA_character_, length(fields))), fields))
  }

  stage_name <- safe_column_access(previous_stage, "stage", NA_character_)

  # map field names to possible locations based on stage
  field_mappings <- list(
    central_question = c("central_question", "previous_central_question"),
    hook = c("hook", "previous_hook"),
    problem = c("problem", "previous_problem"),
    theory = c("theory", "previous_theory"),
    audience = c("audience", "target_audience", "previous_audience"),
    personas = c("personas", "user_personas", "previous_personas"),
    bias_mitigations = c("bias_mitigations", "previous_bias"),
    layout = c("layout", "previous_layout"),
    concepts = c("concepts", "previous_concepts"),
    accessibility = c("accessibility", "previous_accessibility")
  )

  result <- list()
  for (field in fields) {
    possible_names <- field_mappings[[field]]
    if (is.null(possible_names)) {
      # use field name directly if not in mapping
      possible_names <- c(field, paste0("previous_", field))
    }

    # try each possible name in order
    value <- NA_character_
    for (name in possible_names) {
      temp_value <- safe_column_access(previous_stage, name, NA_character_)
      if (!is.na(temp_value) && nchar(trimws(as.character(temp_value))) > 0) {
        value <- temp_value
        break
      }
    }
    result[[field]] <- value
  }

  return(result)
}

#' Generate persona from audience string (DRY helper for bid_interpret)
#'
#' @param audience Audience string to analyze
#'
#' @return bid_user_personas object or NULL
#'
#' @keywords internal
#' @noRd
generate_persona_from_audience <- function(audience) {
  if (is.null(audience) || is.na(audience)) {
    return(NULL)
  }

  audience_lower <- tolower(audience)

  # determine user type
  user_type <- if (grepl("analyst|data scientist|technical|developer|engineer", audience_lower)) {
    "Data Analyst"
  } else if (grepl("executive|manager|director|leadership|ceo|cfo|cto|vp", audience_lower)) {
    "Executive"
  } else if (grepl("market|advertis|campaign|brand", audience_lower)) {
    "Marketing Professional"
  } else if (grepl("sales|account|business develop", audience_lower)) {
    "Sales Representative"
  } else if (grepl("customer|client|user|consumer", audience_lower)) {
    "End User"
  } else {
    "Dashboard User"
  }

  # determine technical level
  technical_level <- if (grepl("analyst|data scientist|technical|developer|engineer", audience_lower)) {
    "advanced"
  } else if (grepl("executive|leadership|ceo|cfo", audience_lower)) {
    "beginner"
  } else {
    "intermediate"
  }

  # determine goals
  goals <- if (grepl("executive|leadership", audience_lower)) {
    "Needs quick insights for strategic decisions"
  } else if (grepl("analyst|data", audience_lower)) {
    "Needs to explore data in depth to find patterns"
  } else if (grepl("market", audience_lower)) {
    "Needs to track campaign performance metrics"
  } else if (grepl("sales", audience_lower)) {
    "Needs to identify sales opportunities and track performance"
  } else {
    "Needs to extract relevant insights efficiently"
  }

  # determine pain points
  pain_points <- if (grepl("executive|leadership", audience_lower)) {
    "Limited time to analyze detailed reports"
  } else if (grepl("analyst|data", audience_lower)) {
    "Frustrated by interfaces that limit data exploration"
  } else if (grepl("market", audience_lower)) {
    "Struggles to connect multiple data sources for complete picture"
  } else if (grepl("sales", audience_lower)) {
    "Needs mobile-friendly dashboards for client meetings"
  } else {
    "Gets overwhelmed by complex dashboards with too many options"
  }

  # create persona using S3 class
  persona_df <- data.frame(
    name = paste(user_type, "Persona"),
    goals = goals,
    pain_points = pain_points,
    technical_level = technical_level,
    stringsAsFactors = FALSE
  )

  return(new_user_personas(persona_df))
}
