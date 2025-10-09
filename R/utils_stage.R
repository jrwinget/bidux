# ==============================================================================
# STAGE UTILITIES
# ==============================================================================
#
# Functions for extracting and manipulating BID stage data.
#

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
