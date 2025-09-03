#' Suggest layout based on previous stage content using heuristics
#'
#' @description
#' Automatically suggests an appropriate layout type based on content analysis
#' of previous BID stages. Uses deterministic heuristics to match keywords in
#' problem descriptions, evidence, data story, and other contextual information
#' to layout types that best address the identified issues.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#'        function containing stage data with potential fields like problem,
#'        evidence, central_question, data_story, etc.
#' @return Character string indicating the suggested layout type
#'         ("breathable", "dual_process", "grid", "card", "tabs", or fallback)
#'
#' @details
#' The heuristics follow a priority order:
#' 1. **breathable** - if content suggests information overload, confusion, or
#'    cognitive load issues
#' 2. **dual_process** - if content mentions overview vs detail, quick vs deep,
#'    or two-mode interactions
#' 3. **grid** - if content focuses on grouping, clustering, visual hierarchy,
#'    or comparing related metrics
#' 4. **card** - if content mentions cards, chunks, tiles, modular blocks,
#'    or per-item summaries
#' 5. **tabs** - if content suggests sections, categories, progressive
#'    disclosure, but avoids tabs if telemetry shows tab drop-off
#' 6. **breathable** - fallback for any unmatched cases
#'
#' @keywords internal
suggest_layout_from_previous <- function(previous_stage) {
  # Extract and normalize text from various fields in previous_stage
  # Handle both flattened columns (from bid_interpret) and nested data_story
  txt <- paste(
    safe_lower(safe_column_access(previous_stage, "problem", "")),
    safe_lower(safe_column_access(previous_stage, "evidence", "")),
    safe_lower(safe_column_access(previous_stage, "central_question", "")),
    # Try flattened columns first (from bid_interpret output)
    safe_lower(safe_column_access(previous_stage, "hook", "")),
    safe_lower(safe_column_access(previous_stage, "context", "")),
    safe_lower(safe_column_access(previous_stage, "tension", "")),
    safe_lower(safe_column_access(previous_stage, "resolution", "")),
    safe_lower(safe_column_access(previous_stage, "audience", "")),
    # Fallback to nested data_story access (for other tibble structures)
    safe_lower(safe_stage_data_story_access(previous_stage, "hook")),
    safe_lower(safe_stage_data_story_access(previous_stage, "context")),
    safe_lower(safe_stage_data_story_access(previous_stage, "tension")),
    safe_lower(safe_stage_data_story_access(previous_stage, "resolution")),
    safe_lower(safe_stage_data_story_access(previous_stage, "audience")),
    collapse = " "
  )

  # Heuristic 1: Dual-process for overview vs detail patterns
  # (check first since it's more specific)
  if (
    grepl(
      "summary vs detail|overview and detail|quick vs deep|fast vs thorough|two modes|at a glance|quick access.*summaries|detailed breakdowns|two different modes|dual mode|quick access to summaries.*detailed breakdowns|summaries.*detailed breakdowns",
      txt
    )
  ) {
    return("dual_process")
  }

  # Heuristic 2: Breathable layout for overload/confusion patterns
  if (
    grepl(
      "\\boverload|overwhelmed|too many|confus|clutter|busy|noise|cognitive load|whitespace\\b",
      txt
    )
  ) {
    return("breathable")
  }

  # Heuristic 3: Grid layout for grouping/hierarchy patterns
  if (
    grepl(
      "\\bgroup|cluster|related metrics|compare panels|visual hierarchy|proximity\\b",
      txt
    )
  ) {
    return("grid")
  }

  # Heuristic 4: Card layout for modular/chunked patterns
  if (
    grepl(
      "\\bcards|chunks|tiles|modular blocks|per-item summary|entity cards\\b",
      txt
    )
  ) {
    return("card")
  }

  # Heuristic 5: Tabs layout for sections/categories, but check telemetry
  if (
    grepl(
      "\\bsections|categories|module separation|progressive disclosure|stepwise\\b",
      txt
    )
  ) {
    # Check if telemetry indicates tab navigation issues
    telemetry_data <- safe_column_access(previous_stage, "telemetry", NULL)
    if (!is.null(telemetry_data) && isTRUE(telemetry_data$nav_dropoff_tabs)) {
      return("grid") # Fallback to grid if tabs have telemetry issues
    }
    return("tabs")
  }

  # Heuristic 6: Default fallback
  "breathable"
}

#' Generate layout selection rationale
#'
#' @description
#' Provides a concise explanation for why a particular layout was chosen
#' based on the content analysis of the previous stage.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#' @param chosen Character string with the chosen layout type
#' @return Character string with explanation for the layout choice
#'
#' @keywords internal
layout_rationale <- function(previous_stage, chosen) {
  # Extract text for pattern matching
  # Handle both flattened columns (from bid_interpret) and nested data_story
  txt <- paste(
    safe_lower(safe_column_access(previous_stage, "problem", "")),
    safe_lower(safe_column_access(previous_stage, "evidence", "")),
    safe_lower(safe_column_access(previous_stage, "central_question", "")),
    # Try flattened columns first (from bid_interpret output)
    safe_lower(safe_column_access(previous_stage, "hook", "")),
    safe_lower(safe_column_access(previous_stage, "context", "")),
    safe_lower(safe_column_access(previous_stage, "tension", "")),
    safe_lower(safe_column_access(previous_stage, "resolution", "")),
    safe_lower(safe_column_access(previous_stage, "audience", "")),
    # Fallback to nested data_story access (for other tibble structures)
    safe_lower(safe_stage_data_story_access(previous_stage, "hook")),
    safe_lower(safe_stage_data_story_access(previous_stage, "context")),
    safe_lower(safe_stage_data_story_access(previous_stage, "tension")),
    safe_lower(safe_stage_data_story_access(previous_stage, "resolution")),
    safe_lower(safe_stage_data_story_access(previous_stage, "audience")),
    collapse = " "
  )

  # Generate specific rationale based on detected patterns
  rationale <- switch(
    chosen,
    "dual_process" = {
      "Detected overview vs detail patterns; choosing 'dual_process' for quick insights and detailed analysis."
    },
    "breathable" = {
      if (
        grepl(
          "\\boverload|overwhelmed|too many|confus|clutter|busy|noise|cognitive load\\b",
          txt
        )
      ) {
        "Detected information overload patterns; choosing 'breathable' to reduce cognitive load."
      } else {
        "Selected 'breathable' as safe default to ensure clean, uncluttered layout."
      }
    },
    "grid" = {
      if (
        grepl(
          "\\bsections|categories|module separation|progressive disclosure|stepwise\\b",
          txt
        ) &&
          !is.null(safe_column_access(previous_stage, "telemetry", NULL)) &&
          isTRUE(
            safe_column_access(
              previous_stage,
              "telemetry",
              NULL
            )$nav_dropoff_tabs
          )
      ) {
        "Detected section-based content but telemetry shows tab navigation issues; choosing 'grid' instead."
      } else {
        "Detected grouping and comparison needs; choosing 'grid' for related content organization."
      }
    },
    "card" = {
      "Detected modular content patterns; choosing 'card' for distinct content containers."
    },
    "tabs" = {
      "Detected categorical content structure; choosing 'tabs' for progressive disclosure."
    },
    sprintf("Selected '%s' based on detected content and user context.", chosen)
  )

  return(rationale)
}

#' Safely convert text to lowercase with null handling
#'
#' @description
#' Helper function that safely converts text to lowercase while handling
#' NULL, NA, and non-character values gracefully.
#'
#' @param x Input value to convert to lowercase string
#' @return Character string in lowercase, or empty string if input is NULL/NA
#'
#' @keywords internal
safe_lower <- function(x) {
  x <- if (is.null(x)) "" else as.character(x)
  tolower(trimws(paste(x, collapse = " ")))
}

#' Safe access to data_story elements from previous stage
#' @param previous_stage Previous stage data
#' @param element Name of data_story element to access
#' @return Character string or empty string if not found
#' @keywords internal
safe_stage_data_story_access <- function(previous_stage, element) {
  data_story <- safe_column_access(previous_stage, "data_story", NULL)
  if (is.null(data_story)) {
    return("")
  }

  # Handle tibble list column case - data_story is a list with unnamed elements
  if (is.list(data_story) && length(data_story) > 0) {
    # If names are present, use direct access
    if (element %in% names(data_story)) {
      value <- data_story[[element]]
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(as.character(value))
      }
    } else if (
      is.list(data_story[[1]]) && element %in% names(data_story[[1]])
    ) {
      # If no names (typical tibble list column), access first element
      value <- data_story[[1]][[element]]
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(as.character(value))
      }
    }
  }
  return("")
}
