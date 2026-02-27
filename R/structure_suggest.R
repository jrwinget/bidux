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

# ==============================================================================
# concept-grouped structure suggestions
# ==============================================================================

#' Generate ranked, concept-grouped, actionable UI/UX suggestions
#'
#' @description
#' Creates structured suggestions organized by UX concepts with specific
#' component recommendations and rationales. Suggestions are ranked by
#' relevance and grouped by concept for systematic implementation.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#' @param concepts Optional character vector of additional concepts to include
#' @return List of concept groups with ranked suggestions
#'
#' @details
#' The function combines concepts from multiple sources:
#' - Stage 1 theory (from Notice)
#' - Stage 2 inferred concepts (from keywords in story)
#' - Optional user-provided concepts
#'
#' Each suggestion includes:
#' - title: Brief actionable description
#' - details: Specific implementation guidance
#' - components: R dashboard component recommendations (shiny::, bslib::, etc.)
#' - rationale: 1-2 sentence explanation
#' - score: Relevance ranking (0-1)
#'
#' **Framework Compatibility:**
#' Components prefixed with 'shiny::' require Shiny runtime (either a Shiny app
#' or Quarto dashboard with `server: shiny`). Components from bslib, DT, plotly,
#' reactable, and leaflet work in both Shiny apps and static/OJS Quarto
#' dashboards.
#'
#' @importFrom cli cli_alert_info
#' @keywords internal
structure_suggestions <- function(
    previous_stage,
    concepts = NULL,
    quiet = NULL) {
  # combine concepts from multiple sources
  concepts_final <- unique(c(
    extract_stage1_theory(previous_stage), # from Notice
    infer_concepts_from_story(previous_stage), # from Interpret (keywords)
    concepts # user-provided
  ))

  # remove empty/NA concepts
  concepts_final <- concepts_final[
    !is.na(concepts_final) & nchar(trimws(concepts_final)) > 0
  ]

  # build suggestion groups with rankings
  groups <- build_groups_with_suggestions(
    concepts_final,
    previous_stage
  )
  groups <- rank_and_sort_suggestions(groups, previous_stage)

  bid_alert_info(
    'Tip: Learn more about any concept via bid_concept("<concept>").',
    quiet = quiet
  )

  return(groups)
}

#' Extract theory concepts from Stage 1 (Notice)
#'
#' @param previous_stage Previous stage data
#' @return Character vector of theory-based concepts
#' @keywords internal
extract_stage1_theory <- function(previous_stage) {
  theory <- safe_column_access(previous_stage, "theory")
  if (!is.na(theory) && nchar(trimws(theory)) > 0) {
    return(theory)
  }
  return(character(0))
}

#' Infer concepts from Stage 2 (Interpret) story elements
#'
#' @param previous_stage Previous stage data
#' @return Character vector of story-inferred concepts
#' @keywords internal
infer_concepts_from_story <- function(previous_stage) {
  # extract story text
  story_text <- paste(
    safe_column_access(previous_stage, "central_question", ""),
    safe_column_access(previous_stage, "hook", ""),
    safe_column_access(previous_stage, "context", ""),
    safe_column_access(previous_stage, "tension", ""),
    safe_column_access(previous_stage, "resolution", ""),
    safe_column_access(previous_stage, "audience", ""),
    collapse = " "
  )

  detected_concepts <- character(0)
  story_lower <- tolower(story_text)

  # keyword-based concept detection — delegates to canonical map
  concept_keywords <- get_concept_keywords()

  for (concept_name in names(concept_keywords)) {
    keywords <- concept_keywords[[concept_name]]
    if (any(vapply(keywords, function(k) grepl(k, story_lower), logical(1)))) {
      detected_concepts <- c(detected_concepts, concept_name)
    }
  }

  # add audience-based concepts
  audience_text <- tolower(safe_column_access(previous_stage, "audience", ""))
  data_story <- safe_column_access(previous_stage, "data_story", NULL)
  if (
    !is.null(data_story) &&
      is.list(data_story) &&
      "audience" %in% names(data_story)
  ) {
    audience_text <- paste(
      audience_text,
      tolower(as.character(data_story$audience))
    )
  }
  problem_text <- tolower(safe_column_access(previous_stage, "problem", ""))

  combined_audience <- paste(audience_text, problem_text)
  if (
    grepl(
      "first.time|first-time|new.*user|beginner|novice|confused easily",
      combined_audience
    )
  ) {
    detected_concepts <- c(detected_concepts, "User Onboarding")
  }

  return(unique(detected_concepts))
}

#' Build suggestion groups organized by concept
#'
#' @param concepts_final Final list of concepts to generate suggestions for
#' @param previous_stage Previous stage data for context
#' @return List of concept groups with suggestions
#' @keywords internal
build_groups_with_suggestions <- function(
    concepts_final,
    previous_stage) {
  # ensure at least some core concepts if none provided
  if (length(concepts_final) == 0) {
    concepts_final <- c("Cognitive Load Theory", "Visual Hierarchy")
  }

  # always include essential concepts for any structure stage
  essential_concepts <- c(
    "Cognitive Load Theory",
    "Progressive Disclosure",
    "Visual Hierarchy"
  )
  concepts_final <- unique(c(concepts_final, essential_concepts))

  groups <- list()

  for (concept in concepts_final) {
    group <- build_concept_group(concept, previous_stage)
    if (!is.null(group) && length(group$suggestions) > 0) {
      groups <- append(groups, list(group))
    }
  }

  return(groups)
}

#' Build suggestions for a specific concept
#'
#' @param concept Name of the concept to generate suggestions for
#' @param previous_stage Previous stage data
#' @return List with concept name and suggestions
#' @keywords internal
build_concept_group <- function(concept, previous_stage) {
  suggestions <- switch(concept,
    "Cognitive Load Theory" = get_cognitive_load_suggestions(previous_stage),
    "Progressive Disclosure" = get_progressive_disclosure_suggestions(previous_stage),
    "Visual Hierarchy" = get_visual_hierarchy_suggestions(previous_stage),
    "Dual-Processing Theory" = get_dual_processing_suggestions(previous_stage),
    "User Onboarding" = get_onboarding_suggestions(previous_stage),
    "Information Scent" = get_information_scent_suggestions(previous_stage),
    "Principle of Proximity" = get_proximity_suggestions(previous_stage),
    get_generic_suggestions(concept, previous_stage)
  )

  if (length(suggestions) == 0) {
    return(NULL)
  }

  return(list(
    concept = concept,
    suggestions = suggestions
  ))
}

# concept-specific suggestion factories

#' Generate Cognitive Load Theory suggestions
#' @keywords internal
get_cognitive_load_suggestions <- function(previous_stage) {
  base_suggestions <- list(
    list(
      title = "Limit initial choices",
      details = "Show only core filters by default; defer advanced options to secondary views or accordions.",
      components = c(
        "bslib::accordion",
        "shiny::conditionalPanel",
        "shiny::updateSelectizeInput"
      ),
      rationale = "Reduces initial cognitive load for new users while preserving functionality.",
      score = 0.92
    ),
    list(
      title = "Use progressive complexity",
      details = "Start with simple views and allow users to add complexity incrementally.",
      components = c(
        "shiny::tabsetPanel",
        "bslib::accordion",
        "shiny::actionButton"
      ),
      rationale = "Prevents overwhelming users with too many options at once.",
      score = 0.88
    ),
    list(
      title = "Provide smart defaults",
      details = "Pre-select commonly used filters and settings to reduce decision fatigue.",
      components = c(
        "shiny::selectInput",
        "shiny::checkboxInput",
        "bslib::input_switch"
      ),
      rationale = "Leverages the Default Effect to reduce cognitive burden.",
      score = 0.85
    )
  )

  return(base_suggestions)
}

#' Generate Progressive Disclosure suggestions
#' @keywords internal
get_progressive_disclosure_suggestions <- function(previous_stage) {
  base_suggestions <- list(
    list(
      title = "Use collapsible advanced filters",
      details = "Place seldom-used filters in accordion sections or 'Show more' toggles.",
      components = c("bslib::accordion", "shiny::conditionalPanel"),
      rationale = "Reveals complexity on demand without overwhelming the interface.",
      score = 0.88
    ),
    list(
      title = "Implement drill-down navigation",
      details = "Allow users to start with summaries and progressively reveal details.",
      components = c(
        "shiny::actionButton",
        "DT::datatable",
        "reactable::reactable"
      ),
      rationale = "Matches user mental models of exploration from general to specific.",
      score = 0.84
    )
  )

  # check telemetry for tab issues
  telemetry_data <- safe_column_access(previous_stage, "telemetry", NULL)
  if (!is.null(telemetry_data) && isTRUE(telemetry_data$nav_dropoff_tabs)) {
    # demote tab-related suggestions
    for (i in seq_along(base_suggestions)) {
      if (grepl("tab", base_suggestions[[i]]$details, ignore.case = TRUE)) {
        base_suggestions[[i]]$score <- base_suggestions[[i]]$score - 0.1
      }
    }
  }

  return(base_suggestions)
}

#' Generate Visual Hierarchy suggestions
#' @keywords internal
get_visual_hierarchy_suggestions <- function(previous_stage) {
  list(
    list(
      title = "Establish clear information priority",
      details = "Use size, color, and spacing to guide attention to key metrics first.",
      components = c(
        "bslib::card",
        "shiny::h1",
        "shiny::h2",
        "bslib::value_box"
      ),
      rationale = "Helps users quickly identify what matters most in the interface.",
      score = 0.90
    ),
    list(
      title = "Group related content visually",
      details = "Use consistent spacing and visual containers to show relationships.",
      components = c("bslib::layout_columns", "bslib::card", "shiny::fluidRow"),
      rationale = "Leverages Gestalt principles to reduce cognitive processing.",
      score = 0.87
    )
  )
}

#' Generate Dual-Processing Theory suggestions
#' @keywords internal
get_dual_processing_suggestions <- function(previous_stage) {
  base_suggestions <- list(
    list(
      title = "Provide summary and detail views",
      details = "Show key insights prominently with option to drill into detailed data.",
      components = c("bslib::value_box", "DT::datatable", "shiny::tabsetPanel"),
      rationale = "Serves both System 1 (quick) and System 2 (deliberate) thinking modes.",
      score = 0.89
    )
  )

  return(base_suggestions)
}

#' Generate User Onboarding suggestions
#' @keywords internal
get_onboarding_suggestions <- function(previous_stage) {
  list(
    list(
      title = "Add contextual guidance",
      details = "Provide help text and examples for first-time users without cluttering the interface.",
      components = c("shiny::helpText", "bslib::popover", "shiny::modalDialog"),
      rationale = "Reduces learning curve while allowing experienced users to work efficiently.",
      score = 0.83
    ),
    list(
      title = "Use progressive onboarding",
      details = "Introduce features gradually rather than overwhelming with tutorials upfront.",
      components = c(
        "shiny::conditionalPanel",
        "shiny::observeEvent",
        "bslib::tooltip"
      ),
      rationale = "Matches natural learning patterns and reduces abandonment.",
      score = 0.81
    )
  )
}

#' Generate Information Scent suggestions
#' @keywords internal
get_information_scent_suggestions <- function(previous_stage) {
  list(
    list(
      title = "Use descriptive labels and headers",
      details = "Make navigation and section names clearly indicate what users will find.",
      components = c("shiny::h3", "bslib::card_header", "shiny::navbarPage"),
      rationale = "Strong information scent helps users navigate efficiently to their goals.",
      score = 0.86
    )
  )
}

#' Generate Principle of Proximity suggestions
#' @keywords internal
get_proximity_suggestions <- function(previous_stage) {
  base_suggestions <- list(
    list(
      title = "Group related controls together",
      details = "Place filters and controls near the content they affect.",
      components = c(
        "bslib::layout_columns",
        "shiny::wellPanel",
        "bslib::card"
      ),
      rationale = "Spatial proximity indicates functional relationship.",
      score = 0.85
    )
  )

  return(base_suggestions)
}

#' Generate generic suggestions for unrecognized concepts
#' @keywords internal
get_generic_suggestions <- function(concept, previous_stage) {
  list(
    list(
      title = paste("Apply", concept, "principles"),
      details = paste(
        "Consider how",
        concept,
        "applies to your dashboard design."
      ),
      components = c("bslib::card", "shiny::fluidRow", "bslib::layout_columns"),
      rationale = paste(
        "Systematic application of",
        concept,
        "can improve user experience."
      ),
      score = 0.70
    )
  )
}

#' Rank and sort suggestions within each group
#'
#' @param groups List of concept groups with suggestions
#' @param previous_stage Previous stage data for scoring adjustments
#' @return List of groups with ranked suggestions
#' @keywords internal
rank_and_sort_suggestions <- function(groups, previous_stage) {
  for (i in seq_along(groups)) {
    # apply context-based scoring adjustments
    for (j in seq_along(groups[[i]]$suggestions)) {
      groups[[i]]$suggestions[[j]] <- adjust_suggestion_score(
        groups[[i]]$suggestions[[j]],
        previous_stage,
        groups[[i]]$concept
      )
    }

    # sort suggestions by score (descending)
    scores <- vapply(groups[[i]]$suggestions, function(s) s$score, numeric(1))
    order_idx <- order(scores, decreasing = TRUE)
    groups[[i]]$suggestions <- groups[[i]]$suggestions[order_idx]
  }

  # sort groups by highest suggestion score in each group
  group_max_scores <- vapply(groups, function(g) {
    max(vapply(g$suggestions, function(s) s$score, numeric(1)))
  }, numeric(1))
  group_order <- order(group_max_scores, decreasing = TRUE)

  return(groups[group_order])
}

#' Apply context-based scoring adjustments
#' @keywords internal
adjust_suggestion_score <- function(
    suggestion,
    previous_stage,
    concept) {
  score <- suggestion$score

  # boost if concept originated from Stage 1 theory
  stage1_theory <- extract_stage1_theory(previous_stage)
  if (concept %in% stage1_theory) {
    score <- score + 0.05
  }

  # demote tabs-related suggestions if telemetry shows issues
  telemetry_data <- safe_column_access(previous_stage, "telemetry", NULL)
  if (
    !is.null(telemetry_data) &&
      isTRUE(telemetry_data$nav_dropoff_tabs) &&
      grepl("tab", suggestion$details, ignore.case = TRUE)
  ) {
    score <- score - 0.08
  }

  # ensure score stays within bounds
  suggestion$score <- max(0, min(1, score))
  return(suggestion)
}

# ==============================================================================
# tibble conversion utilities
# ==============================================================================

#' Assign difficulty rating based on components and suggestion complexity
#'
#' @description
#' Analyzes suggestion details and component count to assign a difficulty rating.
#' Considers complexity patterns in the suggestion text and number of components.
#'
#' @param suggestion Suggestion object with title, details, and components
#' @return Character string: "Easy", "Medium", or "Hard"
#' @keywords internal
assign_difficulty <- function(suggestion) {
  # count number of components
  num_components <- length(suggestion$components)

  # check for complexity indicators in details
  details_lower <- tolower(suggestion$details)

  # hard indicators
  is_hard <- grepl("complex|multiple.*step|advanced|implement.*system|integrate", details_lower) ||
    num_components >= 4

  # easy indicators
  is_easy <- grepl("^(add|use|show|display|provide)\\s", details_lower) &&
    num_components <= 2 &&
    !grepl("conditional|dynamic|multiple", details_lower)

  if (is_hard) {
    return("Hard")
  } else if (is_easy) {
    return("Easy")
  } else {
    return("Medium")
  }
}

#' Assign category based on suggestion content and concept
#'
#' @description
#' Categorizes suggestions into high-level groupings based on their focus area.
#'
#' @param suggestion Suggestion object with title and details
#' @param concept The behavioral science concept this suggestion relates to
#' @return Character string representing the category
#' @keywords internal
assign_category <- function(suggestion, concept) {
  title_lower <- tolower(suggestion$title)
  details_lower <- tolower(suggestion$details)
  concept_lower <- tolower(concept)
  combined_text <- paste(title_lower, details_lower)

  # check for visual patterns first (more specific)
  if (grepl("visual|color|size|priority|emphasis|style", combined_text)) {
    return("Visual Design")
  }

  # check for help/guidance patterns (before content to avoid false positives)
  if (grepl("help|guidance|tutorial|onboarding|tooltip|hint|example", combined_text)) {
    return("Guidance")
  }

  # check for navigation/structure patterns
  if (grepl("navigation|\\btab\\b|menu|header|footer", combined_text)) {
    return("Navigation")
  }

  # check for layout patterns
  if (grepl("layout|spacing|group|position|arrange|column|row|container", combined_text)) {
    return("Layout")
  }

  # check for content patterns
  if (grepl("content|text|label|description|message|information|data", combined_text)) {
    return("Content")
  }

  # check for interaction patterns
  if (grepl("filter|control|input|button|click|interact|action", combined_text)) {
    return("Interaction")
  }

  # check for hierarchy last (can be in multiple contexts)
  if (grepl("hierarchy", combined_text)) {
    return("Visual Design")
  }

  # fallback to concept-based categorization
  if (grepl("cognitive load", concept_lower)) {
    return("Complexity Management")
  } else if (grepl("progressive disclosure", concept_lower)) {
    return("Information Architecture")
  } else if (grepl("visual hierarchy", concept_lower)) {
    return("Visual Design")
  } else if (grepl("onboarding", concept_lower)) {
    return("Guidance")
  } else if (grepl("proximity", concept_lower)) {
    return("Layout")
  } else {
    return("General")
  }
}

#' Convert nested suggestion groups to flat tibble
#'
#' @description
#' Transforms the nested list structure of suggestions (grouped by concept)
#' into a flat tibble with one row per suggestion. Maintains backward
#' compatibility by keeping the nested structure available.
#'
#' @param suggestion_groups List of concept groups with suggestions
#' @return Tibble with columns: concept, title, details, components,
#'         rationale, score, difficulty, category
#' @keywords internal
flatten_suggestions_to_tibble <- function(suggestion_groups) {
  if (length(suggestion_groups) == 0) {
    # return empty tibble with correct structure
    return(tibble::tibble(
      concept = character(0),
      title = character(0),
      details = character(0),
      components = character(0),
      rationale = character(0),
      score = numeric(0),
      difficulty = character(0),
      category = character(0)
    ))
  }

  # flatten nested structure
  rows <- list()
  for (group in suggestion_groups) {
    concept <- group$concept
    for (suggestion in group$suggestions) {
      # assign difficulty and category
      difficulty <- assign_difficulty(suggestion)
      category <- assign_category(suggestion, concept)

      # create row
      row <- tibble::tibble(
        concept = concept,
        title = suggestion$title,
        details = suggestion$details,
        components = paste(suggestion$components, collapse = ", "),
        rationale = suggestion$rationale,
        score = suggestion$score,
        difficulty = difficulty,
        category = category
      )
      rows <- append(rows, list(row))
    }
  }

  # combine all rows
  result <- do.call(rbind, rows)

  # ensure proper ordering by score (descending)
  result <- result[order(result$score, decreasing = TRUE), ]

  return(result)
}
