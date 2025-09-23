#' Generate ranked, concept-grouped, actionable UI/UX suggestions
#'
#' @description
#' Creates structured suggestions organized by UX concepts with specific
#' component recommendations and rationales. Suggestions are ranked by
#' relevance and grouped by concept for systematic implementation.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#' @param chosen_layout Character string with the selected layout type
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
#' - components: Shiny/bslib component recommendations
#' - rationale: 1-2 sentence explanation
#' - score: Relevance ranking (0-1)
#'
#' @importFrom cli cli_alert_info
#' @keywords internal
structure_suggestions <- function(
    previous_stage,
    chosen_layout,
    concepts = NULL) {
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
    chosen_layout,
    previous_stage
  )
  groups <- rank_and_sort_suggestions(groups, previous_stage, chosen_layout)

  cli::cli_alert_info(
    'Tip: Learn more about any concept via bid_concept("<concept>").'
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
  # Extract story text
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

  # keyword-based concept detection
  concept_keywords <- list(
    "Cognitive Load Theory" = c(
      "overload",
      "overwhelm",
      "too many",
      "complex",
      "confusing",
      "mental load"
    ),
    "Progressive Disclosure" = c(
      "step",
      "gradually",
      "reveal",
      "detail",
      "progressive",
      "stage",
      "phase"
    ),
    "Visual Hierarchy" = c(
      "hierarchy",
      "priority",
      "important",
      "focus",
      "attention",
      "prominence"
    ),
    "Dual-Processing Theory" = c(
      "quick",
      "glance",
      "summary",
      "overview",
      "detail",
      "fast",
      "thorough"
    ),
    "User Onboarding" = c(
      "first time",
      "new user",
      "beginner",
      "getting started",
      "initial",
      "welcome"
    ),
    "Information Scent" = c(
      "find",
      "search",
      "locate",
      "discover",
      "navigation",
      "scent",
      "wayfinding"
    ),
    "Principle of Proximity" = c(
      "group",
      "related",
      "together",
      "proximity",
      "close",
      "associate"
    )
  )

  for (concept_name in names(concept_keywords)) {
    keywords <- concept_keywords[[concept_name]]
    if (any(sapply(keywords, function(k) grepl(k, story_lower)))) {
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
#' @param chosen_layout Selected layout type
#' @param previous_stage Previous stage data for context
#' @return List of concept groups with suggestions
#' @keywords internal
build_groups_with_suggestions <- function(
    concepts_final,
    chosen_layout,
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
    group <- build_concept_group(concept, chosen_layout, previous_stage)
    if (!is.null(group) && length(group$suggestions) > 0) {
      groups <- append(groups, list(group))
    }
  }

  return(groups)
}

#' Build suggestions for a specific concept
#'
#' @param concept Name of the concept to generate suggestions for
#' @param chosen_layout Selected layout type
#' @param previous_stage Previous stage data
#' @return List with concept name and suggestions
#' @keywords internal
build_concept_group <- function(concept, chosen_layout, previous_stage) {
  suggestions <- switch(concept,
    "Cognitive Load Theory" = get_cognitive_load_suggestions(
      chosen_layout,
      previous_stage
    ),
    "Progressive Disclosure" = get_progressive_disclosure_suggestions(
      chosen_layout,
      previous_stage
    ),
    "Visual Hierarchy" = get_visual_hierarchy_suggestions(
      chosen_layout,
      previous_stage
    ),
    "Dual-Processing Theory" = get_dual_processing_suggestions(
      chosen_layout,
      previous_stage
    ),
    "User Onboarding" = get_onboarding_suggestions(
      chosen_layout,
      previous_stage
    ),
    "Information Scent" = get_information_scent_suggestions(
      chosen_layout,
      previous_stage
    ),
    "Principle of Proximity" = get_proximity_suggestions(
      chosen_layout,
      previous_stage
    ),
    get_generic_suggestions(concept, chosen_layout, previous_stage)
  )

  if (length(suggestions) == 0) {
    return(NULL)
  }

  return(list(
    concept = concept,
    suggestions = suggestions
  ))
}

#' Generate Cognitive Load Theory suggestions
#' @keywords internal
get_cognitive_load_suggestions <- function(chosen_layout, previous_stage) {
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

  # Layout-specific adjustments
  if (chosen_layout == "breathable") {
    base_suggestions[[1]]$score <- base_suggestions[[1]]$score + 0.05
  }

  return(base_suggestions)
}

#' Generate Progressive Disclosure suggestions
#' @keywords internal
get_progressive_disclosure_suggestions <- function(
    chosen_layout,
    previous_stage) {
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

  # Boost score for tabs layout
  if (chosen_layout == "tabs") {
    base_suggestions[[1]]$score <- base_suggestions[[1]]$score + 0.05
    base_suggestions[[2]]$score <- base_suggestions[[2]]$score + 0.03
  }

  # Check telemetry for tab issues
  telemetry_data <- safe_column_access(previous_stage, "telemetry", NULL)
  if (!is.null(telemetry_data) && isTRUE(telemetry_data$nav_dropoff_tabs)) {
    # Demote tab-related suggestions
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
get_visual_hierarchy_suggestions <- function(chosen_layout, previous_stage) {
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
get_dual_processing_suggestions <- function(chosen_layout, previous_stage) {
  base_suggestions <- list(
    list(
      title = "Provide summary and detail views",
      details = "Show key insights prominently with option to drill into detailed data.",
      components = c("bslib::value_box", "DT::datatable", "shiny::tabsetPanel"),
      rationale = "Serves both System 1 (quick) and System 2 (deliberate) thinking modes.",
      score = 0.89
    )
  )

  # Boost for dual_process layout
  if (chosen_layout == "dual_process") {
    base_suggestions[[1]]$score <- base_suggestions[[1]]$score + 0.08
  }

  return(base_suggestions)
}

#' Generate User Onboarding suggestions
#' @keywords internal
get_onboarding_suggestions <- function(chosen_layout, previous_stage) {
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
get_information_scent_suggestions <- function(chosen_layout, previous_stage) {
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
get_proximity_suggestions <- function(chosen_layout, previous_stage) {
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

  # Boost for grid layout
  if (chosen_layout == "grid") {
    base_suggestions[[1]]$score <- base_suggestions[[1]]$score + 0.05
  }

  return(base_suggestions)
}

#' Generate generic suggestions for unrecognized concepts
#' @keywords internal
get_generic_suggestions <- function(concept, chosen_layout, previous_stage) {
  list(
    list(
      title = paste("Apply", concept, "principles"),
      details = paste(
        "Consider how",
        concept,
        "applies to your",
        chosen_layout,
        "layout design."
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
#' @param chosen_layout Selected layout type
#' @return List of groups with ranked suggestions
#' @keywords internal
rank_and_sort_suggestions <- function(groups, previous_stage, chosen_layout) {
  for (i in seq_along(groups)) {
    # Apply context-based scoring adjustments
    for (j in seq_along(groups[[i]]$suggestions)) {
      groups[[i]]$suggestions[[j]] <- adjust_suggestion_score(
        groups[[i]]$suggestions[[j]],
        previous_stage,
        chosen_layout,
        groups[[i]]$concept
      )
    }

    # Sort suggestions by score (descending)
    scores <- sapply(groups[[i]]$suggestions, function(s) s$score)
    order_idx <- order(scores, decreasing = TRUE)
    groups[[i]]$suggestions <- groups[[i]]$suggestions[order_idx]
  }

  # Sort groups by highest suggestion score in each group
  group_max_scores <- sapply(groups, function(g) {
    max(sapply(g$suggestions, function(s) s$score))
  })
  group_order <- order(group_max_scores, decreasing = TRUE)

  return(groups[group_order])
}

#' Apply context-based scoring adjustments
#' @keywords internal
adjust_suggestion_score <- function(
    suggestion,
    previous_stage,
    chosen_layout,
    concept) {
  score <- suggestion$score

  # Boost if concept originated from Stage 1 theory
  stage1_theory <- extract_stage1_theory(previous_stage)
  if (concept %in% stage1_theory) {
    score <- score + 0.05
  }

  # Boost for layout-appropriate suggestions
  layout_boosts <- list(
    "breathable" = c("Cognitive Load Theory"),
    "tabs" = c("Progressive Disclosure"),
    "dual_process" = c("Dual-Processing Theory"),
    "grid" = c("Visual Hierarchy", "Principle of Proximity")
  )

  if (
    chosen_layout %in%
      names(layout_boosts) &&
      concept %in% layout_boosts[[chosen_layout]]
  ) {
    score <- score + 0.03
  }

  # Demote tabs-related suggestions if telemetry shows issues
  telemetry_data <- safe_column_access(previous_stage, "telemetry", NULL)
  if (
    !is.null(telemetry_data) &&
      isTRUE(telemetry_data$nav_dropoff_tabs) &&
      grepl("tab", suggestion$details, ignore.case = TRUE)
  ) {
    score <- score - 0.08
  }

  # Ensure score stays within bounds
  suggestion$score <- max(0, min(1, score))
  return(suggestion)
}
