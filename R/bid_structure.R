#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard, including layout and
#' design elements such as proximity, dual-processing, and default effects.
#'
#' @param previous_stage A tibble or list output from \code{bid_interpret()}.
#' @param layout A character string indicating the layout type (e.g.,
#'        "dual_process", "grid", "card", "tabs", "breathable").
#' @param concepts A character vector of BID concepts applied in this stage.
#' @param accessibility A list of accessibility considerations (optional).
#'
#' @return A tibble containing the documented information for the "Structure"
#'         stage.
#'
#' @examples
#' interpret <- bid_notice(
#'   problem = "Users struggle with information overload",
#'   evidence = "Survey results indicate delays"
#' ) |>
#'   bid_interpret(
#'     central_question = "How can we simplify data presentation?",
#'     data_story = list(
#'       hook = "Data is too complex",
#'       context = "Overloaded with charts",
#'       tension = "Confusing layout",
#'       resolution = "Introduce clear grouping"
#'     )
#'   )
#' 
#' # Basic usage
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("principle_of_proximity", "default_effect")
#' )
#' 
#' # With accessibility considerations
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("principle_of_proximity", "default_effect"),
#'   accessibility = list(
#'     color_contrast = "Using WCAG AA-compliant color contrasts",
#'     keyboard_navigation = "All interactive elements are keyboard accessible",
#'     screen_reader = "Charts include descriptive alt text"
#'   )
#' )
#'
#' @export
bid_structure <- function(
    previous_stage,
    layout,
    concepts,
    accessibility = NULL) {
  validate_required_params(
    previous_stage = previous_stage,
    layout = layout,
    concepts = concepts
  )

  validate_previous_stage(previous_stage, "Interpret")

  layout <- tolower(layout)
  valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")
  
  if (!layout %in% valid_layouts) {
    warning(
      paste0(
        "Layout '", layout, "' is not one of the recommended layouts: ",
        paste(valid_layouts, collapse = ", "), ". ",
        "Proceeding with custom layout."
      )
    )
  }

  all_concepts <- bid_concepts()
  known_concepts <- tolower(gsub("[^a-zA-Z0-9]", "_", all_concepts$concept))
  provided_concepts <- tolower(gsub("[^a-zA-Z0-9]", "_", concepts))

  unknown_concepts <- provided_concepts[!provided_concepts %in% known_concepts]
  if (length(unknown_concepts) > 0) {
    warning(
      "Some concepts are not currently recognized: ",
      paste(unknown_concepts, collapse = ", ")
    )
  }

  layout_suggestions <- switch(layout,
    "dual_process" = paste(
      "Consider separating quick insights (System 1) from detailed analysis",
      "(System 2)."
    ),
    "grid" = paste(
      "Ensure your grid layout groups related metrics and maintains clear",
      "visual hierarchy."
    ),
    "card" = paste(
      "Use cards to visually separate distinct content areas and enable",
      "flexible layout."
    ),
    "tabs" = paste(
      "Tabs work well for distinct categories of information, but ensure",
      "critical info isn't hidden."
    ),
    "breathable" = paste(
      "Use whitespace effectively to create visual rhythm and reduce cognitive",
      "load."
    ),
    paste(
      "Consider how your layout choice affects information visibility and",
      "cognitive load."
    )
  )

  # get implementation tips
  concept_tips <- character(0)
  for (concept in concepts) {
    concept_info <- bid_concept(concept)
    if (!is.null(concept_info) && nrow(concept_info) > 0) {
      concept_tips <- c(concept_tips, concept_info$implementation_tips)
    }
  }

  # generate accessibility suggestions
  accessibility_suggestion <- if (
    !is.null(accessibility) && length(accessibility) > 0
  ) {
    paste0(
      "Good job including accessibility considerations.",
      "Remember to test with actual assistive technologies."
    )
  } else {
    paste(
      "Consider adding accessibility features like sufficient color contrast,",
      "keyboard navigation, and screen reader support."
    )
  }

  # format accessibility considerations
  accessibility_formatted <- if (
    !is.null(accessibility) && length(accessibility) > 0
  ) {
    if (is.list(accessibility)) {
      jsonlite::toJSON(accessibility)
    } else {
      as.character(accessibility)
    }
  } else {
    NA_character_
  }

  # final suggestions
  if (length(concept_tips) > 0) {
    suggestions <- paste(
      layout_suggestions,
      paste(concept_tips, collapse = " "),
      accessibility_suggestion,
      sep = " "
    )
  } else {
    suggestions <- paste(
      layout_suggestions,
      accessibility_suggestion,
      sep = " "
    )
  }

  result <- tibble::tibble(
    stage = "Structure",
    layout = layout,
    concepts = paste(concepts, collapse = ", "),
    accessibility = accessibility_formatted,
    previous_question = previous_stage$central_question[1],
    previous_story_hook = previous_stage$hook[1],
    previous_audience = previous_stage$audience[1] %||% NA_character_,
    previous_personas = previous_stage$user_personas[1] %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 3 (Structure) completed.",
    paste0("Layout: ", layout),
    paste0("Concepts: ", paste(concepts, collapse = ", ")),
    if (!is.null(accessibility)) "Accessibility considerations included",
    accessibility_suggestion
  )

  return(result)
}
