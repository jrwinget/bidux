#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard, including layout and
#' design elements such as proximity, dual-processing, and default effects.
#'
#' @param previous_stage A tibble or list output from \code{bid_interpret()}.
#' @param layout A character string indicating the layout type (e.g.,
#'        "dual_process", "grid").
#' @param concepts A character vector of BID concepts applied in this stage.

#' @return A tibble containing the documented information for the "Structure"
#'         stage.

#' @examples
#' interpret <- bid_interpret(
#'   bid_notice(
#'     problem = "Users struggle with information overload",
#'     evidence = "Survey results indicate delays"
#'   ),
#'   central_question = "How can we simplify data presentation?",
#'   data_story = list(
#'     hook = "Data is too complex",
#'     context = "Overloaded with charts",
#'     tension = "Confusing layout",
#'     resolution = "Introduce clear grouping"
#'   )
#' )
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("principle_of_proximity", "default_effect")
#' )

#' @export
bid_structure <- function(previous_stage, layout, concepts) {
  if (missing(previous_stage) || missing(layout) || missing(concepts)) {
    stop("All parameters (previous_stage, layout, concepts) must be provided.")
  }

  # Validate concepts against known concepts
  all_concepts <- bid_concepts()
  known_concepts <- tolower(gsub("[^a-zA-Z0-9]", "_", all_concepts$concept))
  provided_concepts <- tolower(gsub("[^a-zA-Z0-9]", "_", concepts))

  unknown_concepts <- provided_concepts[!provided_concepts %in% known_concepts]
  if (length(unknown_concepts) > 0) {
    warning(
      "Some concepts are not recognized: ",
      paste(unknown_concepts, collapse = ", ")
    )
  }

  # Generate layout suggestions
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
    paste(
      "Consider how your layout choice affects information visibility and",
      "cognitive load."
    )
  )

  # Get implementation tips for the concepts
  concept_tips <- character(0)
  for (concept in concepts) {
    concept_info <- bid_concept(concept)
    if (!is.null(concept_info) && nrow(concept_info) > 0) {
      concept_tips <- c(concept_tips, concept_info$implementation_tips)
    }
  }

  # Combine suggestions
  if (length(concept_tips) > 0) {
    suggestions <- paste(
      layout_suggestions,
      paste(concept_tips, collapse = " "),
      sep = " "
    )
  } else {
    suggestions <- layout_suggestions
  }

  # Create result tibble
  result <- tibble::tibble(
    stage = "Structure",
    layout = layout,
    concepts = paste(concepts, collapse = ", "),
    previous_question = previous_stage$central_question[1],
    previous_story_hook = previous_stage$hook[1],
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  # Provide helpful messaging
  message(
    "Stage 3 (Structure) completed. Layout suggestion: ",
    layout_suggestions
  )

  return(result)
}
