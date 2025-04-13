#' Document User Interpretation Stage in BID Framework
#'
#' @description
#' This function documents the interpretation of user needs, capturing the
#' central question and the data storytelling narrative. It represents stage 2
#' in the BID framework.
#'
#' @param previous_stage A tibble or list output from \code{bid_notice()}.
#' @param central_question A character string representing the main question to
#'        be answered.
#' @param data_story A list containing elements such as \code{hook},
#'        \code{context}, \code{tension}, \code{resolution}, and optionally
#'        \code{audience}, \code{metrics}, and \code{visual_approach}.
#' @param user_personas Optional list of user personas to consider in the design.
#'
#' @return A tibble containing the documented information for the "Interpret"
#'         stage.
#'
#' @examples
#' notice <- bid_notice(
#'   problem = "Users struggle with complex data",
#'   evidence = "Test results indicate delays"
#' )
#'
#' # Basic usage
#' bid_interpret(
#'   previous_stage = notice,
#'   central_question = "What drives the decline in user engagement?",
#'   data_story = list(
#'     hook = "Declining trend in engagement",
#'     context = "Previous high engagement levels",
#'     tension = "Unexpected drop",
#'     resolution = "Investigate new UI changes",
#'     audience = "Marketing team",
#'     metrics = c("Daily Active Users", "Session Duration"),
#'     visual_approach = "Comparison charts showing before/after UI change"
#'   )
#' )
#'
#' # With user personas
#' bid_interpret(
#'   previous_stage = notice,
#'   central_question = "How can we improve data discovery?",
#'   data_story = list(
#'     hook = "Users are missing key insights",
#'     context = "Critical data is available but overlooked",
#'     tension = "Time-sensitive decisions are delayed",
#'     resolution = "Highlight key metrics more effectively"
#'   ),
#'   user_personas = list(
#'     list(
#'       name = "Sara, Data Analyst",
#'       goals = "Needs to quickly find patterns in data",
#'       pain_points = "Gets overwhelmed by too many visualizations",
#'       technical_level = "Advanced"
#'     ),
#'     list(
#'       name = "Marcus, Executive",
#'       goals = "Wants high-level insights at a glance",
#'       pain_points = "Limited time to analyze detailed reports",
#'       technical_level = "Basic"
#'     )
#'   )
#' )
#'
#' @export
bid_interpret <- function(
  previous_stage,
  central_question,
  data_story,
  user_personas = NULL
) {
  validate_required_params(
    previous_stage = previous_stage,
    central_question = central_question,
    data_story = data_story
  )

  validate_previous_stage(previous_stage, "Notice")

  storytelling_concepts <- bid_concepts("storytelling|fluency")

  # generate storytelling suggestions
  required_story_elements <- c("hook", "context", "tension", "resolution")
  provided_elements <- required_story_elements %in% names(data_story)
  story_completeness <- sum(provided_elements) / length(required_story_elements)

  # generate story suggestions
  if (story_completeness < 0.5) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- paste0(
      "Your data story is incomplete. Consider adding these missing elements: ",
      paste(missing_elements, collapse = ", "),
      "."
    )
  } else if (story_completeness < 0.75) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- paste0(
      "Your data story is taking shape. Consider adding: ",
      paste(missing_elements, collapse = ", "),
      "."
    )
  } else if (story_completeness < 1) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- paste0(
      "Your data story is almost complete. Consider adding: ",
      paste(missing_elements, collapse = ", "),
      "."
    )
  } else {
    story_suggestion <- paste(
      "Your data story has all key elements. Focus on making each component",
      "compelling and relevant."
    )
  }

  # generate question suggestions
  question_suggestion <- if (
    is.na(central_question) || is.null(central_question)
  ) {
    "Please provide a central question to guide your dashboard design."
  } else if (stringr::str_length(central_question) > 100) {
    "Consider simplifying your central question for more focus."
  } else if (stringr::str_length(central_question) < 20) {
    "Your central question might benefit from more specificity."
  } else {
    "Your central question is appropriately scoped."
  }

  # generate persona suggestions
  persona_suggestion <- if (
    !is.null(user_personas) && length(user_personas) > 0
  ) {
    paste0(
      "You've defined ",
      length(user_personas),
      " persona(s). ",
      "Ensure your design addresses the specific needs of each."
    )
  } else {
    "Consider defining specific user personas to better target your design."
  }

  # final suggestions
  suggestions <- paste(
    story_suggestion,
    question_suggestion,
    persona_suggestion,
  )

  # additional context elements
  audience <- data_story$audience %||% NA_character_

  metrics <- if (!is.null(data_story$metrics)) {
    if (is.character(data_story$metrics)) {
      paste(data_story$metrics, collapse = ", ")
    } else {
      paste(as.character(data_story$metrics), collapse = ", ")
    }
  } else {
    NA_character_
  }

  visual_approach <- data_story$visual_approach %||% NA_character_

  personas_formatted <- if (
    !is.null(user_personas) && length(user_personas) > 0
  ) {
    jsonlite::toJSON(user_personas)
  } else {
    NA_character_
  }

  result <- tibble::tibble(
    stage = "Interpret",
    central_question = central_question,
    hook = data_story$hook %||% NA_character_,
    context = data_story$context %||% NA_character_,
    tension = data_story$tension %||% NA_character_,
    resolution = data_story$resolution %||% NA_character_,
    audience = audience,
    metrics = metrics,
    visual_approach = visual_approach,
    user_personas = personas_formatted,
    previous_problem = previous_stage$problem[1],
    previous_theory = previous_stage$theory[1],
    previous_audience = previous_stage$target_audience[1] %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 2 (Interpret) completed.",
    paste0(
      "Central question: ",
      substring(central_question, 1, 60),
      if (nchar(central_question) > 60) "..." else ""
    ),
    story_suggestion,
    question_suggestion,
    if (!is.null(user_personas)) {
      paste0("User personas: ", length(user_personas), " defined")
    }
  )

  return(result)
}
