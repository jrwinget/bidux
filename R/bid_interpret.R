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
#'
#' @return A tibble containing the documented information for the "Interpret"
#'         stage.
#'
#' @examples
#' notice <- bid_notice(
#'   problem = "Users struggle with complex data",
#'   evidence = "Test results indicate delays"
#' )
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
#' @export
bid_interpret <- function(previous_stage, central_question, data_story) {
  if (
    missing(previous_stage) || missing(central_question) || missing(data_story)
  ) {
    stop(
      paste(
        "All parameters (previous_stage, central_question, data_story) must be",
        "provided."
      )
    )
  }

  # Get appropriate storytelling concepts
  storytelling_concepts <- bid_concepts("storytelling|fluency")

  # Generate storytelling suggestions based on data_story components
  story_completeness <- sum(
    c("hook", "context", "tension", "resolution") %in% names(data_story)
  )

  story_completeness <- story_completeness / 4 # Normalize to 0-1

  if (story_completeness < 0.5) {
    story_suggestion <- paste(
      "Consider enhancing your data story with more elements (hook, context,",
      "tension, resolution)."
    )
  } else if (story_completeness < 0.75) {
    story_suggestion <- paste(
      "Your data story is taking shape. Consider adding any missing elements",
      "for a complete narrative."
    )
  } else {
    story_suggestion <- paste(
      "Your data story has a good structure. Focus on making each element",
      "compelling and relevant."
    )
  }

  # Additional suggestions based on central question
  question_suggestion <- if (stringr::str_length(central_question) > 100) {
    "Consider simplifying your central question for more focus."
  } else if (stringr::str_length(central_question) < 20) {
    "Your central question might benefit from more specificity."
  } else {
    "Your central question is appropriately scoped."
  }

  # Combine suggestions
  suggestions <- paste(story_suggestion, question_suggestion, sep = " ")

  # Get additional context elements if provided
  audience <- data_story$audience %||% NA

  metrics <- if (!is.null(data_story$metrics)) {
    paste(data_story$metrics, collapse = ", ")
  } else {
    NA
  }

  visual_approach <- data_story$visual_approach %||% NA

  # Create result tibble
  result <- tibble::tibble(
    stage = "Interpret",
    central_question = central_question,
    hook = data_story$hook %||% NA,
    context = data_story$context %||% NA,
    tension = data_story$tension %||% NA,
    resolution = data_story$resolution %||% NA,
    audience = audience,
    metrics = metrics,
    visual_approach = visual_approach,
    previous_problem = previous_stage$problem[1],
    previous_theory = previous_stage$theory[1],
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  # Provide helpful messaging
  message(
    "Stage 2 (Interpret) completed. Key suggestions:\n",
    "- ", story_suggestion, "\n",
    "- ", question_suggestion
  )

  return(result)
}
