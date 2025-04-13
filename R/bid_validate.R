#' Document Validation and Empowerment Stage in BID Framework
#'
#' @description
#' This function documents the validation and empowerment stage by capturing how
#' the dashboard is validated, including summary takeaways and collaborative
#' annotations.
#'
#' @param previous_stage A tibble or list output from \code{bid_anticipate()}.
#' @param summary_panel A character string summarizing key insights.
#' @param collaboration A character string describing collaboration or
#'        annotation features.
#' @param next_steps An optional character vector listing recommended next steps
#'        or actions for users (follows Peak-End Rule).
#'
#' @return A tibble containing the documented information for the "Validate"
#'         stage.
#'
#' @examples
#' anticipate_info <- bid_notice(
#'   problem = "Users confused by interface",
#'   evidence = "Multiple clicks observed"
#' ) |>
#'   bid_interpret(
#'     central_question = "How can we enhance clarity?",
#'     data_story = list(
#'       hook = "Lack of focus",
#'       context = "Too many visual elements",
#'       tension = "User complaints",
#'       resolution = "Improve layout"
#'     )
#'   ) |>
#'   bid_structure(
#'     layout = "dual_process",
#'     concepts = c("principle_of_proximity", "default_effect")
#'   ) |> 
#'   bid_anticipate(
#'     bias_mitigations = list(
#'       anchoring = "Reference baseline values",
#'       framing = "Use gain-framed messages"
#'     )
#'   )
#' 
#' # Basic usage
#' bid_validate(
#'   previous_stage = anticipate_info,
#'   summary_panel = "Key insights include clarity and user satisfaction",
#'   collaboration = "Annotations enabled for team feedback"
#' )
#' 
#' # With next steps
#' bid_validate(
#'   previous_stage = anticipate_info,
#'   summary_panel = "Key insights include clarity and user satisfaction",
#'   collaboration = "Annotations enabled for team feedback",
#'   next_steps = c(
#'     "Review performance metrics",
#'     "Schedule team discussion",
#'     "Implement suggested UI changes"
#'   )
#' )
#'
#' @export
bid_validate <- function(
    previous_stage,
    summary_panel,
    collaboration,
    next_steps = NULL) {
  validate_required_params(
    previous_stage = previous_stage,
    summary_panel = summary_panel,
    collaboration = collaboration
  )
  
  validate_previous_stage(previous_stage, "Anticipate")

  validation_concepts <- bid_concepts("peak|end|beautiful|cooperation")

  # generate summary panel suggestions
  summary_suggestion <- if (is.na(summary_panel) || is.null(summary_panel)) {
    "Please provide a summary panel to highlight key insights."
  } else if (stringr::str_length(summary_panel) < 50) {
    "Consider expanding your summary panel to highlight 3-5 key takeaways."
  } else if (stringr::str_length(summary_panel) > 200) {
    "Your summary may be too long. Focus on the most important insights."
  } else {
    paste(
      "Your summary length is appropriate.",
      "Ensure it focuses on actionable insights."
    )
  }

  # generate collaboration features
  collaboration_suggestion <- if (is.na(collaboration) || is.null(collaboration)) {
    "Please specify collaboration features for your dashboard."
  } else if (
    stringr::str_detect(
      tolower(collaboration), 
      "annotation|comment|feedback|discuss|share|collaborate"
    )
  ) {
    paste(
      "Your collaboration approach includes user feedback,",
      "which aligns with best practices."
    )
  } else {
    paste(
      "Consider adding annotation or commenting features to enhance",
      "collaboration."
    )
  }
  
  # generate next steps suggestions
  next_steps_suggestion <- if (!is.null(next_steps) && length(next_steps) > 0) {
    paste0(
      "Good job including ", length(next_steps), " next steps. ",
      "This follows the Peak-End Rule by ending with clear actions."
    )
  } else {
    paste(
      "Consider adding specific next steps for users to follow.",
      "This helps implement the Peak-End Rule."
    )
  }
  
  next_steps_formatted <- if (!is.null(next_steps) && length(next_steps) > 0) {
    paste(next_steps, collapse = "; ")
  } else {
    NA_character_
  }

  # final suggestions
  suggestions <- paste(
    summary_suggestion, 
    collaboration_suggestion, 
    next_steps_suggestion,
    sep = " "
  )

  result <- tibble::tibble(
    stage = "Validate",
    summary_panel = summary_panel,
    collaboration = collaboration,
    next_steps = next_steps_formatted,
    previous_bias = previous_stage$bias_mitigations[1],
    previous_interaction = previous_stage$interaction_principles[1] %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 5 (Validate) completed. Final BID Framework implementation summary:",
    summary_suggestion,
    collaboration_suggestion,
    next_steps_suggestion,
    paste(
      "Consider adding these visualization components based on your BID",
      "implementation:"
    ),
    "- A summary card with key insights",
    "- Collaboration features like annotations or comments",
    if (!is.null(next_steps)) {
      "- Clear next steps section implemented"
    } else {
      "- A clear 'next steps' section aligned with the Peak-End Rule"
    }
  )

  return(result)
}
