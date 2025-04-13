#' Document Validation and Empowerment Stage in BID Framework
#'
#' @description
#' This function documents the validation and empowerment stage by capturing how
#' the dashboard is validated, including summary takeaways and collaborative
#' annotations.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage function.
#' @param summary_panel A character string summarizing key insights. If NULL,
#'        the function will suggest a summary based on previous stages.
#' @param collaboration A character string describing collaboration or
#'        annotation features. If NULL, the function will suggest collaboration
#'        features based on previous stages.
#' @param next_steps An optional character vector listing recommended next steps or
#'        actions for users (follows Peak-End Rule). If NULL, the function will
#'        suggest next steps based on previous stages.
#'
#' @return A tibble containing the documented information for the "Validate"
#'         stage.
#'
#' @examples
#' anticipate_info <- bid_anticipate(
#'   bid_structure(
#'     bid_interpret(
#'       bid_notice(
#'         "Users confused by interface",
#'         evidence = "Multiple clicks observed"
#'       ),
#'       central_question = "How can we enhance clarity?",
#'       data_story = list(
#'         hook = "Lack of focus",
#'         context = "Too many visual elements",
#'         tension = "User complaints",
#'         resolution = "Improve layout"
#'       )
#'     ),
#'     layout = "dual_process",
#'     concepts = c("principle_of_proximity", "default_effect")
#'   ),
#'   bias_mitigations = list(
#'     anchoring = "Reference baseline values",
#'     framing = "Use gain-framed messages"
#'   )
#' )
#'
#' # Basic usage
#' bid_validate(
#'   previous_stage = anticipate_info,
#'   summary_panel = "Key insights include clarity and user satisfaction",
#'   collaboration = "Annotations enabled for team feedback"
#' )
#'
#' # Let the function suggest content based on previous stages
#' bid_validate(
#'   previous_stage = anticipate_info
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
    summary_panel = NULL,
    collaboration = NULL,
    next_steps = NULL) {
  validate_required_params(
    previous_stage = previous_stage
  )

  validate_previous_stage(previous_stage, "Validate")

  validation_concepts <- bid_concepts("peak|end|beautiful|cooperation")

  if (is.null(summary_panel)) {
    bias_mitigations <- previous_stage$bias_mitigations[1]

    previous_layout <- if ("previous_layout" %in% names(previous_stage)) {
      previous_stage$previous_layout[1]
    } else {
      NA_character_
    }

    previous_concepts <- if ("previous_concepts" %in% names(previous_stage)) {
      previous_stage$previous_concepts[1]
    } else {
      NA_character_
    }

    summary_parts <- character(0)

    if (!is.na(previous_layout)) {
      layout_summary <- switch(previous_layout,
        "dual_process" = "The dashboard uses a dual-process layout, separating quick insights from detailed analysis.",
        "grid" = "The dashboard uses a grid layout for easy comparison of related metrics.",
        "card" = "The dashboard uses cards to organize and separate different content areas.",
        "tabs" = "The dashboard uses tabs to separate different categories of information.",
        "breathable" = "The dashboard uses adequate whitespace for reduced cognitive load.",
        paste("The dashboard uses a", previous_layout, "layout.")
      )
      summary_parts <- c(summary_parts, layout_summary)
    }

    if (!is.na(bias_mitigations)) {
      bias_names <- gsub(":.*?;", ";", bias_mitigations)
      bias_names <- gsub(":.*$", "", bias_names)
      bias_summary <- paste0(
        "Cognitive biases are addressed through strategies for ",
        bias_names, "."
      )
      summary_parts <- c(summary_parts, bias_summary)
    }

    if (!is.na(previous_concepts)) {
      concept_list <- strsplit(previous_concepts, ", ")[[1]]
      if (length(concept_list) > 0) {
        if (length(concept_list) == 1) {
          concept_summary <- paste0(
            "The design implements the ", concept_list[1], " concept."
          )
        } else {
          concept_summary <- paste0(
            "The design implements key concepts including ",
            paste(concept_list[1:min(3, length(concept_list))], collapse = ", "), "."
          )
        }
        summary_parts <- c(summary_parts, concept_summary)
      }
    }

    if (length(summary_parts) == 0) {
      summary_parts <- c(
        "This dashboard implements behavioral science principles to enhance user experience.",
        "Key metrics are presented in an intuitive layout with appropriate context."
      )
    }

    summary_parts <- c(
      summary_parts,
      "The design aims to minimize cognitive load while providing clear insights and actionable information."
    )

    summary_panel <- paste(summary_parts, collapse = " ")

    message("Automatically generated summary panel based on previous stages.")
  }

  if (is.null(collaboration)) {
    collaboration <- paste(
      "Enable team annotations and comments to foster collaborative decision-making.",
      "Include export and sharing options for key insights.",
      "Consider implementing saved view functionality for recurring analysis tasks."
    )

    message("Automatically suggested collaboration features.")
  }

  if (is.null(next_steps)) {
    suggested_steps <- character(0)

    problem <- NA_character_
    biases <- NA_character_

    if (!is.na(previous_stage$bias_mitigations[1])) {
      biases <- strsplit(previous_stage$bias_mitigations[1], ";")[[1]]
      biases <- gsub("^\\s*|\\s*$", "", biases)
    }

    if (!is.na(biases[1])) {
      for (bias in biases) {
        if (grepl("anchor", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Review baseline reference points for accuracy and relevance")
        } else if (grepl("fram", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Test alternative data framings with actual users")
        } else if (grepl("confirm", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Review data for alternative interpretations")
        }
      }
    }

    if (length(suggested_steps) < 2) {
      suggested_steps <- c(
        suggested_steps,
        "Schedule user testing sessions to validate design improvements",
        "Document successful patterns for future projects",
        "Review dashboard with stakeholders to ensure it meets business needs"
      )
    }

    if (length(suggested_steps) > 5) {
      suggested_steps <- suggested_steps[1:5]
    }

    next_steps <- suggested_steps

    message(paste0(
      "Automatically suggested ", length(next_steps), " next steps based on previous stages."
    ))
  }

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
    "Consider adding these visualization components based on your BID implementation:",
    "- A summary card with key insights",
    "- Collaboration features like annotations or comments",
    if (!is.null(next_steps)) "- Clear next steps section implemented" else "- A clear 'next steps' section aligned with the Peak-End Rule"
  )

  return(result)
}
