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
  validate_required_params(previous_stage = previous_stage)
  validate_previous_stage(previous_stage, "Validate")

  if (!is.null(next_steps)) {
    if (!is.character(next_steps)) {
      cli::cli_abort(
        c(
          "The next_steps parameter must be a character vector",
          "i" = "You provided {.cls {class(next_steps)}}"
        )
      )
    }

    short_steps <- which(nchar(trimws(next_steps)) < 5)
    if (length(short_steps) > 0) {
      cli::cli_warn(
        c(
          "!" = "Some next steps are very short and may lack sufficient detail",
          "i" = "Consider expanding steps at positions: {paste(short_steps, collapse = ', ')}"
        )
      )
    }

    long_steps <- which(nchar(trimws(next_steps)) > 100)
    if (length(long_steps) > 0) {
      cli::cli_warn(
        c(
          "!" = "Some next steps are very long and may be difficult to follow",
          "i" = "Consider breaking down steps at positions: {paste(long_steps, collapse = ', ')}"
        )
      )
    }

    next_steps <- trimws(next_steps)
    next_steps <- next_steps[next_steps != ""]

    if (length(next_steps) == 0) {
      cli::cli_warn(
        c(
          "!" = "next_steps provided but contained only empty strings",
          "i" = "Treating as NULL and generating suggestions instead"
        )
      )
      next_steps <- NULL
    }
  }

  extract_from_previous <- function(field, default = NA_character_) {
    if (
      field %in% names(previous_stage) &&
        !is.na(previous_stage[[field]][1])
    ) {
      return(previous_stage[[field]][1])
    }

    previous_field <- paste0("previous_", field)
    if (
      previous_field %in% names(previous_stage) &&
        !is.na(previous_stage[[previous_field]][1])
    ) {
      return(previous_stage[[previous_field]][1])
    }
    return(default)
  }

  bias_mitigations <- extract_from_previous("bias_mitigations")
  interaction_principles <- extract_from_previous("interaction_principles")
  layout <- extract_from_previous("previous_layout", extract_from_previous("layout"))
  concepts <- extract_from_previous("previous_concepts", extract_from_previous("concepts"))
  accessibility <- extract_from_previous("previous_accessibility", extract_from_previous("accessibility"))
  problem <- extract_from_previous("previous_problem", extract_from_previous("problem"))
  central_question <- extract_from_previous("previous_question", extract_from_previous("central_question"))

  interaction_principles_parsed <- NULL
  if (!is.na(interaction_principles)) {
    tryCatch(
      {
        if (
          startsWith(interaction_principles, "{") ||
            startsWith(interaction_principles, "[")
        ) {
          interaction_principles_parsed <- jsonlite::fromJSON(interaction_principles)
        }
      },
      error = function(e) {
        # leave as NULL if parsing fails. handled later
        cli::cli_warn(
          c(
            "!" = "Could not parse interaction_principles as JSON",
            "i" = "Using as plain text instead"
          )
        )
      }
    )
  }

  validation_concepts <- bid_concepts("peak|end|beautiful|cooperation")

  if (is.null(summary_panel)) {
    summary_parts <- character(0)

    if (!is.na(layout)) {
      layout_summary <- switch(layout,
        "dual_process" = "The dashboard uses a dual-process layout, separating quick insights from detailed analysis.",
        "grid" = "The dashboard uses a grid layout for easy comparison of related metrics.",
        "card" = "The dashboard uses cards to organize and separate different content areas.",
        "tabs" = "The dashboard uses tabs to separate different categories of information.",
        "breathable" = "The dashboard uses adequate whitespace for reduced cognitive load.",
        paste("The dashboard uses a", layout, "layout.")
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

    if (!is.na(concepts)) {
      concept_list <- strsplit(concepts, ", ")[[1]]
      if (length(concept_list) > 0) {
        if (length(concept_list) == 1) {
          concept_summary <- paste0(
            "The design implements the ", concept_list[1], " concept."
          )
        } else {
          concept_summary <- paste0(
            "The design implements key concepts including ",
            paste(
              concept_list[1:min(3, length(concept_list))],
              collapse = ", "
            ),
            "."
          )
        }
        summary_parts <- c(summary_parts, concept_summary)
      }
    }

    if (!is.na(central_question)) {
      question_summary <- paste0(
        "The dashboard addresses the key question: \"",
        central_question,
        "\"."
      )
      summary_parts <- c(summary_parts, question_summary)
    }

    if (length(summary_parts) == 0) {
      summary_parts <- c(
        "This dashboard implements behavioral science principles to enhance user experience.",
        "Key metrics are presented in an intuitive layout with appropriate context."
      )
    }

    summary_parts <- c(
      summary_parts,
      paste(
        "The design aims to minimize cognitive load while",
        "providing clear insights and actionable information."
      )
    )

    summary_panel <- paste(summary_parts, collapse = " ")

    cli::cli_alert_info(
      "Automatically generated summary panel based on previous stages."
    )
  }

  if (is.null(collaboration)) {
    collaboration_parts <- character(0)

    if (!is.null(interaction_principles_parsed)) {
      if (
        any(
          grepl(
            "comment|annotation|note|collaborate",
            names(interaction_principles_parsed),
            ignore.case = TRUE
          )
        )
      ) {
        collaboration_parts <- c(
          collaboration_parts,
          "Enhance existing commenting and annotation features for team collaboration."
        )
      }

      if (
        any(
          grepl(
            "share|export|save",
            names(interaction_principles_parsed),
            ignore.case = TRUE
          )
        )
      ) {
        collaboration_parts <- c(
          collaboration_parts,
          "Extend sharing capabilities with email notifications and scheduled reports."
        )
      }
    }

    standard_collab <- c(
      "Enable team annotations and comments to foster collaborative decision-making.",
      "Include export and sharing options for key insights.",
      "Consider implementing saved view functionality for recurring analysis tasks."
    )

    collaboration_parts <- unique(c(collaboration_parts, standard_collab))
    collaboration <- paste(collaboration_parts, collapse = " ")

    cli::cli_alert_info("Automatically suggested collaboration features.")
  }

  if (is.null(next_steps)) {
    suggested_steps <- character(0)

    if (!is.na(bias_mitigations)) {
      biases <- strsplit(bias_mitigations, ";")[[1]]
      biases <- gsub("^\\s*|\\s*$", "", biases)

      for (bias in biases) {
        if (grepl("anchor", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Review baseline reference points for accuracy and relevance")
        } else if (grepl("fram", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Test alternative data framings with actual users")
        } else if (grepl("confirm", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Review data for alternative interpretations")
        } else if (grepl("availab", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Ensure all critical information is easily accessible")
        } else if (grepl("loss", bias, ignore.case = TRUE)) {
          suggested_steps <- c(suggested_steps, "Test both gain and loss framing with user groups")
        }
      }
    }

    if (!is.na(concepts)) {
      concept_list <- strsplit(concepts, ", ")[[1]]

      for (concept in concept_list) {
        concept_lower <- tolower(concept)
        if (grepl("hierarchy|visual", concept_lower)) {
          suggested_steps <- c(suggested_steps, "Validate visual hierarchy effectiveness with eye-tracking or user observation")
        } else if (grepl("proxim|group", concept_lower)) {
          suggested_steps <- c(suggested_steps, "Confirm related items are properly grouped in the interface")
        } else if (grepl("access", concept_lower)) {
          suggested_steps <- c(suggested_steps, "Test dashboard with screen reader and keyboard navigation")
        } else if (grepl("breath|space", concept_lower)) {
          suggested_steps <- c(suggested_steps, "Review whitespace and layout balance across different screen sizes")
        }
      }
    }

    if (!is.na(central_question)) {
      if (grepl("improve|enhance|optimize", tolower(central_question))) {
        suggested_steps <- c(suggested_steps, "Establish metrics to measure improvement after implementation")
      }
      if (grepl("compare|contrast|difference", tolower(central_question))) {
        suggested_steps <- c(suggested_steps, "Validate that comparison views effectively highlight key differences")
      }
    }

    if (length(suggested_steps) < 3) {
      default_steps <- c(
        "Schedule user testing sessions to validate design improvements",
        "Document successful patterns for future projects",
        "Review dashboard with stakeholders to ensure it meets business needs",
        "Set up metrics to measure dashboard effectiveness",
        "Plan for regular reviews and updates based on user feedback"
      )

      needed_steps <- max(0, 3 - length(suggested_steps))
      if (needed_steps > 0) {
        suggested_steps <- c(suggested_steps, default_steps[1:min(needed_steps, length(default_steps))])
      }
    }

    if (length(suggested_steps) > 5) {
      suggested_steps <- suggested_steps[1:5]
    }

    next_steps <- suggested_steps

    cli::cli_alert_info(paste0(
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
    if (length(next_steps) >= 3 && length(next_steps) <= 5) {
      paste0(
        "Good job including ", length(next_steps), " next steps. ",
        "This follows the Peak-End Rule by ending with clear actions."
      )
    } else if (length(next_steps) < 3) {
      paste0(
        "You have included ", length(next_steps), " next steps. ",
        "Consider adding more (3-5 total) for a more complete implementation of the Peak-End Rule."
      )
    } else {
      paste0(
        "You have included ", length(next_steps), " next steps. ",
        "Consider focusing on the 3-5 most important ones for clarity."
      )
    }
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
    previous_bias = bias_mitigations,
    previous_interaction = interaction_principles %||% NA_character_,
    previous_layout = layout %||% NA_character_,
    previous_concepts = concepts %||% NA_character_,
    previous_accessibility = accessibility %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  cli::cli_h1("Stage 5 (Validate) completed")

  cli::cli_h2("Summary")
  cli::cli_bullets(c("i" = summary_suggestion))

  cli::cli_h2("Collaboration")
  cli::cli_bullets(c("i" = collaboration_suggestion))

  cli::cli_h2("Next Steps")
  cli::cli_bullets(c("i" = next_steps_suggestion))

  cli::cli_h2("Recommended Visualization Components")
  cli::cli_bullets(
    c(
      "*" = "A summary card with key insights",
      "*" = "Collaboration features like annotations or comments",
      "*" = if (!is.null(next_steps)) {
        "Clear next steps section implemented"
      } else {
        "A clear 'next steps' section aligned with the Peak-End Rule"
      }
    )
  )

  return(result)
}
