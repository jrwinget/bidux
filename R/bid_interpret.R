#' Document User Interpretation Stage in BID Framework
#'
#' @description
#' This function documents the interpretation of user needs, capturing the
#' central question and the data storytelling narrative. It represents stage 1
#' in the BID framework.
#'
#' @param previous_stage Optional tibble or list output from an earlier BID
#'        stage function. Since Interpret is the first stage in the BID
#'        framework, this is typically NULL but can accept previous stage output
#'        in some iteration scenarios.
#' @param central_question Required. A character string representing the main
#'        question to be answered. If NULL, will be suggested based on previous
#'        stage information.
#' @param data_story A list containing elements such as \code{hook},
#'        \code{context}, \code{tension}, \code{resolution}, and optionally
#'        \code{audience}, \code{metrics}, and \code{visual_approach}. If NULL,
#'        elements will be suggested based on previous stage.
#' @param user_personas Optional list of user personas to consider in the
#'        design.
#'
#' @return A tibble containing the documented information for the "Interpret"
#'         stage.
#'
#' @examples
#' # Basic usage
#' interpret_result <- bid_interpret(
#'   central_question = "What drives the decline in user engagement?",
#'   data_story = list(
#'     hook = "Declining trend in engagement",
#'     context = "Previous high engagement levels",
#'     tension = "Unexpected drop",
#'     resolution = "Investigate new UI changes"
#'   )
#' )
#'
#' # With user personas
#' interpret_personas <- bid_interpret(
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
#' summary(interpret_personas)
#'
#' @export
bid_interpret <- function(
    previous_stage = NULL,
    central_question,
    data_story = NULL,
    user_personas = NULL) {
  # parameter validation
  if (!is.null(data_story) && !is.list(data_story)) {
    cli::cli_abort("'data_story' must be a list")
  }


  # standardized parameter validation for previous_stage if provided
  if (!is.null(previous_stage)) {
    validate_bid_stage_params(
      previous_stage,
      "Interpret",
      list()
    )
  }

  if (!is.null(user_personas)) {
    validate_user_personas(user_personas)
  }

  if (is.null(central_question)) {
    if (!is.null(previous_stage) && previous_stage$stage[1] == "Notice") {
      stage_data <- extract_stage_data(previous_stage, c("problem", "theory"))
      problem <- stage_data$problem
      theory <- stage_data$theory

      if (!is.na(problem)) {
        problem_lower <- tolower(problem)

        if (grepl("struggl|difficult|hard|confus|unclear", problem_lower)) {
          central_question <- paste0(
            "How can we simplify the interface to address ",
            problem,
            "?"
          )
        } else if (grepl("find|locat|discov", problem_lower)) {
          central_question <- paste0(
            "How can we make it easier for users to find important information?"
          )
        } else if (grepl("slow|delay|time", problem_lower)) {
          central_question <- paste0(
            "How can we improve the speed and efficiency of user interactions?"
          )
        } else if (grepl("overwhelm|too many|excess", problem_lower)) {
          central_question <- paste(
            "How can we reduce cognitive load and",
            "help users focus on what matters?"
          )
        } else {
          central_question <- paste0(
            "How can we address the issue where ",
            problem,
            "?"
          )
        }

        if (!is.na(theory)) {
          theory_lower <- tolower(theory)

          if (grepl("cognitive load", theory_lower)) {
            central_question <- paste0(
              "How can we reduce cognitive load to address ",
              problem,
              "?"
            )
          } else if (grepl("hick", theory_lower)) {
            central_question <- paste0(
              "How can we simplify choices to address ",
              problem,
              "?"
            )
          } else if (grepl("visual hierarch", theory_lower)) {
            central_question <- paste0(
              "How can we improve visual hierarchy to address ",
              problem,
              "?"
            )
          }
        }
      } else {
        central_question <- paste0(
          "How can we improve the user experience of the dashboard?"
        )
      }

      cli::cli_alert_info(
        paste0("Suggested central question: ", central_question)
      )
    } else if (
      !is.null(previous_stage) &&
        (previous_stage$stage[1] == "Structure" ||
          previous_stage$stage[1] == "Anticipate")
    ) {
      central_question <- paste0(
        "How can we refine our understanding of user needs for this dashboard?"
      )
      cli::cli_alert_info(
        paste0("Suggested central question: ", central_question)
      )
    } else if (is.null(previous_stage)) {
      central_question <- paste0(
        "How can we improve the user experience of the dashboard?"
      )
      cli::cli_alert_info(
        paste0("Suggested central question: ", central_question)
      )
    }
  }

  if (is.null(data_story)) {
    data_story <- list()

    if (!is.null(previous_stage) && previous_stage$stage[1] == "Notice") {
      stage_data <- extract_stage_data(
        previous_stage,
        c("problem", "theory", "evidence", "target_audience")
      )
      problem <- stage_data$problem
      theory <- stage_data$theory
      evidence <- stage_data$evidence
      target_audience <- stage_data$target_audience

      if (!is.na(problem)) {
        # hook
        data_story$hook <- paste0(
          "Users are experiencing problems with ",
          problem
        )

        # context
        if (!is.na(evidence)) {
          data_story$context <- paste0("According to our evidence: ", evidence)
        } else {
          data_story$context <- "
            Our current interface may be contributing to this issue.
          "
        }

        # tension
        data_story$tension <- paste0(
          "This is creating friction in the user experience",
          if (!is.na(theory)) paste0(" related to ", theory) else ""
        )

        # resolution
        data_story$resolution <- paste0(
          "We need to redesign the interface",
          if (!is.na(theory)) {
            paste0(" using principles from ", theory)
          } else {
            ""
          },
          " to address this problem."
        )

        # audience
        if (!is.na(target_audience)) {
          data_story$audience <- target_audience
        }

        # visual approach
        if (!is.na(theory)) {
          theory_lower <- tolower(theory)

          if (grepl("cognitive load", theory_lower)) {
            data_story$visual_approach <- "
              Simplified visualizations with reduced clutter
            "
          } else if (grepl("hick", theory_lower)) {
            data_story$visual_approach <- "
              Clear hierarchy of choices with progressive disclosure
            "
          } else if (grepl("visual hierarch", theory_lower)) {
            data_story$visual_approach <- "
              Strong visual hierarchy using size, color, and positioning
            "
          } else {
            data_story$visual_approach <- "
              Clean, focused visualizations with clear purpose
            "
          }
        }
      } else {
        data_story <- list(
          hook = "Dashboard users may not be getting maximum value",
          context = "
            Current interface could be improved for better user experience
          ",
          tension = "
            Users may be missing important insights or spending too much time
          ",
          resolution = "Redesign interface using behavioral science principles"
        )
      }

      cli::cli_alert_info(
        "Suggested data story elements based on previous stage information"
      )
    } else if (
      !is.null(previous_stage) &&
        (previous_stage$stage[1] == "Structure" ||
          previous_stage$stage[1] == "Anticipate")
    ) {
      data_story <- list(
        hook = "We need to revisit our understanding of user needs",
        context = "The current design may need refinement",
        tension = "User needs may have evolved or been incompletely understood",
        resolution = "
          Gather additional user feedback and refine our interpretation
        "
      )

      cli::cli_alert_info("Suggested generic data story for iteration cycle")
    } else if (is.null(previous_stage)) {
      data_story <- list(
        hook = "Dashboard users may not be getting maximum value",
        context = "Current interface could be improved for better user experience",
        tension = "Users may be missing important insights or spending too much time",
        resolution = "Redesign interface using behavioral science principles"
      )

      cli::cli_alert_info(
        "Suggested generic data story for new dashboard design"
      )
    }
  }

  required_story_elements <- c("hook", "context", "tension", "resolution")
  provided_elements <- required_story_elements %in% names(data_story)
  story_completeness <- sum(provided_elements) / length(required_story_elements)

  if (story_completeness < 0.5) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- cli::format_inline(
      paste(
        "Your data story is incomplete ({round(story_completeness * 100)}%).",
        "Consider adding these missing elements:",
        "{paste(missing_elements, collapse = ', ')}."
      )
    )
  } else if (story_completeness < 0.75) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- cli::format_inline(
      paste(
        "Your data story is taking shape ({round(story_completeness * 100)}%).",
        "Consider adding: {paste(missing_elements, collapse = ', ')}."
      )
    )
  } else if (story_completeness < 1) {
    missing_elements <- required_story_elements[!provided_elements]
    story_suggestion <- cli::format_inline(
      paste(
        "Your data story is almost complete",
        "({round(story_completeness * 100)}%). Consider adding:",
        "{paste(missing_elements, collapse = ', ')}."
      )
    )
  } else {
    story_suggestion <- paste(
      "Your data story has all key elements. Focus on making each component",
      "compelling and relevant."
    )
  }

  question_suggestion <- if (
    is.na(central_question) ||
      is.null(central_question)
  ) {
    "Please provide a central question to guide your dashboard design."
  } else if (stringr::str_length(central_question) > 100) {
    "Consider simplifying your central question for more focus."
  } else if (stringr::str_length(central_question) < 20) {
    "Your central question might benefit from more specificity."
  } else {
    "Your central question is appropriately scoped."
  }

  if (is.null(user_personas)) {
    audience <- NULL

    if (
      # try to get audience from data_story
      !is.null(data_story) &&
        "audience" %in% names(data_story) &&
        !is.na(data_story$audience)
    ) {
      audience <- data_story$audience
    } else if (
      # try to get audience from previous_stage
      !is.null(previous_stage) &&
        previous_stage$stage[1] == "Notice" &&
        "target_audience" %in% names(previous_stage) &&
        !is.na(previous_stage$target_audience[1])
    ) {
      audience <- previous_stage$target_audience[1]
    }

    if (!is.null(audience) && !is.na(audience)) {
      audience_lower <- tolower(audience)

      # nuanced user type
      user_type <- if (
        grepl(
          "analyst|data scientist|technical|developer|engineer",
          audience_lower
        )
      ) {
        "Data Analyst"
      } else if (
        grepl(
          "executive|manager|director|leadership|ceo|cfo|cto|vp",
          audience_lower
        )
      ) {
        "Executive"
      } else if (grepl("market|advertis|campaign|brand", audience_lower)) {
        "Marketing Professional"
      } else if (grepl("sales|account|business develop", audience_lower)) {
        "Sales Representative"
      } else if (grepl("customer|client|user|consumer", audience_lower)) {
        "End User"
      } else {
        "Dashboard User"
      }

      # technical level
      technical_level <- if (
        grepl(
          "analyst|data scientist|technical|developer|engineer",
          audience_lower
        )
      ) {
        "Advanced"
      } else if (grepl("executive|leadership|ceo|cfo", audience_lower)) {
        "Basic"
      } else {
        "Intermediate"
      }

      # specific goals based on role
      goals <- if (grepl("executive|leadership", audience_lower)) {
        "Needs quick insights for strategic decisions"
      } else if (grepl("analyst|data", audience_lower)) {
        "Needs to explore data in depth to find patterns"
      } else if (grepl("market", audience_lower)) {
        "Needs to track campaign performance metrics"
      } else if (grepl("sales", audience_lower)) {
        "Needs to identify sales opportunities and track performance"
      } else {
        "Needs to extract relevant insights efficiently"
      }

      # specific pain points based on role
      pain_points <- if (grepl("executive|leadership", audience_lower)) {
        "Limited time to analyze detailed reports"
      } else if (grepl("analyst|data", audience_lower)) {
        "Frustrated by interfaces that limit data exploration"
      } else if (grepl("market", audience_lower)) {
        "Struggles to connect multiple data sources for complete picture"
      } else if (grepl("sales", audience_lower)) {
        "Needs mobile-friendly dashboards for client meetings"
      } else {
        "Gets overwhelmed by complex dashboards with too many options"
      }

      # complete persona
      user_personas <- list(
        list(
          name = paste(user_type, "Persona"),
          goals = goals,
          pain_points = pain_points,
          technical_level = technical_level
        )
      )

      cli::cli_alert_info(
        paste0(
          "Created user persona '",
          user_personas[[1]]$name,
          "' based on audience information"
        )
      )
    }
  }

  persona_suggestion <- if (
    !is.null(user_personas) &&
      length(user_personas) > 0
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

  suggestions <- paste(
    story_suggestion,
    question_suggestion,
    persona_suggestion
  )

  audience <- if (!is.null(data_story) && "audience" %in% names(data_story)) {
    data_story$audience %||% NA_character_
  } else {
    NA_character_
  }

  metrics <- if (
    !is.null(data_story) &&
      "metrics" %in% names(data_story) &&
      !is.null(data_story$metrics)
  ) {
    if (is.character(data_story$metrics)) {
      paste(data_story$metrics, collapse = ", ")
    } else if (is.numeric(data_story$metrics)) {
      paste(as.character(data_story$metrics), collapse = ", ")
    } else if (is.list(data_story$metrics)) {
      paste(unlist(lapply(data_story$metrics, as.character)), collapse = ", ")
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }

  visual_approach <- if (
    !is.null(data_story) &&
      "visual_approach" %in% names(data_story)
  ) {
    data_story$visual_approach %||% NA_character_
  } else {
    NA_character_
  }

  personas_formatted <- if (
    !is.null(user_personas) &&
      length(user_personas) > 0
  ) {
    tryCatch(
      {
        jsonlite::toJSON(user_personas)
      },
      error = function(e) {
        cli::cli_warn(c(
          "Could not convert user_personas to JSON format",
          "i" = "Using default NA value instead",
          "x" = paste0(e$message)
        ))
        NA_character_
      }
    )
  } else {
    NA_character_
  }

  # normalize previous stage to ensure field name consistency
  normalized_previous <- if (!is.null(previous_stage)) {
    normalize_previous_stage(previous_stage)
  } else {
    NULL
  }

  result_data <- tibble::tibble(
    stage = "Interpret",
    central_question = central_question,
    hook = safe_data_story_access(data_story, "hook"),
    context = safe_data_story_access(data_story, "context"),
    tension = safe_data_story_access(data_story, "tension"),
    resolution = safe_data_story_access(data_story, "resolution"),
    audience = audience,
    metrics = metrics,
    visual_approach = visual_approach,
    personas = personas_formatted,
    previous_problem = safe_column_access(normalized_previous, "problem"),
    previous_theory = safe_column_access(normalized_previous, "theory"),
    previous_audience = safe_column_access(
      normalized_previous,
      "target_audience"
    ),
    suggestions = suggestions,
    timestamp = .now()
  )

  metadata <- get_stage_metadata(
    1,
    list(
      has_central_question = !is.null(central_question),
      story_completeness = story_completeness,
      personas_count = if (!is.null(user_personas)) {
        length(user_personas)
      } else {
        0
      },
      auto_generated_question = is.null(central_question),
      auto_generated_story = is.null(data_story)
    )
  )

  result <- bid_stage("Interpret", result_data, metadata)

  bid_message(
    "Stage 1 (Interpret) completed.",
    paste0(
      "Central question: ",
      if (nchar(central_question) > 60) {
        paste0(substring(central_question, 1, 60), "...")
      } else {
        central_question
      }
    ),
    story_suggestion,
    question_suggestion,
    if (!is.null(user_personas) && length(user_personas) > 0) {
      paste0("User personas: ", length(user_personas), " defined")
    } else {
      "No user personas defined"
    }
  )

  return(result)
}
