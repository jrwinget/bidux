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
#' @param quiet Logical indicating whether to suppress informational messages.
#'        If NULL, uses getOption("bidux.quiet", FALSE).
#'
#' @return A tibble containing the documented information for the "Interpret"
#'         stage.
#'
#' @examples
#' # Recommended: use new_data_story() with flat API
#' interpret_result <- bid_interpret(
#'   central_question = "What drives the decline in user engagement?",
#'   data_story = new_data_story(
#'     hook = "Declining trend in engagement",
#'     context = "Previous high engagement levels",
#'     tension = "Unexpected drop",
#'     resolution = "Investigate new UI changes"
#'   )
#' )
#'
#' # With user personas (using data.frame)
#' interpret_personas <- bid_interpret(
#'   central_question = "How can we improve data discovery?",
#'   data_story = new_data_story(
#'     hook = "Users are missing key insights",
#'     context = "Critical data is available but overlooked",
#'     tension = "Time-sensitive decisions are delayed",
#'     resolution = "Highlight key metrics more effectively",
#'     audience = "Data analysts and executives"
#'   ),
#'   user_personas = data.frame(
#'     name = c("Sara, Data Analyst", "Marcus, Executive"),
#'     goals = c(
#'       "Needs to quickly find patterns in data",
#'       "Wants high-level insights at a glance"
#'     ),
#'     pain_points = c(
#'       "Gets overwhelmed by too many visualizations",
#'       "Limited time to analyze detailed reports"
#'     ),
#'     technical_level = c("advanced", "beginner"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#'
#' summary(interpret_personas)
#'
#' # Legacy list format still works (with deprecation warning)
#' \dontrun{
#' interpret_legacy <- bid_interpret(
#'   central_question = "How can we improve UX?",
#'   data_story = list(
#'     hook = "Users struggling",
#'     context = "Dashboard complexity",
#'     tension = "High abandonment rate",
#'     resolution = "Simplify interface"
#'   )
#' )
#' }
#'
#' @export
bid_interpret <- function(
    previous_stage = NULL,
    central_question,
    data_story = NULL,
    user_personas = NULL,
    quiet = NULL) {
  # enhanced parameter validation for data_story
  if (!is.null(data_story)) {
    if (inherits(data_story, "bid_data_story")) {
      # new s3 class - validate structure
      if (!validate_data_story(data_story)) {
        cli::cli_abort(standard_error_msg(
          "Invalid bid_data_story object",
          suggestions = "Use new_data_story() constructor to create valid objects"
        ))
      }
    } else if (is.list(data_story)) {
      # legacy list format - migrate to new s3 class with deprecation warning
      cli::cli_warn(c(
        "!" = "Using deprecated list format for data_story parameter",
        "i" = "Please use new_data_story() constructor for new code",
        "i" = "Legacy format will be automatically migrated"
      ))
      data_story <- migrate_data_story(data_story)
    } else {
      cli::cli_abort(standard_error_msg(
        "data_story must be a bid_data_story object or list",
        context = glue::glue("You provided: {class(data_story)[1]}"),
        suggestions = c(
          "Use new_data_story() constructor",
          "Or provide a list with context, variables, relationships"
        )
      ))
    }
  }


  # standardized parameter validation for previous_stage if provided
  if (!is.null(previous_stage)) {
    validate_bid_stage_params(
      previous_stage,
      "Interpret",
      list()
    )
  }

  # enhanced parameter validation for user_personas
  if (!is.null(user_personas)) {
    if (inherits(user_personas, "bid_user_personas")) {
      # new s3 class - validate structure
      if (!validate_user_personas(user_personas)) {
        cli::cli_abort(standard_error_msg(
          "Invalid bid_user_personas object",
          suggestions = "Use new_user_personas() constructor to create valid objects"
        ))
      }
    } else if (is.data.frame(user_personas)) {
      # data.frame provided directly - convert to bid_user_personas
      user_personas <- new_user_personas(user_personas)
    } else if (is.list(user_personas)) {
      # legacy list format - migrate to new s3 class with deprecation warning
      cli::cli_warn(c(
        "!" = "Using deprecated list format for user_personas parameter",
        "i" = "Please use new_user_personas() constructor for new code",
        "i" = "Legacy format will be automatically migrated"
      ))
      user_personas <- migrate_user_personas(user_personas)
    } else {
      cli::cli_abort(standard_error_msg(
        "user_personas must be a bid_user_personas object, data.frame, or list",
        context = glue::glue("You provided: {class(user_personas)[1]}"),
        suggestions = c(
          "Use new_user_personas() constructor",
          "Or provide a data.frame with required columns",
          "Or provide a list of persona objects"
        )
      ))
    }
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

      bid_alert_info(
        paste0("Suggested central question: ", central_question),
        quiet = quiet
      )
    } else if (
      !is.null(previous_stage) && (
        previous_stage$stage[1] == "Structure" ||
          previous_stage$stage[1] == "Anticipate"
      )
    ) {
      central_question <- paste0(
        "How can we refine our understanding of user needs for this dashboard?"
      )
      bid_alert_info(
        paste0("Suggested central question: ", central_question),
        quiet = quiet
      )
    } else if (is.null(previous_stage)) {
      central_question <- paste0(
        "How can we improve the user experience of the dashboard?"
      )
      bid_alert_info(
        paste0("Suggested central question: ", central_question),
        quiet = quiet
      )
    }
  }

  if (is.null(data_story)) {
    # create structured data story using s3 class
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
        # create context from problem and evidence
        context_text <- paste0("Users are experiencing problems with ", problem)

        # build variables list from extracted data
        variables_list <- list(
          problem = problem,
          evidence = if (!is.na(evidence)) evidence else "Interface usability issue",
          theory = if (!is.na(theory)) theory else "General usability principles",
          hook = paste0("Current interface challenges are affecting user success")
        )

        # build relationships based on theory
        relationships_list <- list(
          problem_to_solution = paste0(
            "We need to redesign the interface",
            if (!is.na(theory)) paste0(" using principles from ", theory) else "",
            " to address this problem."
          ),
          user_friction = paste0(
            "This is creating friction in the user experience",
            if (!is.na(theory)) paste0(" related to ", theory) else ""
          )
        )

        # add metadata with legacy fields for backward compatibility
        metadata_list <- list(
          audience = if (!is.na(target_audience)) target_audience else NULL,
          visual_approach = if (!is.na(theory)) {
            theory_lower <- tolower(theory)
            if (grepl("cognitive load", theory_lower)) {
              "Simplified visualizations with reduced clutter"
            } else if (grepl("hick", theory_lower)) {
              "Clear hierarchy of choices with progressive disclosure"
            } else if (grepl("visual hierarch", theory_lower)) {
              "Strong visual hierarchy using size, color, and positioning"
            } else {
              "Clean, focused visualizations with clear purpose"
            }
          } else {
            "Clean, focused visualizations with clear purpose"
          }
        )

        data_story <- new_data_story(
          context = context_text,
          variables = variables_list,
          relationships = relationships_list,
          metadata = metadata_list
        )
      } else {
        # fallback for missing problem data
        data_story <- new_data_story(
          context = "Dashboard users may not be getting maximum value from current interface",
          variables = list(
            user_challenge = "Suboptimal user experience",
            improvement_area = "Interface design",
            hook = "Users may be experiencing interface challenges"
          ),
          relationships = list(
            solution_path = "Redesign interface using behavioral science principles"
          )
        )
      }

      bid_alert_info(
        "Suggested data story elements based on previous stage information",
        quiet = quiet
      )
    } else if (
      !is.null(previous_stage) && (
        previous_stage$stage[1] == "Structure" ||
          previous_stage$stage[1] == "Anticipate"
      )
    ) {
      # iteration cycle data story
      data_story <- new_data_story(
        context = "We need to revisit our understanding of user needs",
        variables = list(
          design_status = "The current design may need refinement",
          user_understanding = "User needs may have evolved or been incompletely understood",
          hook = "There may be gaps in our current understanding"
        ),
        relationships = list(
          improvement_cycle = "Gather additional user feedback and refine our interpretation"
        )
      )

      bid_alert_info("Suggested generic data story for iteration cycle", quiet = quiet)
    } else if (is.null(previous_stage)) {
      # new project data story
      data_story <- new_data_story(
        context = "Dashboard users may not be getting maximum value from current interface",
        variables = list(
          user_challenge = "Users may be missing important insights or spending too much time",
          opportunity = "Current interface could be improved for better user experience",
          hook = "Users deserve a better interface experience"
        ),
        relationships = list(
          solution_approach = "Redesign interface using behavioral science principles"
        )
      )

      bid_alert_info(
        "Suggested generic data story for new dashboard design",
        quiet = quiet
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
  } else if (nchar(central_question) > 100) {
    "Consider simplifying your central question for more focus."
  } else if (nchar(central_question) < 20) {
    "Your central question might benefit from more specificity."
  } else {
    "Your central question is appropriately scoped."
  }

  if (is.null(user_personas)) {
    # try to extract audience from data_story or previous_stage
    audience <- NULL

    if (!is.null(data_story)) {
      if (inherits(data_story, "bid_data_story")) {
        audience <- safe_list_access(data_story$metadata, "audience", NULL)
      } else if (is.list(data_story) && "audience" %in% names(data_story)) {
        audience <- data_story$audience
      }
    }

    # fallback to previous_stage if no audience in data_story
    if (is.null(audience) || is.na(audience)) {
      audience <- get_audience_from_previous(previous_stage)
    }

    # generate persona from audience using DRY helper
    if (!is.null(audience) && !is.na(audience) && nchar(trimws(audience)) > 0) {
      user_personas <- generate_persona_from_audience(audience)

      if (!is.null(user_personas)) {
        bid_alert_info(
          paste0(
            "Created user persona '",
            user_personas$name[1],
            "' based on audience information"
          ),
          quiet = quiet
        )
      }
    }
  }

  persona_suggestion <- if (!is.null(user_personas)) {
    persona_count <- if (inherits(user_personas, "bid_user_personas")) {
      nrow(user_personas)
    } else if (is.list(user_personas)) {
      length(user_personas)
    } else {
      0
    }

    if (persona_count > 0) {
      paste0(
        "You've defined ",
        persona_count,
        " persona(s). ",
        "Ensure your design addresses the specific needs of each."
      )
    } else {
      "Consider defining specific user personas to better target your design."
    }
  } else {
    "Consider defining specific user personas to better target your design."
  }

  suggestions <- paste(
    story_suggestion,
    question_suggestion,
    persona_suggestion
  )

  # extract fields from data_story s3 object or legacy format
  audience <- if (!is.null(data_story)) {
    if (inherits(data_story, "bid_data_story")) {
      # new s3 class - check metadata
      safe_list_access(data_story$metadata, "audience", NA_character_)
    } else if (is.list(data_story) && "audience" %in% names(data_story)) {
      # legacy list format
      data_story$audience %||% NA_character_
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }

  metrics <- if (!is.null(data_story)) {
    metrics_value <- if (inherits(data_story, "bid_data_story")) {
      safe_list_access(data_story$metadata, "metrics", NULL)
    } else if (is.list(data_story)) {
      safe_list_access(data_story, "metrics", NULL)
    } else {
      NULL
    }

    if (!is.null(metrics_value)) {
      if (is.character(metrics_value)) {
        paste(metrics_value, collapse = ", ")
      } else if (is.numeric(metrics_value)) {
        paste(as.character(metrics_value), collapse = ", ")
      } else if (is.list(metrics_value)) {
        paste(unlist(lapply(metrics_value, as.character)), collapse = ", ")
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }

  visual_approach <- if (!is.null(data_story)) {
    if (inherits(data_story, "bid_data_story")) {
      # new s3 class - check metadata
      safe_list_access(data_story$metadata, "visual_approach", NA_character_)
    } else if (is.list(data_story) && "visual_approach" %in% names(data_story)) {
      # legacy list format
      data_story$visual_approach %||% NA_character_
    } else {
      NA_character_
    }
  } else {
    NA_character_
  }

  personas_formatted <- if (is.null(user_personas)) {
    NA_character_
  } else {
    tryCatch(
      {
        # jsonlite handles both data frames and lists natively
        # dataframe = "rows" converts tibbles to array of objects
        jsonlite::toJSON(user_personas, auto_unbox = TRUE, dataframe = "rows")
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
    },
    quiet = quiet
  )

  return(result)
}
