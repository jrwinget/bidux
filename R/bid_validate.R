#' Document User Validation Stage in BID Framework
#'
#' @description
#' This function documents the validation stage, where the user tests and
#' refines the dashboard. It represents stage 5 in the BID framework.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#'        function.
#' @param summary_panel A character string describing the final summary panel
#'        or key insight presentation.
#' @param collaboration A character string describing how the dashboard enables
#'        collaboration and sharing.
#' @param next_steps A character vector or string describing recommended next
#'        steps for implementation and iteration.
#'
#' @return A tibble containing the documented information for the "Validate"
#'         stage.
#'
#' @examples
#' structure_input <- bid_notice(
#'   problem  = "Issue with dropdown menus",
#'   evidence = "User testing indicated delays"
#' ) |>
#'   bid_interpret(
#'     central_question = "How can we improve selection efficiency?",
#'     data_story = list(
#'       hook = "Too many options",
#'       context = "Excessive choices",
#'       tension = "User frustration",
#'       resolution = "Simplify menu"
#'     )
#'   )
#'
#' structure_result <- bid_structure(
#'   previous_stage = structure_input,
#'   layout         = "dual_process",
#'   concepts       = c("Principle of Proximity", "Default Effect")
#' )
#'
#' anticipate <- bid_anticipate(
#'   previous_stage = structure_result,
#'   bias_mitigations = list(
#'     anchoring = "Provide reference points",
#'     framing   = "Use gain-framed messaging"
#'   )
#' )
#'
#' bid_validate(
#'   previous_stage = anticipate,
#'   summary_panel = "Clear summary of key insights with action items",
#'   collaboration = "Team annotation and sharing features",
#'   next_steps = c(
#'     "Conduct user testing with target audience",
#'     "Implement accessibility improvements",
#'     "Add mobile responsiveness"
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

  if (is.null(summary_panel)) {
    summary_panel <- generate_summary_panel_suggestion(previous_stage)
    cli::cli_alert_info(paste0("Suggested summary panel: ", summary_panel))
  }

  if (is.null(collaboration)) {
    collaboration <- generate_collaboration_suggestion(previous_stage)
    cli::cli_alert_info(paste0(
      "Suggested collaboration features: ",
      collaboration
    ))
  }

  if (is.null(next_steps)) {
    next_steps <- generate_next_steps_suggestion(previous_stage)
    cli::cli_alert_info("Suggested next steps:")
    for (step in next_steps) {
      cli::cli_li(step)
    }
  }

  next_steps_formatted <- format_next_steps(next_steps)

  suggestions <- generate_validation_suggestions(
    summary_panel,
    collaboration,
    next_steps,
    previous_stage
  )

  previous_info <- extract_previous_stage_info(previous_stage)

  result <- tibble::tibble(
    stage = "Validate",
    summary_panel = summary_panel %||% NA_character_,
    collaboration = collaboration %||% NA_character_,
    next_steps = next_steps_formatted,
    previous_bias = previous_info$bias %||% NA_character_,
    previous_interaction = previous_info$interaction %||% NA_character_,
    previous_layout = previous_info$layout %||% NA_character_,
    previous_concepts = previous_info$concepts %||% NA_character_,
    previous_accessibility = previous_info$accessibility %||% NA_character_,
    previous_central_question = previous_info$central_question %||%
      NA_character_,
    previous_problem = previous_info$problem %||% NA_character_,
    previous_theory = previous_info$theory %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 5 (Validate) completed.",
    paste0("Summary panel: ", truncate_text(summary_panel, 50)),
    paste0("Collaboration: ", truncate_text(collaboration, 50)),
    paste0(
      "Next steps: ",
      length(parse_next_steps(next_steps_formatted)),
      " items defined"
    ),
    suggestions
  )

  return(result)
}

generate_summary_panel_suggestion <- function(previous_stage) {
  central_question <- safe_column_access(previous_stage, "central_question", "")
  problem <- safe_column_access(previous_stage, "problem", "")
  theory <- safe_column_access(previous_stage, "theory", "")

  base_suggestion <- "Dashboard provides clear summary of key insights with actionable recommendations"

  if (central_question != "" && !is.na(central_question)) {
    if (grepl("simplify|reduce|minimize", tolower(central_question))) {
      return(
        "Simplified summary panel highlighting the most critical insights to reduce cognitive load"
      )
    } else if (grepl("compare|versus|against", tolower(central_question))) {
      return(
        "Comparative summary showing key differences and their implications"
      )
    } else if (grepl("trend|time|change", tolower(central_question))) {
      return(
        "Time-based summary panel showing trends and forecasts with clear directional indicators"
      )
    }
  }

  if (problem != "" && !is.na(problem)) {
    if (grepl("complex|overwhelm", tolower(problem))) {
      return(
        "Clean, focused summary panel that distills complex information into digestible insights"
      )
    } else if (grepl("find|search|locate", tolower(problem))) {
      return(
        "Summary panel with clear navigation paths to detailed information"
      )
    } else if (grepl("mobile|phone", tolower(problem))) {
      return(
        "Mobile-optimized summary panel with touch-friendly interaction elements"
      )
    }
  }

  if (theory != "" && !is.na(theory)) {
    if (grepl("cognitive load", tolower(theory))) {
      return("Streamlined summary panel designed to minimize cognitive burden")
    } else if (grepl("visual", tolower(theory))) {
      return(
        "Visually hierarchical summary panel with clear information organization"
      )
    }
  }

  return(base_suggestion)
}

generate_collaboration_suggestion <- function(previous_stage) {
  audience_fields <- c("audience", "target_audience", "previous_audience")
  audience <- ""

  for (field in audience_fields) {
    audience_value <- safe_column_access(previous_stage, field, "")
    if (audience_value != "" && !is.na(audience_value)) {
      audience <- audience_value
      break
    }
  }

  base_suggestion <- "Enable team sharing and collaborative decision-making features"

  if (audience != "" && !is.na(audience)) {
    audience_lower <- tolower(audience)

    if (grepl("executive|leadership|manager", audience_lower)) {
      return(
        "Executive-focused collaboration with summary sharing and decision tracking"
      )
    } else if (grepl("analyst|technical|data", audience_lower)) {
      return(
        "Advanced collaboration tools including data export, annotation, and methodology sharing"
      )
    } else if (grepl("team|group|multiple", audience_lower)) {
      return(
        "Multi-user collaboration with role-based permissions and shared annotations"
      )
    } else if (grepl("client|customer|external", audience_lower)) {
      return(
        "Client-friendly sharing options with controlled access and presentation modes"
      )
    }
  }

  concepts_field <- safe_column_access(previous_stage, "concepts", "")
  if (grepl("cooperation", tolower(concepts_field))) {
    return(
      "Structured collaboration workflows that enhance group decision-making"
    )
  }

  return(base_suggestion)
}

generate_next_steps_suggestion <- function(previous_stage) {
  stage_name <- previous_stage$stage[1]
  next_steps <- character(0)

  next_steps <- c(
    next_steps,
    "Conduct user testing with target audience to validate design decisions"
  )

  if (stage_name == "Anticipate") {
    next_steps <- c(
      next_steps,
      "Implement bias mitigation strategies identified in the Anticipate stage",
      "Monitor user behavior patterns to validate bias assumptions"
    )
  } else if (stage_name == "Structure") {
    next_steps <- c(
      next_steps,
      "Implement the structured layout with proper visual hierarchy",
      "Test accessibility features across different devices and assistive technologies"
    )
  } else if (stage_name == "Interpret") {
    next_steps <- c(
      next_steps,
      "Validate data storytelling approach with stakeholders",
      "Refine dashboard based on central question effectiveness"
    )
  }

  problem <- safe_column_access(previous_stage, "problem", "")
  if (problem != "" && !is.na(problem)) {
    if (grepl("mobile|phone", tolower(problem))) {
      next_steps <- c(
        next_steps,
        "Optimize mobile experience and responsive design"
      )
    }
    if (grepl("performance|slow", tolower(problem))) {
      next_steps <- c(
        next_steps,
        "Conduct performance testing and optimization"
      )
    }
    if (grepl("accessibility", tolower(problem))) {
      next_steps <- c(
        next_steps,
        "Complete comprehensive accessibility audit and remediation"
      )
    }
  }

  collaboration <- safe_column_access(previous_stage, "collaboration", "")
  if (collaboration != "" && !is.na(collaboration)) {
    next_steps <- c(
      next_steps,
      "Test collaboration features with multi-user scenarios"
    )
  }

  next_steps <- c(
    next_steps,
    "Document successful patterns and lessons learned for future projects",
    "Plan iterative improvements based on user feedback and analytics"
  )

  return(next_steps)
}


generate_validation_suggestions <- function(
    summary_panel,
    collaboration,
    next_steps,
    previous_stage) {
  suggestions <- character(0)

  if (!is.null(summary_panel) && nchar(summary_panel) > 0) {
    if (nchar(summary_panel) > 150) {
      suggestions <- c(
        suggestions,
        "Consider simplifying the summary panel description for clarity"
      )
    }
    if (!grepl("insight|action|recommendation", tolower(summary_panel))) {
      suggestions <- c(
        suggestions,
        "Ensure summary panel includes actionable insights"
      )
    }
  } else {
    suggestions <- c(
      suggestions,
      "Define a clear summary panel to help users extract key insights"
    )
  }

  if (!is.null(collaboration) && nchar(collaboration) > 0) {
    if (!grepl("share|export|team|collaborate", tolower(collaboration))) {
      suggestions <- c(
        suggestions,
        "Consider adding specific sharing or collaboration mechanisms"
      )
    }
  } else {
    suggestions <- c(
      suggestions,
      "Consider adding collaboration features to enable team decision-making"
    )
  }

  if (!is.null(next_steps)) {
    steps_list <- parse_next_steps(format_next_steps(next_steps))
    if (length(steps_list) < 3) {
      suggestions <- c(
        suggestions,
        "Consider adding more specific next steps for implementation"
      )
    }
    if (!any(grepl("test|user|feedback", tolower(steps_list)))) {
      suggestions <- c(suggestions, "Include user testing in your next steps")
    }
  } else {
    suggestions <- c(
      suggestions,
      "Define specific next steps for dashboard implementation and iteration"
    )
  }

  stage_name <- previous_stage$stage[1]
  if (stage_name == "Anticipate") {
    bias_field <- safe_column_access(previous_stage, "bias_mitigations", "")
    if (bias_field == "" || is.na(bias_field)) {
      suggestions <- c(
        suggestions,
        "Ensure bias mitigation strategies from Anticipate stage are documented"
      )
    }
  }

  if (length(suggestions) == 0) {
    suggestions <- paste(
      "Validation stage is well-defined.",
      "Focus on implementation and user testing."
    )
  }

  return(paste(suggestions, collapse = " "))
}

extract_previous_stage_info <- function(previous_stage) {
  info <- list()

  stage_name <- previous_stage$stage[1]

  if (stage_name == "Anticipate") {
    info$bias <- safe_column_access(
      previous_stage,
      "bias_mitigations",
      NA_character_
    )
    info$interaction <- safe_column_access(
      previous_stage,
      "interaction_principles",
      NA_character_
    )

    # get information from any previous stages
    info$layout <- safe_column_access(
      previous_stage,
      "previous_layout",
      NA_character_
    )
    info$concepts <- safe_column_access(
      previous_stage,
      "previous_concepts",
      NA_character_
    )
    info$accessibility <- safe_column_access(
      previous_stage,
      "previous_accessibility",
      NA_character_
    )
    info$central_question <- safe_column_access(
      previous_stage,
      "previous_central_question",
      NA_character_
    )
    info$problem <- safe_column_access(
      previous_stage,
      "previous_problem",
      NA_character_
    )
    info$theory <- safe_column_access(
      previous_stage,
      "previous_theory",
      NA_character_
    )
  } else if (stage_name == "Structure") {
    info$layout <- safe_column_access(previous_stage, "layout", NA_character_)
    info$concepts <- safe_column_access(
      previous_stage,
      "concepts",
      NA_character_
    )
    info$accessibility <- safe_column_access(
      previous_stage,
      "accessibility",
      NA_character_
    )

    # get information from previous stages
    info$central_question <- safe_column_access(
      previous_stage,
      "previous_central_question",
      NA_character_
    )
    info$problem <- safe_column_access(
      previous_stage,
      "previous_problem",
      NA_character_
    )
    info$theory <- safe_column_access(
      previous_stage,
      "previous_theory",
      NA_character_
    )
  } else if (stage_name == "Interpret") {
    info$central_question <- safe_column_access(
      previous_stage,
      "central_question",
      NA_character_
    )

    # get information from previous stages
    info$problem <- safe_column_access(
      previous_stage,
      "previous_problem",
      NA_character_
    )
    info$theory <- safe_column_access(
      previous_stage,
      "previous_theory",
      NA_character_
    )
  }

  return(info)
}
