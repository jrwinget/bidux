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
#' @param include_exp_design Logical indicating whether to include experiment
#'        design suggestions. Default is TRUE.
#' @param include_telemetry Logical indicating whether to include telemetry
#'        tracking and monitoring suggestions. Default is TRUE.
#' @param include_empower_tools Logical indicating whether to include
#'        context-aware empowerment tool suggestions. Default is TRUE.
#'
#' @return A tibble containing the documented information for the "Validate"
#'         stage.
#'
#' @examples
#' validate_result <- bid_interpret(
#'     central_question = "How can we improve delivery efficiency?",
#'     data_story = list(
#'       hook = "Too many delays",
#'       context = "Excessive shipments",
#'       tension = "User frustration",
#'       resolution = "Increase delivery channels"
#'     )
#'   ) |>
#'   bid_notice(
#'     problem  = "Issue with dropdown menus",
#'     evidence = "User testing indicated delays"
#'   ) |>
#'   bid_anticipate(
#'     bias_mitigations = list(
#'       anchoring = "Provide reference points",
#'       framing = "Use gain-framed messaging"
#'     )
#'   ) |>
#'   bid_structure() |>
#'   bid_validate(
#'    include_exp_design = FALSE,
#'    include_telemetry = TRUE,
#'    include_empower_tools = TRUE
#'   )
#'
#' summary(validate_result)
#'
#' @export
bid_validate <- function(
    previous_stage,
    summary_panel = NULL,
    collaboration = NULL,
    next_steps = NULL,
    include_exp_design = TRUE,
    include_telemetry = TRUE,
    include_empower_tools = TRUE) {
  validate_required_params(previous_stage = previous_stage)
  validate_previous_stage(previous_stage, "Validate")

  # validate boolean parameters
  validate_logical_param(include_exp_design, "include_exp_design")
  validate_logical_param(include_telemetry, "include_telemetry")
  validate_logical_param(include_empower_tools, "include_empower_tools")

  if (is.null(summary_panel)) {
    summary_panel <- generate_summary_panel_suggestion(previous_stage)
    cli::cli_alert_info(paste0("Suggested summary panel: ", summary_panel))
  }

  if (is.null(collaboration)) {
    collaboration <- generate_collaboration_suggestion(
      previous_stage,
      include_empower_tools
    )
    cli::cli_alert_info(paste0(
      "Suggested collaboration features: ",
      collaboration
    ))
  }

  if (is.null(next_steps)) {
    next_steps <- generate_next_steps_suggestion(
      previous_stage,
      include_exp_design,
      include_telemetry
    )
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
    previous_stage,
    include_exp_design,
    include_telemetry,
    include_empower_tools
  )

  # normalize previous stage to ensure field name consistency
  normalized_previous <- normalize_previous_stage(previous_stage)
  previous_info <- extract_previous_stage_info(normalized_previous)

  # create result tibble
  result_data <- tibble::tibble(
    stage = "Validate",
    summary_panel = summary_panel %||% NA_character_,
    collaboration = collaboration %||% NA_character_,
    next_steps = next_steps_formatted,
    previous_bias = previous_info$bias %||% NA_character_,
    previous_accessibility = previous_info$accessibility %||% NA_character_,
    previous_layout = previous_info$layout %||% NA_character_,
    previous_concepts = previous_info$concepts %||% NA_character_,
    previous_central_question = previous_info$central_question %||%
      NA_character_,
    previous_hook = previous_info$hook %||% NA_character_,
    previous_problem = previous_info$problem %||% NA_character_,
    previous_theory = previous_info$theory %||% NA_character_,
    previous_audience = previous_info$audience %||% NA_character_,
    previous_personas = previous_info$personas %||% NA_character_,
    suggestions = suggestions,
    timestamp = .now()
  )

  # create comprehensive metadata using standardized helper
  metadata <- get_stage_metadata(
    5,
    list(
      has_summary_panel = !is.null(summary_panel),
      has_collaboration = !is.null(collaboration),
      next_steps_count = length(parse_next_steps(next_steps_formatted)),
      include_exp_design = include_exp_design,
      include_telemetry = include_telemetry,
      include_empower_tools = include_empower_tools
    )
  )

  # create and validate bid_stage object
  result <- bid_stage("Validate", result_data, metadata)

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

generate_collaboration_suggestion <- function(
  previous_stage,
  include_empower_tools = TRUE
) {
  # use the standardized helper function for consistency
  audience <- get_audience_from_previous(previous_stage)

  base_suggestion <- "Enable team sharing and collaborative decision-making features"

  if (!is.na(audience) && nchar(trimws(audience)) > 0) {
    audience_lower <- tolower(audience)

    empowerment_suffix <- if (include_empower_tools) {
      if (grepl("executive|leadership|manager", audience_lower)) {
        " with executive empowerment tools like annotated insights and decision history"
      } else if (grepl("analyst|technical|data", audience_lower)) {
        " with analyst empowerment features like exploratory chat and methodology explanations"
      } else if (grepl("team|group|multiple", audience_lower)) {
        " with team empowerment via shared workspaces and collaborative insights"
      } else {
        " with user empowerment through guided explanations and contextual help"
      }
    } else {
      ""
    }

    if (grepl("executive|leadership|manager", audience_lower)) {
      return(paste0(
        "Executive-focused collaboration with summary sharing and decision tracking",
        empowerment_suffix
      ))
    } else if (grepl("analyst|technical|data", audience_lower)) {
      return(paste0(
        "Advanced collaboration tools including data export, annotation, and methodology sharing",
        empowerment_suffix
      ))
    } else if (grepl("team|group|multiple", audience_lower)) {
      return(paste0(
        "Multi-user collaboration with role-based permissions and shared annotations",
        empowerment_suffix
      ))
    } else if (grepl("client|customer|external", audience_lower)) {
      return(paste0(
        "Client-friendly sharing options with controlled access and presentation modes",
        empowerment_suffix
      ))
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

generate_next_steps_suggestion <- function(
  previous_stage,
  include_exp_design = TRUE,
  include_telemetry = TRUE
) {
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

  # add experiment design recommendations if requested
  if (include_exp_design) {
    next_steps <- c(
      next_steps,
      "Design A/B tests to validate key design decisions and user pathways",
      "Conduct usability testing sessions with representative users",
      "Plan controlled experiments to measure impact of bias mitigations"
    )
  }

  # add telemetry and monitoring recommendations if requested
  if (include_telemetry) {
    next_steps <- c(
      next_steps,
      "Implement telemetry tracking for user interactions and pain points",
      "Set up monitoring dashboards to track key performance indicators",
      "Plan post-launch telemetry analysis to validate design improvements"
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
    previous_stage,
    include_exp_design = TRUE,
    include_telemetry = TRUE,
    include_empower_tools = TRUE) {
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

  # add suggestions based on flags
  if (
    include_exp_design && !any(grepl("test|experiment", tolower(steps_list)))
  ) {
    suggestions <- c(
      suggestions,
      "Consider adding experimental design and A/B testing to your validation plan"
    )
  }

  if (
    include_telemetry &&
      !any(grepl("telemetry|monitor|track", tolower(steps_list)))
  ) {
    suggestions <- c(
      suggestions,
      "Include telemetry and monitoring in your post-launch validation"
    )
  }

  if (
    include_empower_tools &&
      !grepl("empower|explain|help", tolower(collaboration %||% ""))
  ) {
    suggestions <- c(
      suggestions,
      "Consider adding user empowerment tools to enhance collaboration"
    )
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
    info$bias <- safe_column_access(
      previous_stage,
      "previous_bias",
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
    info$hook <- safe_column_access(
      previous_stage,
      "previous_hook",
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
    info$audience <- safe_column_access(
      previous_stage,
      "previous_audience",
      NA_character_
    )
    info$personas <- safe_column_access(
      previous_stage,
      "previous_personas",
      NA_character_
    )
  } else if (stage_name == "Interpret") {
    info$central_question <- safe_column_access(
      previous_stage,
      "central_question",
      NA_character_
    )
    info$hook <- safe_column_access(
      previous_stage,
      "hook",
      NA_character_
    )
    info$audience <- safe_column_access(
      previous_stage,
      "audience",
      NA_character_
    )
    info$personas <- safe_column_access(
      previous_stage,
      "personas",
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
  } else if (stage_name == "Notice") {
    info$problem <- safe_column_access(
      previous_stage,
      "problem",
      NA_character_
    )
    info$theory <- safe_column_access(
      previous_stage,
      "theory",
      NA_character_
    )
  }

  return(info)
}
