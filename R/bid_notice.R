#' Document User Notice Stage in BID Framework
#'
#' @description
#' This function documents the initial observation and problem identification
#' stage. It represents stage 1 in the BID framework and now returns a
#' structured bid_stage object with enhanced metadata and external mapping
#' support.
#'
#' @param problem A character string describing the observed user problem.
#' @param theory A character string describing the behavioral theory that might
#'        explain the problem. If NULL, will be auto-suggested using external
#'        theory mappings.
#' @param evidence A character string describing evidence supporting the
#'        problem.
#' @param target_audience Optional character string describing the target
#'        audience.
#'
#' @return A bid_stage object containing the documented information for the
#'         "Notice" stage with enhanced metadata and validation.
#'
#' @examples
#' # Basic usage with auto-suggested theory
#' notice_result <- bid_notice(
#'   problem = "Users struggling with complex dropdowns and too many options",
#'   evidence = "User testing shows 65% abandonment rate on filter selection"
#' )
#'
#' # Print shows human-friendly summary
#' print(notice_result)
#'
#' # Access underlying data
#' summary(notice_result)
#'
#' # Check stage and metadata
#' get_stage(notice_result)
#' get_metadata(notice_result)
#'
#' # With explicit theory
#' notice_explicit <- bid_notice(
#'   problem = "Mobile interface is difficult to navigate",
#'   theory = "Fitts's Law",
#'   evidence = "Mobile users report frustration with small touch targets",
#'   target_audience = "Mobile users with varying technical expertise"
#' )
#'
#' @export
bid_notice <- function(
    problem,
    theory = NULL,
    evidence = NULL,
    target_audience = NULL) {
  if (missing(problem) || is.null(problem)) {
    stop("Required parameter 'problem' must be provided", call. = FALSE)
  }

  if (!is.character(problem) || length(problem) != 1) {
    stop("'problem' must be a single character string", call. = FALSE)
  }

  if (missing(evidence) || is.null(evidence)) {
    stop("Required parameter 'evidence' must be provided", call. = FALSE)
  }

  if (!is.character(evidence) || length(evidence) != 1) {
    stop("'evidence' must be a single character string", call. = FALSE)
  }

  if (!is.null(theory) && (!is.character(theory) || length(theory) != 1)) {
    stop("'theory' must be a single character string or NULL", call. = FALSE)
  }

  if (
    !is.null(target_audience) &&
      (!is.character(target_audience) || length(target_audience) != 1)
  ) {
    stop(
      "'target_audience' must be a single character string or NULL",
      call. = FALSE
    )
  }

  # Input quality validation with warnings
  problem_clean <- trimws(problem)
  evidence_clean <- trimws(evidence)

  # Check for empty strings after trimming FIRST
  if (nchar(problem_clean) == 0) {
    stop("Problem cannot be empty or whitespace only", call. = FALSE)
  }
  if (nchar(evidence_clean) == 0) {
    stop("Evidence cannot be empty or whitespace only", call. = FALSE)
  }

  # Enhanced parameter validation (moved after empty string check)
  validate_required_params(problem = problem_clean, evidence = evidence_clean)

  if (nchar(problem_clean) < 10) {
    warning(
      paste(
        paste(
          "Problem description is very short (< 10 characters).",
          "Consider providing more detail."
        )
      ),
      call. = FALSE
    )
  }
  if (nchar(evidence_clean) < 10) {
    warning(
      paste(
        "Evidence description is very short (< 10 characters).",
        "Consider providing more detail."
      ),
      call. = FALSE
    )
  }

  auto_suggested_theory <- FALSE
  theory_confidence <- 1.0

  if (is.null(theory)) {
    theory <- suggest_theory_from_mappings(
      problem_clean,
      evidence_clean,
      mappings = NULL
    )
    auto_suggested_theory <- TRUE

    # get confidence score
    default_mappings <- load_theory_mappings()
    matching_row <- default_mappings[default_mappings$theory == theory, ]
    if (nrow(matching_row) > 0) {
      theory_confidence <- matching_row$confidence[1]
    }

    cat(paste0(
      "Auto-suggested theory: ",
      theory,
      " (confidence: ",
      round(theory_confidence * 100),
      "%)\n"
    ))
  }

  suggestions <- generate_notice_suggestions(
    problem_clean,
    theory,
    evidence_clean,
    target_audience
  )

  # create result tibble
  result_data <- tibble::tibble(
    stage = "Notice",
    problem = problem, # use original problem to preserve exact string
    theory = theory %||% NA_character_,
    evidence = evidence, # use original evidence to preserve exact string
    target_audience = target_audience %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  # create comprehensive metadata
  metadata <- list(
    auto_suggested_theory = auto_suggested_theory,
    theory_confidence = theory_confidence,
    problem_length = nchar(problem_clean),
    evidence_length = nchar(evidence_clean),
    has_target_audience = !is.null(target_audience),
    validation_status = "completed",
    stage_number = 1,
    total_stages = 5,
    custom_mappings_used = FALSE
  )

  # create and validate bid_stage object
  result <- bid_stage("Notice", result_data, metadata)

  # enhanced user feedback with progress tracking
  bid_message(
    "Stage 1 (Notice) completed. (20% complete)",
    paste0("Problem: ", truncate_text(problem_clean, 60)),
    paste0(
      "Theory: ",
      theory,
      if (auto_suggested_theory) " (auto-suggested)" else ""
    ),
    paste0("Evidence: ", truncate_text(evidence_clean, 60)),
    if (auto_suggested_theory) {
      paste0("Theory confidence: ", round(theory_confidence * 100), "%")
    },
    "Next: Use bid_interpret() for Stage 2"
  )

  return(result)
}

#' Enhanced suggestions generator for Notice stage
#'
#' @param problem Character string with problem description
#' @param theory Character string with theory
#' @param evidence Character string with evidence
#' @param target_audience Character string with target audience (optional)
#' @return Character string with suggestions
#' @keywords internal
generate_notice_suggestions <- function(
    problem,
    theory,
    evidence,
    target_audience) {
  suggestions <- character(0)

  # theory-specific suggestions
  if (!is.null(theory) && !is.na(theory)) {
    if (theory == "Cognitive Load Theory") {
      suggestions <- c(
        suggestions,
        paste(
          "Consider conducting cognitive load assessment to measure mental",
          "effort required"
        )
      )
    } else if (theory == "Hick's Law") {
      suggestions <- c(
        suggestions,
        paste(
          "Measure decision time and number of choices to validate Hick's Law",
          "application"
        )
      )
    } else if (theory == "Visual Hierarchies") {
      suggestions <- c(
        suggestions,
        paste(
          "Conduct eye-tracking or attention mapping to understand visual",
          "processing patterns"
        )
      )
    } else if (theory == "Information Scent") {
      suggestions <- c(
        suggestions,
        "Analyze user navigation patterns and information-seeking behavior"
      )
    } else if (theory == "Fitts's Law") {
      suggestions <- c(
        suggestions,
        "Measure target sizes and distances, especially for mobile interactions"
      )
    } else if (theory == "Aesthetic-Usability") {
      suggestions <- c(
        suggestions,
        "Balance visual appeal with functional usability testing"
      )
    }
  }

  # evidence quality suggestions
  if (!is.null(evidence) && !is.na(evidence)) {
    evidence_lower <- tolower(evidence)
    if (!grepl("\\d", evidence)) {
      suggestions <- c(
        suggestions,
        paste(
          "Consider adding quantitative metrics to strengthen evidence",
          "(e.g., completion rates, time on task)"
        )
      )
    }
    if (!grepl("test|study|research|data|metric", evidence_lower)) {
      suggestions <- c(
        suggestions,
        paste(
          "Consider conducting formal user testing or collecting analytics",
          "data to support observations"
        )
      )
    }
  }

  # target audience suggestions
  if (is.null(target_audience) || is.na(target_audience)) {
    suggestions <- c(
      suggestions,
      "Define specific target audience to better focus design solutions"
    )
  } else {
    audience_lower <- tolower(target_audience)
    if (grepl("varying|different|mixed", audience_lower)) {
      suggestions <- c(
        suggestions,
        paste(
          "Consider creating user personas for different skill levels within",
          "your audience"
        )
      )
    }
  }

  # Problem-specific suggestions
  problem_lower <- tolower(problem)
  if (grepl("users struggle|difficult|hard|confus", problem_lower)) {
    suggestions <- c(
      suggestions,
      paste(
        "Consider conducting task analysis to understand specific struggle",
        "points and failure modes"
      )
    )
  }
  if (grepl("slow|delay|time|performance", problem_lower)) {
    suggestions <- c(
      suggestions,
      paste(
        "Measure task completion times and identify specific performance",
        "bottlenecks"
      )
    )
  }
  if (grepl("mobile|phone|tablet|touch", problem_lower)) {
    suggestions <- c(
      suggestions,
      paste(
        "Ensure mobile-specific usability testing and consider touch",
        "interaction patterns"
      )
    )
  }
  if (grepl("too many|overwhelm|choice|option", problem_lower)) {
    suggestions <- c(
      suggestions,
      paste(
        "Consider progressive disclosure or categorization to reduce choice",
        "complexity"
      )
    )
  }

  # Default suggestion if none generated
  if (length(suggestions) == 0) {
    suggestions <- c(
      paste(
        "Problem clearly identified. Consider gathering additional",
        "quantitative evidence."
      ),
      "Move to bid_interpret() to develop central question and data story."
    )
  }

  return(paste(suggestions, collapse = " "))
}

# legacy function name support for backward compatibility
suggest_theory_from_problem <- function(problem, evidence = NULL) {
  .Deprecated(
    "suggest_theory_from_mappings",
    msg = paste(
      "suggest_theory_from_problem is deprecated.",
      "Use suggest_theory_from_mappings instead."
    )
  )
  
  suggest_theory_from_mappings(problem, evidence, mappings = NULL)
}
