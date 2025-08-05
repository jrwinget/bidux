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
  # standardized parameter validation
  validate_character_param(problem, "problem", min_length = 1)
  validate_character_param(evidence, "evidence", min_length = 1)
  validate_character_param(theory, "theory", allow_null = TRUE)
  validate_character_param(target_audience, "target_audience", allow_null = TRUE)

  # clean input for processing
  problem_clean <- trimws(problem)
  evidence_clean <- trimws(evidence)

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

  # generate contextual suggestions using unified system
  context_data <- list(
    problem = problem_clean,
    evidence = evidence_clean,
    theory = theory,
    target_audience = target_audience
  )
  suggestions <- generate_stage_suggestions("Notice", context_data)

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

  # create comprehensive metadata using standardized helper
  metadata <- get_stage_metadata(1, list(
    auto_suggested_theory = auto_suggested_theory,
    theory_confidence = theory_confidence,
    problem_length = nchar(problem_clean),
    evidence_length = nchar(evidence_clean),
    has_target_audience = !is.null(target_audience),
    custom_mappings_used = FALSE
  ))

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
