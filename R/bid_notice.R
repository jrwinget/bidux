#' Document Problem Notice Stage in BID Framework
#'
#' @description
#' This function documents the problem area by capturing insights related to
#' cognitive load, Hick's Law, and visual hierarchies. It forms the first stage
#' in the Behavior Insight Design framework. If no theory is specified, it will
#' suggest appropriate theories based on the problem description.
#'
#' @param problem A character string describing the identified problem.
#' @param evidence A character string providing evidence or example (e.g.,
#'        results from user testing).
#' @param theory Optional character string representing the underlying
#'        psychological theory.
#' @param target_audience Optional character string describing the primary users
#'        of the dashboard.
#'
#' @return A tibble containing the documented information for the "Notice"
#'         stage.
#'
#' @examples
#' bid_notice(
#'   problem = "Users struggle to navigate cluttered dashboards",
#'   evidence = "User testing showed increased time to locate key metrics."
#' )
#'
#' # With specified theory
#' bid_notice(
#'   problem = "Users struggle to navigate cluttered dashboards",
#'   theory = "Cognitive Load Theory",
#'   evidence = "User testing showed increased time to locate key metrics."
#' )
#'
#' # With target audience
#' bid_notice(
#'   problem = "Sales team struggles with complex filter combinations",
#'   evidence = "Training sessions revealed confusion with multiple selections",
#'   target_audience = "Sales representatives with varying technical skills"
#' )
#'
#' # Simple example
#' \donttest{
#' bid_notice(
#'   problem = "Too many options",
#'   evidence = "User feedback"
#' )
#' }
#'
#' @export
bid_notice <- function(
    problem,
    evidence,
    theory = NULL,
    target_audience = NULL) {
  validate_required_params(problem = problem, evidence = evidence)

  if (!is.character(problem)) {
    cli::cli_abort(
      c(
        "Problem must be a character string",
        "i" = "You provided {.cls {class(problem)}}"
      )
    )
  }

  if (!is.character(evidence)) {
    cli::cli_abort(
      c(
        "Evidence must be a character string",
        "i" = "You provided {.cls {class(evidence)}}"
      )
    )
  }

  if (!is.null(theory) && !is.character(theory)) {
    cli::cli_abort(
      c(
        "Theory must be a character string",
        "i" = "You provided {.cls {class(theory)}}"
      )
    )
  }

  if (!is.null(target_audience) && !is.character(target_audience)) {
    cli::cli_abort(
      c(
        "Target audience must be a character string",
        "i" = "You provided {.cls {class(target_audience)}}"
      )
    )
  }

  if (nchar(problem) < 10) {
    cli::cli_warn(
      c(
        "Problem description is very short ({nchar(problem)} characters)",
        "i" = "Consider providing more detail for better theory matching"
      )
    )
  }

  if (nchar(evidence) < 10) {
    cli::cli_warn(
      c(
        "Evidence description is very short ({nchar(evidence)} characters)",
        "i" = "Consider providing more specific evidence for better suggestions"
      )
    )
  }

  theory_was_suggested <- is.null(theory)

  if (is.null(theory)) {
    theory_suggestions <- list(
      "clutter" = "Visual Hierarchies",
      "complex" = "Cognitive Load Theory",
      "overwhelm" = "Cognitive Load Theory",
      "choice" = "Hick's Law",
      "option" = "Hick's Law",
      "dropdown" = "Hick's Law",
      "select" = "Hick's Law",
      "navigation" = "Information Scent",
      "find" = "Information Scent",
      "locate" = "Visual Hierarchies",
      "confus" = "Cognitive Load Theory",
      "scroll" = "Fitts's Law",
      "click" = "Fitts's Law",
      "menu" = "Miller's Law",
      "remember" = "Miller's Law",
      "attention" = "Pre-attentive Processing",
      "notice" = "Pre-attentive Processing",
      "layout" = "Gestalt Principles",
      "organize" = "Gestalt Principles",
      "group" = "Principle of Proximity"
    )

    problem_lower <- stringr::str_to_lower(problem)
    evidence_lower <- stringr::str_to_lower(evidence)
    combined_text <- paste(problem_lower, evidence_lower)

    matched_theories <- character(0)
    for (keyword in names(theory_suggestions)) {
      if (stringr::str_detect(combined_text, keyword)) {
        matched_theories <- c(matched_theories, theory_suggestions[[keyword]])
      }
    }

    if (length(matched_theories) > 0) {
      theory_counts <- table(matched_theories)
      theory <- names(theory_counts)[which.max(theory_counts)]
    } else {
      theory <- "Cognitive Load Theory"
    }
  }

  theory_info <- bid_concept(theory)
  if (!is.null(theory_info) && nrow(theory_info) > 0) {
    suggestions <- theory_info$implementation_tips
  } else {
    suggestions <- paste(
      "Consider how this problem relates to user cognitive processes and",
      "interface design."
    )

    if (!theory_was_suggested) {
      cli::cli_warn(
        c(
          "Theory '{theory}' not found in the concept dictionary",
          "i" = "Using generic implementation suggestions",
          "i" = "Use {.fn bid_concepts} to see available concepts"
        )
      )
    }
  }

  result <- tibble::tibble(
    stage = "Notice",
    problem = problem,
    theory = theory,
    evidence = evidence,
    target_audience = target_audience %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  if (theory_was_suggested) {
    bid_message(
      "Stage 1 (Notice) completed.",
      paste(
        "Based on your problem description, the suggested theory is:",
        theory
      ),
      paste("Primary issue:", problem),
      if (!is.null(target_audience)) {
        paste("Target audience:", target_audience)
      }
    )
  } else {
    bid_message(
      "Stage 1 (Notice) completed.",
      paste("Using theory:", theory),
      paste("Primary issue:", problem),
      if (!is.null(target_audience)) {
        paste("Target audience:", target_audience)
      }
    )
  }

  return(result)
}
