#' Document Problem Notice Stage in BID Framework
#'
#' @description
#' This function documents the problem area by capturing insights related to
#' cognitive load, Hick's Law, and visual hierarchies. It forms the first stage
#' in the Behavior Insight Design framework. If no theory is specified, it will
#' suggest appropriate theories based on the problem description.
#'
#' @param problem A character string describing the identified problem.
#' @param theory Optional character string representing the underlying
#'        psychological theory.
#' @param evidence A character string providing evidence or example (e.g.,
#'        results from user testing).

#' @return A tibble containing the documented information for the "Notice"
#'         stage.

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

#' @export
bid_notice <- function(problem, theory = NULL, evidence) {
  if (missing(problem) || missing(evidence)) {
    stop("Both problem and evidence parameters must be provided.")
  }

  # If theory is not provided, suggest appropriate theories based on keywords
  if (is.null(theory)) {
    # Define keyword-to-theory mappings
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
      "attention" = "Preattentive Processing",
      "notice" = "Preattentive Processing",
      "layout" = "Gestalt Principles",
      "organize" = "Gestalt Principles",
      "group" = "Principle of Proximity"
    )

    # Check for keyword matches in problem and evidence
    problem_lower <- stringr::str_to_lower(problem)
    evidence_lower <- stringr::str_to_lower(evidence)
    combined_text <- paste(problem_lower, evidence_lower)

    matched_theories <- character(0)
    for (keyword in names(theory_suggestions)) {
      if (stringr::str_detect(combined_text, keyword)) {
        matched_theories <- c(matched_theories, theory_suggestions[[keyword]])
      }
    }

    # Use the most frequent theory or default to "Cognitive Load Theory"
    if (length(matched_theories) > 0) {
      theory_counts <- table(matched_theories)
      theory <- names(theory_counts)[which.max(theory_counts)]
    } else {
      theory <- "Cognitive Load Theory"
    }
  }

  # Get relevant insights about the theory
  theory_info <- bid_concept(theory)

  # Generate suggestions based on the theory
  if (!is.null(theory_info) && nrow(theory_info) > 0) {
    suggestions <- theory_info$implementation_tips
  } else {
    suggestions <- paste(
      "Consider how this problem relates to user cognitive processes and",
      "interface design."
    )
  }

  # Create result tibble
  result <- tibble::tibble(
    stage = "Notice",
    problem = problem,
    theory = theory,
    evidence = evidence,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  # Add a message about the theory used/suggested
  if (is.null(theory)) {
    message(
      paste0(
        "Based on your problem description, the suggested theory is: ",
        theory
      )
    )
  }

  return(result)
}
