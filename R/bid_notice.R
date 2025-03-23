#' Document Problem Notice Stage in BID Framework
#'
#' @description
#' This function documents the problem area by capturing insights related to cognitive load,
#' Hick's Law, and visual hierarchies. It forms the first stage in the Behavior Insight Design framework.
#'
#' @param problem A character string describing the identified problem.
#' @param theory A character string representing the underlying psychological theory (e.g., "Cognitive Load Theory", "Hick's Law").
#' @param evidence A character string providing evidence or example (e.g., results from user testing).
#' @return A tibble containing the documented information for the "Notice" stage.
#' @examples
#' bid_notice(
#'   problem = "Users struggle to navigate cluttered dashboards",
#'   theory = "Cognitive Load Theory",
#'   evidence = "User testing showed increased time to locate key metrics."
#' )
#' @export
bid_notice <- function(problem, theory, evidence) {
  # Validate inputs
  if (missing(problem) || missing(theory) || missing(evidence)) {
    stop("All parameters (problem, theory, evidence) must be provided.")
  }
  
  tibble::tibble(
    stage = "Notice",
    problem = problem,
    theory = theory,
    evidence = evidence,
    timestamp = Sys.time()
  )
}
