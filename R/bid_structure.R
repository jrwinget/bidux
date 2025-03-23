#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard, including layout and design elements
#' such as proximity, dual-processing, and default effects.
#'
#' @param previous_stage A tibble or list output from \code{bid_interpret()}.
#' @param layout A character string indicating the layout type (e.g., "dual_process", "grid").
#' @param concepts A character vector of BID concepts applied in this stage.
#' @return A tibble containing the documented information for the "Structure" stage.
#' @examples
#' interpret <- bid_interpret(
#'   bid_notice(
#'     problem = "Users struggle with information overload",
#'     theory = "Cognitive Load Theory",
#'     evidence = "Survey results indicate delays"
#'   ),
#'   central_question = "How can we simplify data presentation?",
#'   data_story = list(
#'     hook = "Data is too complex",
#'     context = "Overloaded with charts",
#'     tension = "Confusing layout",
#'     resolution = "Introduce clear grouping"
#'   )
#' )
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("principle_of_proximity", "default_effect")
#' )
#' @export
bid_structure <- function(previous_stage, layout, concepts) {
  # Validate inputs
  if (missing(previous_stage) || missing(layout) || missing(concepts)) {
    stop("All parameters (previous_stage, layout, concepts) must be provided.")
  }
  
  tibble::tibble(
    stage = "Structure",
    layout = layout,
    concepts = paste(concepts, collapse = ", "),
    previous_question = previous_stage$central_question[1],
    timestamp = Sys.time()
  )
}
