#' Document User Interpretation Stage in BID Framework
#'
#' @description
#' This function documents the interpretation of user needs, capturing the central question and
#' the data storytelling narrative. It represents stage 2 in the BID framework.
#'
#' @param previous_stage A tibble or list output from \code{bid_notice()}.
#' @param central_question A character string representing the main question to be answered.
#' @param data_story A list containing elements such as \code{hook}, \code{context}, \code{tension}, and \code{resolution}.
#' @return A tibble containing the documented information for the "Interpret" stage.
#' @examples
#' notice <- bid_notice(
#'   problem = "Users struggle with complex data",
#'   theory = "Cognitive Load Theory",
#'   evidence = "Test results indicate delays"
#' )
#' bid_interpret(
#'   previous_stage = notice,
#'   central_question = "What drives the decline in user engagement?",
#'   data_story = list(
#'     hook = "Declining trend in engagement",
#'     context = "Previous high engagement levels",
#'     tension = "Unexpected drop",
#'     resolution = "Investigate new UI changes"
#'   )
#' )
#' @export
bid_interpret <- function(previous_stage, central_question, data_story) {
  # Validate inputs
  if (missing(previous_stage) || missing(central_question) || missing(data_story)) {
    stop("All parameters (previous_stage, central_question, data_story) must be provided.")
  }
  
  tibble::tibble(
    stage = "Interpret",
    central_question = central_question,
    hook = data_story$hook %||% NA,
    context = data_story$context %||% NA,
    tension = data_story$tension %||% NA,
    resolution = data_story$resolution %||% NA,
    previous_problem = previous_stage$problem[1],
    timestamp = Sys.time()
  )
}
