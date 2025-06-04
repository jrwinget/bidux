#' Document User Notice Stage in BID Framework
#'
#' @description
#' This function documents the initial observation and problem identification
#' stage. It represents stage 1 in the BID framework.
#'
#' @param problem A character string describing the observed user problem.
#' @param theory A character string describing the behavioral theory that might
#'        explain the problem.
#' @param evidence A character string describing evidence supporting the
#'        problem.
#' @param target_audience Optional character string describing the target
#'        audience.
#'
#' @return A tibble containing the documented information for the "Notice"
#'         stage.
#'
#' @examples
#' bid_notice(
#'   problem = "Users struggle with complex data",
#'   theory = "Cognitive Load Theory",
#'   evidence = "User testing shows confusion with current interface"
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

  if (missing(evidence) || is.null(evidence)) {
    stop("Required parameter 'evidence' must be provided", call. = FALSE)
  }

  if (!is.character(problem)) {
    stop("Problem must be a character string", call. = FALSE)
  }

  if (!is.character(evidence)) {
    stop("Evidence must be a character string", call. = FALSE)
  }

  if (!is.null(theory) && !is.character(theory)) {
    stop("Theory must be a character string", call. = FALSE)
  }

  if (!is.null(target_audience)) {
    if (
      is.logical(target_audience) &&
        length(target_audience) == 1 &&
        is.na(target_audience)
    ) {
      warning("target_audience is NA", call. = FALSE)
    } else if (is.character(target_audience)) {
      if (is.na(target_audience)) {
        warning("target_audience is NA", call. = FALSE)
      } else if (nchar(trimws(target_audience)) == 0) {
        warning("target_audience is empty", call. = FALSE)
      }
    } else {
      stop("Target audience must be a character string", call. = FALSE)
    }
  }

  if (is.character(problem) && nchar(trimws(problem)) == 0) {
    warning("Problem description is very short", call. = FALSE)
  } else if (nchar(trimws(problem)) < 10) {
    warning("Problem description is very short", call. = FALSE)
  }

  if (is.character(evidence) && nchar(trimws(evidence)) == 0) {
    warning("Evidence description is very short", call. = FALSE)
  } else if (nchar(trimws(evidence)) < 10) {
    warning("Evidence description is very short", call. = FALSE)
  }

  if (is.null(theory)) {
    theory <- suggest_theory_from_problem(problem, evidence)
    cli::cli_alert_info(paste0("Suggested theory: ", theory))
  }

  if (is.null(evidence)) {
    evidence <- "Evidence needed to support this observation"
    cli::cli_alert_info(
      "Consider gathering specific evidence to support this problem observation"
    )
  }

  suggestions <- generate_notice_suggestions(
    problem,
    theory,
    evidence,
    target_audience
  )

  result <- tibble::tibble(
    stage = "Notice",
    problem = problem,
    theory = theory %||% NA_character_,
    evidence = evidence %||% NA_character_,
    target_audience = target_audience %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 1 (Notice) completed.",
    paste0("Problem: ", truncate_text(problem, 50)),
    paste0("Theory: ", truncate_text(theory, 50)),
    paste0("Evidence: ", truncate_text(evidence, 50)),
    suggestions
  )

  return(result)
}

suggest_theory_from_problem <- function(problem, evidence = NULL) {
  combined_text <- tolower(paste(problem, evidence %||% "", sep = " "))

  if (
    grepl(
      "too many.*option|overwhelm.*too many|dropdown.*option|too many.*choice|many.*choice|choice.*option|options.*dropdown|too many choices|dropdown.*menu",
      combined_text
    )
  ) {
    return("Hick's Law")
  } else if (
    grepl("find.*information|search|locate|discover|navigation", combined_text)
  ) {
    return("Information Scent")
  } else if (
    grepl(
      "visual.*layout|hierarchy|organization|attention|cluttered.*layout|disorganized|layout.*cluttered|cluttered.*disorganized",
      combined_text
    )
  ) {
    return("Visual Hierarchies")
  } else if (
    grepl(
      "complex|overwhelm|too much|confus|mental load|difficult",
      combined_text
    )
  ) {
    return("Cognitive Load Theory")
  } else if (grepl("mobile|touch|responsive|screen", combined_text)) {
    return("Fitts's Law")
  } else if (grepl("aesthetic|beautiful|appearance|design", combined_text)) {
    return("Aesthetic-Usability")
  } else {
    return("Cognitive Load Theory")
  }
}

generate_notice_suggestions <- function(
    problem,
    theory,
    evidence,
    target_audience) {
  suggestions <- character(0)

  if (
    is.null(theory) || theory == "Evidence needed to support this observation"
  ) {
    suggestions <- c(
      suggestions,
      "Consider gathering specific evidence through user testing or analytics"
    )
  }

  if (is.null(target_audience)) {
    suggestions <- c(
      suggestions,
      "Define specific target audience to better focus solutions"
    )
  }

  problem_lower <- tolower(problem)
  if (grepl("users struggle|difficult|hard", problem_lower)) {
    suggestions <- c(
      suggestions,
      "Consider conducting task analysis to understand specific struggle points"
    )
  }

  if (length(suggestions) == 0) {
    suggestions <- paste(
      "Problem clearly defined.",
      "Move to Interpret stage to develop central question."
    )
  }

  return(paste(suggestions, collapse = " "))
}
