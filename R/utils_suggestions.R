# ==============================================================================
# SUGGESTION UTILITIES
# ==============================================================================
#
# Functions for generating and evaluating contextual suggestions.
#

#' Generate contextual suggestions based on stage and content
#'
#' @param stage_name Current BID stage name
#' @param context_data Named list with stage-specific context
#' @param suggestion_rules Optional custom suggestion rules
#'
#' @return Character string with consolidated suggestions
#'
#' @keywords internal
#' @noRd
generate_stage_suggestions <- function(
    stage_name,
    context_data,
    suggestion_rules = NULL) {
  # use consolidated rules from suggest_rules.R
  applicable_suggestions <- apply_suggestion_rules(
    stage_name,
    context_data,
    suggestion_rules
  )

  # if no suggestions matched, use fallback
  if (length(applicable_suggestions) == 0) {
    return(get_fallback_suggestion(stage_name))
  }

  # limit to top 3 suggestions to avoid overwhelming users
  if (length(applicable_suggestions) > 3) {
    applicable_suggestions <- applicable_suggestions[1:3]
  }

  return(paste(applicable_suggestions, collapse = " "))
}

#' Evaluate suggestion condition against context data
#'
#' @param condition Function that evaluates context
#' @param context_data Named list with context
#'
#' @return Logical indicating if condition is met
#'
#' @keywords internal
#' @noRd
evaluate_suggestion_condition <- function(condition, context_data) {
  if (!is.function(condition)) {
    warning("Condition is not a function, skipping", call. = FALSE)
    return(FALSE)
  }

  if (!is.list(context_data) && !is.null(context_data)) {
    warning(
      "Context data is not a list or NULL, attempting to coerce",
      call. = FALSE
    )
    context_data <- list(context_data)
  }

  tryCatch(
    {
      result <- condition(context_data)
      if (!is.logical(result) || length(result) != 1) {
        warning(
          "Condition function returned non-logical or multi-value result",
          call. = FALSE
        )
        return(FALSE)
      }
      return(result)
    },
    error = function(e) {
      warning(
        "Error evaluating suggestion condition: ",
        e$message,
        call. = FALSE
      )
      FALSE
    }
  )
}
