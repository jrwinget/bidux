`%||%` <- rlang::`%||%`

#' Create notice stage for unused input
#' @param input_info List with input usage information
#' @param total_sessions Total number of sessions
#' @return bid_stage object
#' @keywords internal
create_unused_input_notice <- function(input_info, total_sessions) {
  problem <- sprintf(
    "Users are not interacting with the '%s' input control",
    input_info$input_id
  )

  if (input_info$sessions_used == 0) {
    evidence <- sprintf(
      "Telemetry shows 0 out of %d sessions where '%s' was changed",
      total_sessions,
      input_info$input_id
    )
  } else {
    evidence <- sprintf(
      "Only %d out of %d sessions (%.1f%%) interacted with '%s'",
      input_info$sessions_used,
      total_sessions,
      input_info$usage_rate * 100,
      input_info$input_id
    )
  }

  # create notice stage with auto-suggested theory
  notice <- bid_notice(
    problem = problem,
    evidence = evidence,
    target_audience = NULL
  )

  return(notice)
}

#' Create notice stage for delayed interactions
#' @param delay_info List with delay statistics
#' @param total_sessions Total number of sessions
#' @param threshold Threshold used for analysis
#' @return bid_stage object
#' @keywords internal
create_delay_notice <- function(delay_info, total_sessions, threshold) {
  problem <- "Users take a long time before making their first interaction with the dashboard"

  evidence_parts <- character(0)

  if (!is.na(delay_info$median_delay) && delay_info$median_delay > 0) {
    evidence_parts <- c(
      evidence_parts,
      sprintf(
        "Median time to first input is %.0f seconds",
        delay_info$median_delay
      )
    )
  }

  if (delay_info$no_action_rate > 0) {
    evidence_parts <- c(
      evidence_parts,
      sprintf(
        "%.0f%% of sessions had no interactions at all",
        delay_info$no_action_rate * 100
      )
    )
  }

  if (delay_info$rate_over_threshold > 0.1) {
    evidence_parts <- c(
      evidence_parts,
      sprintf(
        "%.0f%% of sessions took over %d seconds to interact",
        delay_info$rate_over_threshold * 100,
        threshold
      )
    )
  }

  evidence <- paste(evidence_parts, collapse = ", and ")

  notice <- bid_notice(
    problem = problem,
    evidence = evidence,
    target_audience = NULL
  )

  return(notice)
}

#' Create notice stage for error patterns
#' @param error_info List with error pattern information
#' @param total_sessions Total number of sessions
#' @return bid_stage object
#' @keywords internal
create_error_notice <- function(error_info, total_sessions) {
  problem <- "Users encounter errors when using the dashboard"

  evidence_parts <- sprintf(
    "Error '%s' occurred %d times in %.0f%% of sessions",
    truncate_text(error_info$error_message %||% "Unknown error", 50),
    error_info$count,
    error_info$session_rate * 100
  )

  if (!is.null(error_info$output_id)) {
    evidence_parts <- paste0(
      evidence_parts,
      sprintf(" (in output '%s')", error_info$output_id)
    )
  }

  if (!is.null(error_info$associated_input)) {
    evidence_parts <- paste0(
      evidence_parts,
      sprintf(", often after changing '%s'", error_info$associated_input)
    )
  }

  notice <- bid_notice(
    problem = problem,
    evidence = evidence_parts,
    target_audience = NULL
  )

  return(notice)
}

#' Create notice stage for navigation issues
#' @param nav_info List with navigation pattern information
#' @param total_sessions Total number of sessions
#' @return bid_stage object
#' @keywords internal
create_navigation_notice <- function(nav_info, total_sessions) {
  problem <- sprintf(
    "The '%s' page/tab is rarely visited by users",
    nav_info$page
  )

  evidence <- sprintf(
    "Only %d sessions (%.1f%%) visited '%s'",
    nav_info$unique_sessions,
    nav_info$visit_rate * 100,
    nav_info$page
  )

  if (nav_info$exit_rate > 0.5) {
    evidence <- paste0(
      evidence,
      sprintf(
        ", and %.0f%% of those sessions ended there",
        nav_info$exit_rate * 100
      )
    )
  }

  notice <- bid_notice(
    problem = problem,
    evidence = evidence,
    target_audience = NULL
  )

  return(notice)
}

#' Create notice stage for confusion patterns
#' @param confusion_info List with confusion pattern information
#' @param total_sessions Total number of sessions
#' @return bid_stage object
#' @keywords internal
create_confusion_notice <- function(confusion_info, total_sessions) {
  problem <- sprintf(
    "Users show signs of confusion when interacting with '%s'",
    confusion_info$input_id
  )

  evidence <- sprintf(
    "%d sessions showed rapid repeated changes (avg %.0f changes in %.1f seconds), suggesting users are unsure about the input's behavior",
    confusion_info$affected_sessions,
    confusion_info$total_rapid_changes / confusion_info$affected_sessions,
    confusion_info$avg_time_window
  )

  notice <- bid_notice(
    problem = problem,
    evidence = evidence,
    target_audience = NULL
  )

  return(notice)
}

#' Truncate text for display
#' @param text Text to truncate
#' @param max_length Maximum length
#' @return Truncated text
#' @keywords internal
truncate_text <- function(text, max_length) {
  if (is.null(text) || is.na(text)) {
    return("")
  }

  if (nchar(text) <= max_length) {
    return(text)
  }

  paste0(substr(text, 1, max_length - 3), "...")
}
