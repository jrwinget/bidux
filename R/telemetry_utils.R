#' Calculate total unique sessions from events
#'
#' @param events Data frame of telemetry events with session_id column
#'
#' @return Integer count of unique sessions
#'
#' @keywords internal
#' @noRd
get_total_sessions <- function(events) {
  if (!is.data.frame(events) || !"session_id" %in% names(events)) {
    return(0L)
  }
  length(unique(events$session_id))
}

#' NA-safe equality comparison
#'
#' @description
#' Compares two values for equality while handling NA values correctly.
#' Two NA values are considered equal.
#'
#' @param a First value
#' @param b Second value
#'
#' @return Logical indicating if a and b are equal (NA-safe)
#'
#' @keywords internal
#' @noRd
na_safe_equal <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

#' Calculate severity level from impact rate
#'
#' @description
#' Converts a numeric impact rate into a severity category using
#' configurable thresholds.
#'
#' @param impact_rate Numeric value between 0 and 1 indicating proportion
#'        of sessions/users affected
#' @param critical_threshold Threshold for critical severity (default: 0.3)
#' @param high_threshold Threshold for high severity (default: 0.1)
#' @param medium_threshold Threshold for medium severity (default: 0.05)
#'
#' @return Character string: "critical", "high", "medium", or "low"
#'
#' @examples
#' \dontrun{
#' calculate_severity(0.35) # "critical"
#' calculate_severity(0.15) # "high"
#' calculate_severity(0.07) # "medium"
#' calculate_severity(0.02) # "low"
#' }
#'
#' @keywords internal
#' @noRd
calculate_severity <- function(
    impact_rate,
    critical_threshold = 0.3,
    high_threshold = 0.1,
    medium_threshold = 0.05) {
  if (!is.numeric(impact_rate) || length(impact_rate) != 1) {
    return("unknown")
  }

  if (is.na(impact_rate) || impact_rate < 0) {
    return("unknown")
  }

  if (impact_rate >= critical_threshold) {
    return("critical")
  } else if (impact_rate >= high_threshold) {
    return("high")
  } else if (impact_rate >= medium_threshold) {
    return("medium")
  } else {
    return("low")
  }
}

#' Vectorized session rate calculation
#'
#' @description
#' Calculates the proportion of sessions for each count, handling
#' edge cases like zero total sessions.
#'
#' @param session_counts Integer vector of session counts
#' @param total_sessions Total number of sessions
#'
#' @return Numeric vector of rates (proportions) between 0 and 1
#'
#' @examples
#' \dontrun{
#' calculate_session_rates(c(10, 5, 2), 100)
#' # Returns: c(0.10, 0.05, 0.02)
#' }
#'
#' @keywords internal
#' @noRd
calculate_session_rates <- function(session_counts, total_sessions) {
  if (!is.numeric(session_counts) || !is.numeric(total_sessions)) {
    cli::cli_abort("{.arg session_counts} and {.arg total_sessions} must be numeric")
  }

  if (length(total_sessions) != 1 || total_sessions < 0) {
    cli::cli_abort("{.arg total_sessions} must be a single non-negative number")
  }

  if (total_sessions == 0) {
    return(rep(0, length(session_counts)))
  }

  session_counts / total_sessions
}

#' Add performance context from OTEL timing data
#'
#' @description
#' Enhances telemetry issue evidence with performance metrics when
#' duration_ms data is available from OpenTelemetry spans. This function
#' is backward compatible and gracefully handles events without timing data.
#'
#' @param issue_evidence Character string with existing evidence text
#' @param events Data frame of telemetry events (may include duration_ms column)
#' @param event_filter Optional expression to filter relevant events (e.g., input_id match)
#'
#' @return Enhanced evidence string with performance context appended, or
#'         original evidence if no duration data available
#'
#' @examples
#' \dontrun{
#' # add performance context for specific input
#' events_with_timing <- data.frame(
#'   event_type = "input",
#'   input_id = "slider1",
#'   duration_ms = c(120, 150, 1200, 180, 200)
#' )
#' evidence <- "Users frequently change this input"
#' add_performance_context(evidence, events_with_timing)
#' # returns: "Users frequently change this input. Average duration: 0.4s, p95: 1.1s"
#' }
#'
#' @keywords internal
#' @noRd
add_performance_context <- function(
    issue_evidence,
    events,
    event_filter = NULL) {
  # check if duration_ms column exists
  if (!is.data.frame(events) || !"duration_ms" %in% names(events)) {
    return(issue_evidence)
  }

  # apply optional filter to get relevant events (use which() to handle NAs safely)
  filtered_events <- events
  if (!is.null(event_filter)) {
    filtered_events <- events[which(event_filter), ]
  }

  # extract duration data and remove NAs
  durations <- filtered_events$duration_ms
  durations <- durations[!is.na(durations) & is.finite(durations)]

  # need at least 3 measurements for meaningful statistics
  if (length(durations) < 3) {
    return(issue_evidence)
  }

  # calculate performance metrics
  avg_duration_ms <- mean(durations)
  p95_duration_ms <- stats::quantile(durations, probs = 0.95, names = FALSE)

  # convert to user-friendly seconds format
  avg_seconds <- avg_duration_ms / 1000
  p95_seconds <- p95_duration_ms / 1000

  # format timing context based on magnitude
  if (avg_seconds < 0.1) {
    # sub-100ms - show milliseconds
    timing_text <- sprintf(
      "Average duration: %.0fms, p95: %.0fms",
      avg_duration_ms,
      p95_duration_ms
    )
  } else if (avg_seconds < 10) {
    # 0.1s to 10s - show one decimal
    timing_text <- sprintf(
      "Average duration: %.1fs, p95: %.1fs",
      avg_seconds,
      p95_seconds
    )
  } else {
    # 10s+ - show whole seconds
    timing_text <- sprintf(
      "Average duration: %.0fs, p95: %.0fs",
      round(avg_seconds),
      round(p95_seconds)
    )
  }

  # append performance context to existing evidence
  enhanced_evidence <- paste0(issue_evidence, ". ", timing_text)

  return(enhanced_evidence)
}
