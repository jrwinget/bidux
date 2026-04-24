# peak-end pattern: detect sessions whose final window ends on a negative
# experience (errors concentrated in the last N seconds or an error as the
# final event). Kahneman, Fredrickson, Schreiber, and Redelmeier (1993)
# showed that memory of an experience is dominated by its peak and its end,
# so frequent negative-ending sessions are a validate-stage friction signal.
#
# threshold constants (peak_end_window_secs, peak_end_negative_rate_threshold)
# live in R/telemetry_analysis.R alongside the other pattern defaults so all
# thresholds have one source of truth and load-order stays stable.

#' Find sessions that end on a negative user experience
#'
#' @description
#' For each session, check whether any event in the final `window_secs` is
#' an error, or whether the session's very last event is an error. The
#' pattern is flagged when the rate of such sessions meets
#' `negative_rate_threshold`.
#'
#' @param events Telemetry events data frame with `timestamp`,
#'   `session_id`, `event_type`.
#' @param window_secs Width of the "end" window in seconds.
#' @param negative_rate_threshold Minimum share of sessions that must end
#'   negatively to flag the pattern.
#'
#' @return `NULL` when the event set is empty; otherwise a list with
#'   `has_issues`, `negative_end_rate`, `negative_end_count`,
#'   `total_sessions`, `window_secs`.
#' @keywords internal
#' @noRd
find_peak_end_patterns <- function(
    events,
    window_secs = peak_end_window_secs,
    negative_rate_threshold = peak_end_negative_rate_threshold) {
  if (is.null(events) || nrow(events) == 0L) {
    return(NULL)
  }

  # defensive: shiny.telemetry sometimes stores timestamps as character.
  if (!inherits(events$timestamp, "POSIXct")) {
    events$timestamp <- as.POSIXct(events$timestamp)
  }

  # drop rows with unusable session or timestamp before splitting.
  usable <- !is.na(events$session_id) & !is.na(events$timestamp)
  events <- events[usable, , drop = FALSE]
  if (nrow(events) == 0L) {
    return(NULL)
  }

  sessions <- split(events, events$session_id)
  total_sessions <- length(sessions)
  if (total_sessions == 0L) {
    return(NULL)
  }

  is_negative <- vapply(sessions, function(sess) {
    sess <- sess[order(sess$timestamp), , drop = FALSE]
    last_ts <- sess$timestamp[nrow(sess)]
    in_window <- sess[
      !is.na(sess$timestamp) &
        as.numeric(last_ts - sess$timestamp, units = "secs") <= window_secs,
      ,
      drop = FALSE
    ]
    last_type <- sess$event_type[nrow(sess)]
    last_is_error <- !is.na(last_type) && last_type == "error"
    window_has_error <- any(
      !is.na(in_window$event_type) & in_window$event_type == "error"
    )
    last_is_error || window_has_error
  }, logical(1L))

  negative_count <- sum(is_negative, na.rm = TRUE)
  negative_end_rate <- if (total_sessions > 0L) {
    negative_count / total_sessions
  } else {
    0
  }

  list(
    has_issues = negative_end_rate >= negative_rate_threshold,
    negative_end_rate = as.numeric(negative_end_rate),
    negative_end_count = as.integer(negative_count),
    total_sessions = as.integer(total_sessions),
    window_secs = as.numeric(window_secs)
  )
}
