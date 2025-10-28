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
    stop("session_counts and total_sessions must be numeric", call. = FALSE)
  }

  if (length(total_sessions) != 1 || total_sessions < 0) {
    stop("total_sessions must be a single non-negative number", call. = FALSE)
  }

  if (total_sessions == 0) {
    return(rep(0, length(session_counts)))
  }

  session_counts / total_sessions
}
