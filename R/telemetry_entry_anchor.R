# entry-anchoring pattern: detect sessions where users concentrate most of
# their navigation on the page they landed on, failing to explore further.
# Tversky and Kahneman (1974) introduced anchoring as a judgment heuristic
# where an initial reference point disproportionately shapes subsequent
# behavior; for dashboards, the entry page is a salient anchor.
#
# threshold constants (entry_anchor_min_navs, entry_anchor_rate_threshold)
# live in R/telemetry_analysis.R alongside the other pattern defaults.
# `fixation_threshold` (per-session "is this session anchored?") is held
# at 0.7 as a fixed operational definition rather than a tunable.

#' Find sessions that anchor on the entry page
#'
#' @description
#' Among sessions with at least `min_navs` navigation events, flag those
#' whose first navigation id accounts for at least 70% of their total
#' navigation events. The pattern fires when the share of anchored
#' sessions meets `rate_threshold`.
#'
#' @param events Telemetry events data frame.
#' @param min_navs Minimum navigation events required for a session to
#'   count toward the denominator.
#' @param rate_threshold Minimum share of qualifying sessions that must be
#'   entry-anchored for the pattern to fire.
#'
#' @return `NULL` when no qualifying sessions exist; otherwise a list with
#'   `has_issues`, `anchored_rate`, `anchored_count`, `qualifying_sessions`,
#'   `min_navs`, `fixation_threshold`.
#' @keywords internal
#' @noRd
find_entry_anchoring <- function(
    events,
    min_navs = entry_anchor_min_navs,
    rate_threshold = entry_anchor_rate_threshold) {
  if (is.null(events) || nrow(events) == 0L) {
    return(NULL)
  }
  # some telemetry sources omit navigation_id entirely when no navigation
  # events were captured. treat that as "no navigation -> no pattern".
  if (!"navigation_id" %in% names(events)) {
    return(NULL)
  }

  nav_events <- events[
    events$event_type == "navigation" &
      !is.na(events$session_id) &
      !is.na(events$timestamp) &
      !is.na(events$navigation_id),
    ,
    drop = FALSE
  ]
  if (nrow(nav_events) == 0L) {
    return(NULL)
  }

  if (!inherits(nav_events$timestamp, "POSIXct")) {
    nav_events$timestamp <- as.POSIXct(nav_events$timestamp)
  }

  sessions <- split(nav_events, nav_events$session_id)
  qualifying <- Filter(function(s) nrow(s) >= min_navs, sessions)
  total_qualifying <- length(qualifying)
  if (total_qualifying == 0L) {
    return(NULL)
  }

  # per-session "is this session anchored?" threshold. held as a constant
  # operational definition rather than a configurable; tune instead via
  # rate_threshold (fraction of sessions that must be anchored to fire).
  fixation_threshold <- 0.7

  anchored_flags <- vapply(qualifying, function(sess) {
    sess <- sess[order(sess$timestamp), , drop = FALSE]
    first_nav_id <- sess$navigation_id[1L]
    first_count <- sum(sess$navigation_id == first_nav_id, na.rm = TRUE)
    (first_count / nrow(sess)) >= fixation_threshold
  }, logical(1L))

  anchored_count <- sum(anchored_flags, na.rm = TRUE)
  anchored_rate <- anchored_count / total_qualifying

  list(
    has_issues = anchored_rate >= rate_threshold,
    anchored_rate = as.numeric(anchored_rate),
    anchored_count = as.integer(anchored_count),
    qualifying_sessions = as.integer(total_qualifying),
    min_navs = as.integer(min_navs),
    fixation_threshold = as.numeric(fixation_threshold)
  )
}
