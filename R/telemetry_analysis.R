# ==============================================================================
# TELEMETRY ANALYSIS FUNCTIONS
# ==============================================================================

# telemetry analysis default thresholds
unused_input_threshold <- 0.05  # 5% usage rate
delay_threshold_secs <- 30  # 30 seconds
no_action_rate_threshold <- 0.1  # 10% of sessions
delay_rate_threshold <- 0.2  # 20% of sessions
error_rate_threshold <- 0.1  # 10% of sessions
min_error_count <- 5  # minimum occurrences
error_lookback_secs <- 5  # seconds before error
nav_dropoff_threshold <- 0.2  # 20% visit rate
confusion_window_secs <- 10  # rapid change window
confusion_min_changes <- 5  # minimum changes to flag
confusion_min_sessions <- 2  # minimum affected sessions

#' Find unused or under-used inputs
#' @param events Telemetry events data frame
#' @param threshold Percentage threshold for considering input unused
#' @return List of unused input information
#' @keywords internal
find_unused_inputs <- function(events, threshold = unused_input_threshold) {
  input_events <- events[events$event_type == "input", ]

  if (nrow(input_events) == 0) {
    return(list())
  }

  # get total sessions
  total_sessions <- get_total_sessions(events)

  # count sessions per input
  input_usage <- input_events |>
    dplyr::distinct(session_id, input_id) |>
    dplyr::count(input_id, name = "sessions_used") |>
    dplyr::mutate(
      usage_rate = sessions_used / total_sessions,
      is_unused = usage_rate <= threshold
    )

  # also find inputs that appear in UI but were never used
  # (this would require knowledge of all available inputs, which we don't have
  # from telemetry alone, so we focus on rarely used inputs)

  unused_inputs <- input_usage[input_usage$is_unused, ]

  if (nrow(unused_inputs) == 0) {
    return(list())
  }

  # convert to list format for easier processing
  result <- lapply(seq_len(nrow(unused_inputs)), function(i) {
    list(
      input_id = unused_inputs$input_id[i],
      sessions_used = unused_inputs$sessions_used[i],
      usage_rate = unused_inputs$usage_rate[i]
    )
  })

  return(result)
}

#' Find sessions with delayed first interaction
#' @param events Telemetry events data frame
#' @param threshold_seconds Delay threshold in seconds
#' @return List with delay statistics
#' @keywords internal
find_delayed_sessions <- function(
    events,
    threshold_seconds = delay_threshold_secs) {
  # find login events
  login_events <- events[
    events$event_type == "login",
    c("session_id", "timestamp")
  ]
  names(login_events)[2] <- "login_time"

  if (nrow(login_events) == 0) {
    return(NULL)
  }

  # find first user action per session (input, navigation, or custom event)
  action_types <- c("input", "navigation", "custom")
  first_actions <- events[events$event_type %in% action_types, ] |>
    dplyr::group_by(session_id) |>
    dplyr::slice_min(timestamp, n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(
      session_id,
      first_action_time = timestamp,
      first_action_type = event_type
    )

  # join login times with first actions
  session_delays <- dplyr::left_join(
    login_events,
    first_actions,
    by = "session_id"
  )

  # calculate delays
  session_delays$delay_seconds <- as.numeric(
    difftime(
      session_delays$first_action_time,
      session_delays$login_time,
      units = "secs"
    )
  )

  # handle sessions with no actions (infinite delay)
  no_action_sessions <- sum(is.na(session_delays$delay_seconds))
  session_delays$delay_seconds[is.na(session_delays$delay_seconds)] <- Inf

  # calculate statistics
  delays_finite <- session_delays$delay_seconds[is.finite(
    session_delays$delay_seconds
  )]

  result <- list(
    total_sessions = nrow(session_delays),
    no_action_sessions = no_action_sessions,
    no_action_rate = no_action_sessions / nrow(session_delays),
    median_delay = if (length(delays_finite) > 0) median(delays_finite) else NA,
    mean_delay = if (length(delays_finite) > 0) mean(delays_finite) else NA,
    sessions_over_threshold = sum(
      session_delays$delay_seconds > threshold_seconds,
      na.rm = TRUE
    ),
    rate_over_threshold = sum(
      session_delays$delay_seconds > threshold_seconds,
      na.rm = TRUE
    ) /
      nrow(session_delays),
    has_issues = FALSE
  )

  # determine if there are issues
  if (
    result$no_action_rate > no_action_rate_threshold ||
      (!is.na(result$median_delay) &&
        result$median_delay > threshold_seconds) ||
      result$rate_over_threshold > delay_rate_threshold
  ) {
    result$has_issues <- TRUE
  }

  return(result)
}

#' Find error patterns in telemetry
#' @param events Telemetry events data frame
#' @param threshold_rate Error rate threshold
#' @return List of error patterns
#' @keywords internal
find_error_patterns <- function(events, threshold_rate = error_rate_threshold) {
  # filter to error events
  error_events <- events[events$event_type == "error", ]

  if (nrow(error_events) == 0) {
    return(list())
  }

  total_sessions <- get_total_sessions(events)

  # count errors by message and output
  error_patterns <- error_events |>
    dplyr::group_by(error_message, output_id) |>
    dplyr::summarize(
      count = dplyr::n(),
      sessions_affected = dplyr::n_distinct(session_id),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      session_rate = sessions_affected / total_sessions
    ) |>
    dplyr::filter(session_rate >= threshold_rate | count >= min_error_count) |>
    dplyr::arrange(dplyr::desc(count))

  if (nrow(error_patterns) == 0) {
    return(list())
  }

  # find associated context (what inputs triggered errors)
  result <- lapply(seq_len(nrow(error_patterns)), function(i) {
    pattern <- error_patterns[i, ]

    # Helper for NA-safe comparison
    na_safe_equal <- function(a, b) {
      (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    }

    # find inputs changed just before these errors
    error_sessions <- error_events[
      error_events$error_message == pattern$error_message &
        na_safe_equal(error_events$output_id, pattern$output_id),
      c("session_id", "timestamp")
    ]

    # look for inputs changed within error_lookback_secs before error
    associated_inputs <- character(0)
    for (j in seq_len(nrow(error_sessions))) {
      session <- error_sessions$session_id[j]
      error_time <- error_sessions$timestamp[j]

      recent_inputs <- events[
        events$session_id == session &
          events$event_type == "input" &
          events$timestamp >= (error_time - error_lookback_secs) &
          events$timestamp < error_time,
        "input_id",
        drop = FALSE
      ]

      if (!is.null(recent_inputs) && nrow(recent_inputs) > 0) {
        associated_inputs <- c(associated_inputs, recent_inputs$input_id)
      }
    }

    # get most common associated input
    if (length(associated_inputs) > 0) {
      input_table <- table(associated_inputs)
      top_input <- names(input_table)[which.max(input_table)]
    } else {
      top_input <- NULL
    }

    list(
      error_message = pattern$error_message,
      output_id = pattern$output_id,
      count = pattern$count,
      sessions_affected = pattern$sessions_affected,
      session_rate = pattern$session_rate,
      associated_input = top_input
    )
  })

  return(result)
}

#' Find navigation drop-offs or underused pages
#' @param events Telemetry events data frame
#' @param threshold Minimum visit rate threshold
#' @return List of navigation issues
#' @keywords internal
find_navigation_dropoffs <- function(
    events,
    threshold = nav_dropoff_threshold) {
  # filter navigation events
  nav_events <- events[events$event_type == "navigation", ]

  if (nrow(nav_events) == 0) {
    return(list())
  }

  total_sessions <- get_total_sessions(events)

  # count page visits
  page_visits <- nav_events |>
    dplyr::group_by(navigation_id) |>
    dplyr::summarize(
      visit_count = dplyr::n(),
      unique_sessions = dplyr::n_distinct(session_id),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      visit_rate = unique_sessions / total_sessions
    ) |>
    dplyr::filter(visit_rate < threshold) |>
    dplyr::arrange(visit_rate)

  if (nrow(page_visits) == 0) {
    return(list())
  }

  # analyze exit patterns
  result <- lapply(seq_len(nrow(page_visits)), function(i) {
    page <- page_visits$navigation_id[i]

    # find sessions that ended on this page
    page_sessions <- nav_events[
      nav_events$navigation_id == page,
      "session_id",
      drop = FALSE
    ]

    exits_on_page <- 0
    for (session in unique(page_sessions$session_id)) {
      session_events <- events[events$session_id == session, ]
      last_nav <- session_events[
        session_events$event_type == "navigation",
        c("timestamp", "navigation_id")
      ]

      if (nrow(last_nav) > 0) {
        last_nav <- last_nav[which.max(last_nav$timestamp), ]
        if (last_nav$navigation_id == page) {
          exits_on_page <- exits_on_page + 1
        }
      }
    }

    list(
      page = page,
      visit_count = page_visits$visit_count[i],
      unique_sessions = page_visits$unique_sessions[i],
      visit_rate = page_visits$visit_rate[i],
      exit_count = exits_on_page,
      exit_rate = if (page_visits$unique_sessions[i] > 0) {
        exits_on_page / page_visits$unique_sessions[i]
      } else {
        0
      }
    )
  })

  return(result)
}

#' Find confusion patterns (rapid repeated changes)
#' @param events Telemetry events data frame
#' @param window_seconds Time window in seconds
#' @param min_changes Minimum changes to flag as confusion
#' @return List of confusion patterns
#' @keywords internal
find_confusion_patterns <- function(
    events,
    window_seconds = confusion_window_secs,
    min_changes = confusion_min_changes) {
  # filter to input events
  input_events <- events[events$event_type == "input", ]

  if (nrow(input_events) == 0) {
    return(list())
  }

  # group by session and input
  confusion_patterns <- list()

  sessions <- unique(input_events$session_id)
  for (session in sessions) {
    session_inputs <- input_events[input_events$session_id == session, ]

    # check each input for rapid changes
    inputs <- unique(session_inputs$input_id)
    for (input in inputs) {
      input_changes <- session_inputs[session_inputs$input_id == input, ]

      if (nrow(input_changes) >= min_changes) {
        # check for rapid changes using sliding window
        timestamps <- sort(input_changes$timestamp)

        for (i in seq_len(length(timestamps) - min_changes + 1)) {
          window_end <- i + min_changes - 1
          time_diff <- as.numeric(
            difftime(
              timestamps[window_end],
              timestamps[i],
              units = "secs"
            )
          )

          if (time_diff <= window_seconds) {
            # found confusion pattern
            confusion_patterns[[length(confusion_patterns) + 1]] <- list(
              session_id = session,
              input_id = input,
              change_count = min_changes,
              time_window = time_diff,
              timestamp = timestamps[i]
            )
            break # only record once per input/session
          }
        }
      }
    }
  }

  # aggregate by input to find systematic issues
  if (length(confusion_patterns) == 0) {
    return(list())
  }

  # count occurrences by input
  input_confusion_counts <- table(
    sapply(confusion_patterns, function(x) x$input_id)
  )

  # only return inputs with multiple confused sessions
  systematic_inputs <- names(input_confusion_counts)[
    input_confusion_counts >= confusion_min_sessions
  ]

  if (length(systematic_inputs) == 0) {
    return(list())
  }

  # create summary for systematic confusion patterns
  result <- lapply(systematic_inputs, function(input) {
    input_patterns <- confusion_patterns[
      sapply(confusion_patterns, function(x) x$input_id == input)
    ]

    list(
      input_id = input,
      affected_sessions = length(input_patterns),
      total_rapid_changes = sum(sapply(input_patterns, function(x) {
        x$change_count
      })),
      avg_time_window = mean(sapply(input_patterns, function(x) x$time_window))
    )
  })

  return(result)
}
