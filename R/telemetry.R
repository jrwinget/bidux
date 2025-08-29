#' Ingest telemetry data and identify UX friction points
#'
#' @description
#' This function ingests telemetry data from shiny.telemetry output (SQLite or
#' JSON) and automatically identifies potential UX issues, translating them into
#' BID framework Notice stages. It analyzes user behavior patterns to detect
#' friction points such as unused inputs, delayed interactions, frequent errors,
#' and navigation drop-offs.
#'
#' @param path File path to telemetry data (SQLite database or JSON log file)
#' @param format Optional format specification ("sqlite" or "json"). If NULL,
#'        auto-detected from file extension.
#' @param thresholds Optional list of threshold parameters:
#'        - unused_input_threshold: percentage of sessions below which input is
#'          considered unused (default: 0.05)
#'        - delay_threshold_seconds: seconds of delay considered problematic
#'          (default: 30)
#'        - error_rate_threshold: percentage of sessions with errors considered
#'          problematic (default: 0.1)
#'        - navigation_threshold: percentage of sessions visiting a page below
#'          which it's considered underused (default: 0.2)
#'        - rapid_change_window: seconds within which multiple changes indicate
#'          confusion (default: 10)
#'        - rapid_change_count: number of changes within window to flag as
#'          confusion (default: 5)
#'
#' @return A list containing bid_stage objects for each identified issue in the
#'         "Notice" stage. Each element is named by issue type (e.g.,
#'         "unused_input_region", "delayed_interaction", etc.)
#'
#' @examples
#' \dontrun{
#' # Analyze SQLite telemetry database
#' issues <- bid_ingest_telemetry("telemetry.sqlite")
#'
#' # Analyze JSON log with custom thresholds
#' issues <- bid_ingest_telemetry(
#'   "telemetry.log",
#'   format = "json",
#'   thresholds = list(
#'     unused_input_threshold = 0.1,
#'     delay_threshold_seconds = 60
#'   )
#' )
#'
#' # Use results in BID workflow
#' if (length(issues) > 0) {
#'   # Take first issue and continue with BID process
#'   interpret_result <- bid_interpret(
#'     previous_stage = issues[[1]],
#'     central_question = "How can we improve user engagement?"
#'   )
#' }
#' }
#'
#' @export
bid_ingest_telemetry <- function(
    path,
    format = NULL,
    thresholds = list()) {
  if (!file.exists(path)) {
    cli::cli_abort("Telemetry file not found: {path}")
  }

  if (is.null(format)) {
    format <- detect_telemetry_format(path)
  }

  if (!format %in% c("sqlite", "json")) {
    cli::cli_abort("Format must be 'sqlite' or 'json', got: {format}")
  }

  default_thresholds <- list(
    unused_input_threshold = 0.05,
    delay_threshold_seconds = 30,
    error_rate_threshold = 0.1,
    navigation_threshold = 0.2,
    rapid_change_window = 10,
    rapid_change_count = 5
  )

  thresholds <- modifyList(default_thresholds, thresholds)

  cli::cli_alert_info("Reading telemetry data from {format} file...")
  events <- read_telemetry_data(path, format)

  if (nrow(events) == 0) {
    cli::cli_warn("No telemetry events found in {path}")
    return(list())
  }

  # get total sessions for pct calculations
  total_sessions <- length(unique(events$session_id))
  cli::cli_alert_info(
    "Analyzing {nrow(events)} events from {total_sessions} sessions..."
  )

  notice_issues <- list()

  # find unused inputs
  unused_inputs <- find_unused_inputs(events, thresholds$unused_input_threshold)
  if (length(unused_inputs) > 0) {
    for (input_info in unused_inputs) {
      issue_key <- paste0(
        "unused_input_",
        gsub("[^a-zA-Z0-9]", "_", input_info$input_id)
      )
      notice_issues[[issue_key]] <- create_unused_input_notice(
        input_info,
        total_sessions
      )
    }
  }

  # find delayed interactions
  delay_info <- find_delayed_sessions(
    events,
    thresholds$delay_threshold_seconds
  )
  if (!is.null(delay_info) && delay_info$has_issues) {
    notice_issues[["delayed_interaction"]] <- create_delay_notice(
      delay_info,
      total_sessions,
      thresholds$delay_threshold_seconds
    )
  }

  # find error patterns
  error_patterns <- find_error_patterns(events, thresholds$error_rate_threshold)
  if (length(error_patterns) > 0) {
    for (i in seq_along(error_patterns)) {
      error_info <- error_patterns[[i]]
      issue_key <- paste0("error_", i)
      notice_issues[[issue_key]] <- create_error_notice(
        error_info,
        total_sessions
      )
    }
  }

  # find navigation drop-offs
  if ("navigation" %in% unique(events$event_type)) {
    navigation_issues <- find_navigation_dropoffs(
      events,
      thresholds$navigation_threshold
    )
    if (length(navigation_issues) > 0) {
      for (nav_info in navigation_issues) {
        issue_key <- paste0(
          "navigation_",
          gsub("[^a-zA-Z0-9]", "_", nav_info$page)
        )
        notice_issues[[issue_key]] <- create_navigation_notice(
          nav_info,
          total_sessions
        )
      }
    }
  }

  # find rapid change patterns (confusion indicators)
  confusion_patterns <- find_confusion_patterns(
    events,
    thresholds$rapid_change_window,
    thresholds$rapid_change_count
  )
  if (length(confusion_patterns) > 0) {
    for (i in seq_along(confusion_patterns)) {
      confusion_info <- confusion_patterns[[i]]
      issue_key <- paste0(
        "confusion_",
        gsub("[^a-zA-Z0-9]", "_", confusion_info$input_id)
      )
      notice_issues[[issue_key]] <- create_confusion_notice(
        confusion_info,
        total_sessions
      )
    }
  }

  # summary
  if (length(notice_issues) == 0) {
    cli::cli_alert_success(
      paste(
        "No significant UX issues identified from telemetry. All tracked",
        "inputs were used and no systematic problems detected."
      )
    )
  } else {
    cli::cli_alert_warning(
      paste(
        "Identified {length(notice_issues)} potential UX issue{?s} from",
        "telemetry analysis"
      )
    )
  }

  return(notice_issues)
}

#' Auto-detect telemetry format from file extension
#' @param path File path
#' @return Format string ("sqlite" or "json")
#' @keywords internal
detect_telemetry_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("sqlite", "sqlite3", "db")) {
    return("sqlite")
  } else if (ext %in% c("json", "log", "txt")) {
    return("json")
  } else {
    cli::cli_abort(
      paste(
        "Cannot auto-detect format from extension '.{ext}'.",
        "Please specify format parameter."
      )
    )
  }
}

#' Read telemetry data from file
#' @param path File path
#' @param format Format ("sqlite" or "json")
#' @return Data frame of events
#' @keywords internal
read_telemetry_data <- function(path, format) {
  if (format == "sqlite") {
    return(read_telemetry_sqlite(path))
  } else if (format == "json") {
    return(read_telemetry_json(path))
  }
}

#' Read telemetry from SQLite database
#' @param path SQLite database path
#' @return Data frame of events
#' @keywords internal
read_telemetry_sqlite <- function(path) {
  if (
    !requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Packages 'DBI' and 'RSQLite' are required to read SQLite telemetry data"
    )
  }

  con <- NULL
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), path)
      tables <- DBI::dbListTables(con)

      # look for events table (common {shiny.telemetry} table name)
      event_table <- NULL
      if ("event_data" %in% tables) {
        event_table <- "event_data"
      } else if ("events" %in% tables) {
        event_table <- "events"
      } else if (length(tables) > 0) {
        # use first table if no standard name found
        # TODO: allow user to specify table name
        event_table <- tables[1]
        cli::cli_warn(
          "No standard event table found, using '{event_table}'"
        )
      } else {
        cli::cli_abort("No tables found in SQLite database")
      }

      events <- DBI::dbReadTable(con, event_table)
      events <- normalize_telemetry_columns(events)

      return(events)
    },
    error = function(e) {
      cli::cli_abort("Error reading SQLite database: {e$message}")
    },
    finally = {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
    }
  )
}

#' Read telemetry from JSON log file
#' @param path JSON log file path
#' @return Data frame of events
#' @keywords internal
read_telemetry_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort("Package 'jsonlite' is required to read JSON telemetry data")
  }

  tryCatch(
    {
      # try to read as JSON lines (one JSON object per line)
      lines <- readLines(path, warn = FALSE)
      lines <- lines[nchar(trimws(lines)) > 0]

      if (length(lines) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # check if JSON array
      if (substr(trimws(lines[1]), 1, 1) == "[") {
        # if TRUE, parse as whole
        events <- jsonlite::fromJSON(
          paste(lines, collapse = "\n"),
          flatten = TRUE
        )
      } else {
        # if FALSE, try to parse each line as JSON
        events_list <- lapply(lines, function(line) {
          tryCatch(
            jsonlite::fromJSON(line, flatten = TRUE),
            error = function(e) NULL
          )
        })

        events_list <- events_list[!sapply(events_list, is.null)]

        if (length(events_list) == 0) {
          return(data.frame(
            timestamp = character(),
            session_id = character(),
            event_type = character(),
            stringsAsFactors = FALSE
          ))
        }

        # filter out events that don't have required fields
        required_fields <- c("timestamp", "session_id", "event_type")
        valid_events <- lapply(events_list, function(event) {
          if (is.list(event) && all(required_fields %in% names(event))) {
            return(event)
          }
          return(NULL)
        })

        valid_events <- valid_events[!sapply(valid_events, is.null)]

        if (length(valid_events) == 0) {
          cli::cli_abort("No valid events found in JSON file")
        }

        events <- dplyr::bind_rows(valid_events)
      }

      if (!is.data.frame(events)) {
        events <- as.data.frame(events)
      }

      # if empty, return empty data frame with req columns
      if (nrow(events) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # normalize column names
      events <- normalize_telemetry_columns(events)

      return(events)
    },
    error = function(e) {
      cli::cli_abort("Error reading JSON file: {e$message}")
    }
  )
}

#' Normalize telemetry column names
#' @param events Raw events data frame
#' @return Normalized data frame
#' @keywords internal
normalize_telemetry_columns <- function(events) {
  if (is.list(events) && !is.data.frame(events)) {
    # case where events is still a list
    events <- dplyr::bind_rows(events)
  }

  if (!is.data.frame(events)) {
    cli::cli_abort("Events must be a data frame")
  }

  # common name mappings
  col_mappings <- list(
    timestamp = c("timestamp", "time", "datetime", "created_at"),
    session_id = c("session_id", "session", "sessionid", "session_token"),
    event_type = c("event_type", "type", "event", "action"),
    input_id = c("input_id", "input", "widget_id", "element_id"),
    value = c("value", "input_value", "data"),
    error_message = c("error_message", "message", "error", "detail"),
    output_id = c("output_id", "output", "target_id"),
    navigation_id = c("navigation_id", "page", "tab", "panel")
  )

  # normalize column names
  for (target_col in names(col_mappings)) {
    if (!target_col %in% names(events)) {
      for (alt_name in col_mappings[[target_col]]) {
        if (alt_name %in% names(events)) {
          names(events)[names(events) == alt_name] <- target_col
          break
        }
      }
    }
  }

  # req columns
  required_cols <- c("timestamp", "session_id", "event_type")

  if (all(required_cols %in% names(events))) {
    valid_rows <- complete.cases(events[, required_cols])

    for (col in required_cols) {
      if (is.character(events[[col]])) {
        valid_rows <- valid_rows & nchar(trimws(events[[col]])) > 0
      }
    }

    events <- events[valid_rows, ]

    if (nrow(events) == 0) {
      cli::cli_abort("No valid events found after filtering")
    }
  } else {
    missing_cols <- setdiff(required_cols, names(events))
    cli::cli_abort(
      "Required columns missing from telemetry data: {missing_cols}"
    )
  }

  if (is.character(events$timestamp)) {
    events$timestamp <- as.POSIXct(
      events$timestamp,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )

    if (any(is.na(events$timestamp))) {
      events$timestamp <- as.POSIXct(events$timestamp, tz = "UTC")
    }
  }

  events <- events[order(events$timestamp), ]

  return(events)
}


# ==============================================================================
# TELEMETRY ANALYSIS FUNCTIONS
# ==============================================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Find unused or under-used inputs
#' @param events Telemetry events data frame
#' @param threshold Percentage threshold for considering input unused
#' @return List of unused input information
#' @keywords internal
find_unused_inputs <- function(events, threshold = 0.05) {
  input_events <- events[events$event_type == "input", ]

  if (nrow(input_events) == 0) {
    return(list())
  }

  # Get total sessions
  total_sessions <- length(unique(events$session_id))

  # Count sessions per input
  input_usage <- input_events |>
    dplyr::distinct(session_id, input_id) |>
    dplyr::count(input_id, name = "sessions_used") |>
    dplyr::mutate(
      usage_rate = sessions_used / total_sessions,
      is_unused = usage_rate <= threshold
    )

  # Also find inputs that appear in UI but were never used
  # (This would require knowledge of all available inputs, which we don't have
  # from telemetry alone, so we focus on rarely used inputs)

  unused_inputs <- input_usage[input_usage$is_unused, ]

  if (nrow(unused_inputs) == 0) {
    return(list())
  }

  # Convert to list format for easier processing
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
find_delayed_sessions <- function(events, threshold_seconds = 30) {
  # Find login events
  login_events <- events[events$event_type == "login", c("session_id", "timestamp")]
  names(login_events)[2] <- "login_time"

  if (nrow(login_events) == 0) {
    return(NULL)
  }

  # Find first user action per session (input, navigation, or custom event)
  action_types <- c("input", "navigation", "custom")
  first_actions <- events[events$event_type %in% action_types, ] |>
    dplyr::group_by(session_id) |>
    dplyr::slice_min(timestamp, n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(session_id, first_action_time = timestamp, first_action_type = event_type)

  # Join login times with first actions
  session_delays <- dplyr::left_join(login_events, first_actions, by = "session_id")

  # Calculate delays
  session_delays$delay_seconds <- as.numeric(
    difftime(
      session_delays$first_action_time,
      session_delays$login_time,
      units = "secs"
    )
  )

  # Handle sessions with no actions (infinite delay)
  no_action_sessions <- sum(is.na(session_delays$delay_seconds))
  session_delays$delay_seconds[is.na(session_delays$delay_seconds)] <- Inf

  # Calculate statistics
  delays_finite <- session_delays$delay_seconds[is.finite(session_delays$delay_seconds)]

  result <- list(
    total_sessions = nrow(session_delays),
    no_action_sessions = no_action_sessions,
    no_action_rate = no_action_sessions / nrow(session_delays),
    median_delay = if (length(delays_finite) > 0) median(delays_finite) else NA,
    mean_delay = if (length(delays_finite) > 0) mean(delays_finite) else NA,
    sessions_over_threshold = sum(session_delays$delay_seconds > threshold_seconds, na.rm = TRUE),
    rate_over_threshold = sum(session_delays$delay_seconds > threshold_seconds, na.rm = TRUE) / nrow(session_delays),
    has_issues = FALSE
  )

  # Determine if there are issues
  if (result$no_action_rate > 0.1 ||
    (!is.na(result$median_delay) && result$median_delay > threshold_seconds) ||
    result$rate_over_threshold > 0.2) {
    result$has_issues <- TRUE
  }

  return(result)
}

#' Find error patterns in telemetry
#' @param events Telemetry events data frame
#' @param threshold_rate Error rate threshold
#' @return List of error patterns
#' @keywords internal
find_error_patterns <- function(events, threshold_rate = 0.1) {
  # Filter to error events
  error_events <- events[events$event_type == "error", ]

  if (nrow(error_events) == 0) {
    return(list())
  }

  total_sessions <- length(unique(events$session_id))

  # Count errors by message and output
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
    dplyr::filter(session_rate >= threshold_rate | count >= 5) |>
    dplyr::arrange(dplyr::desc(count))

  if (nrow(error_patterns) == 0) {
    return(list())
  }

  # Find associated context (what inputs triggered errors)
  result <- lapply(seq_len(nrow(error_patterns)), function(i) {
    pattern <- error_patterns[i, ]

    # Find inputs changed just before these errors
    error_sessions <- error_events[
      error_events$error_message == pattern$error_message &
        (error_events$output_id %||% "") == (pattern$output_id %||% ""),
      c("session_id", "timestamp")
    ]

    # Look for inputs changed within 5 seconds before error
    associated_inputs <- character(0)
    for (j in seq_len(nrow(error_sessions))) {
      session <- error_sessions$session_id[j]
      error_time <- error_sessions$timestamp[j]

      recent_inputs <- events[
        events$session_id == session &
          events$event_type == "input" &
          events$timestamp >= (error_time - 5) &
          events$timestamp < error_time,
        "input_id",
        drop = FALSE
      ]

      if (!is.null(recent_inputs) && nrow(recent_inputs) > 0) {
        associated_inputs <- c(associated_inputs, recent_inputs$input_id)
      }
    }

    # Get most common associated input
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
find_navigation_dropoffs <- function(events, threshold = 0.2) {
  # Filter navigation events
  nav_events <- events[events$event_type == "navigation", ]

  if (nrow(nav_events) == 0) {
    return(list())
  }

  total_sessions <- length(unique(events$session_id))

  # Count page visits
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

  # Analyze exit patterns
  result <- lapply(seq_len(nrow(page_visits)), function(i) {
    page <- page_visits$navigation_id[i]

    # Find sessions that ended on this page
    page_sessions <- nav_events[nav_events$navigation_id == page, "session_id", drop = FALSE]

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
find_confusion_patterns <- function(events, window_seconds = 10, min_changes = 5) {
  # Filter to input events
  input_events <- events[events$event_type == "input", ]

  if (nrow(input_events) == 0) {
    return(list())
  }

  # Group by session and input
  confusion_patterns <- list()

  sessions <- unique(input_events$session_id)
  for (session in sessions) {
    session_inputs <- input_events[input_events$session_id == session, ]

    # Check each input for rapid changes
    inputs <- unique(session_inputs$input_id)
    for (input in inputs) {
      input_changes <- session_inputs[session_inputs$input_id == input, ]

      if (nrow(input_changes) >= min_changes) {
        # Check for rapid changes using sliding window
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
            # Found confusion pattern
            confusion_patterns[[length(confusion_patterns) + 1]] <- list(
              session_id = session,
              input_id = input,
              change_count = min_changes,
              time_window = time_diff,
              timestamp = timestamps[i]
            )
            break # Only record once per input/session
          }
        }
      }
    }
  }

  # Aggregate by input to find systematic issues
  if (length(confusion_patterns) == 0) {
    return(list())
  }

  # Count occurrences by input
  input_confusion_counts <- table(
    sapply(confusion_patterns, function(x) x$input_id)
  )

  # Only return inputs with multiple confused sessions
  systematic_inputs <- names(input_confusion_counts)[input_confusion_counts >= 2]

  if (length(systematic_inputs) == 0) {
    return(list())
  }

  # Create summary for systematic confusion patterns
  result <- lapply(systematic_inputs, function(input) {
    input_patterns <- confusion_patterns[
      sapply(confusion_patterns, function(x) x$input_id == input)
    ]

    list(
      input_id = input,
      affected_sessions = length(input_patterns),
      total_rapid_changes = sum(sapply(input_patterns, function(x) x$change_count)),
      avg_time_window = mean(sapply(input_patterns, function(x) x$time_window))
    )
  })

  return(result)
}


# ==============================================================================
# TELEMETRY NOTICE CREATION FUNCTIONS
# ==============================================================================

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
    evidence = evidence
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
    evidence = evidence
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
    evidence = evidence
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
    evidence = evidence
  )

  return(notice)
}
