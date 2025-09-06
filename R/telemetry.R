#' Ingest telemetry data and identify UX friction points
#'
#' @description
#' This function ingests telemetry data from shiny.telemetry output (SQLite or
#' JSON) and automatically identifies potential UX issues, translating them into
#' BID framework Notice stages. It returns a hybrid object that is backward-compatible
#' as a list of Notice stages while also providing enhanced functionality with
#' tidy tibble access and flags extraction.
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
#' @return A hybrid object of class c("bid_issues", "list") containing bid_stage objects
#'         for each identified issue in the "Notice" stage. The object includes:
#'         \item{Legacy list}{Named list of bid_stage objects (e.g., "unused_input_region", "delayed_interaction")}
#'         \item{issues_tbl}{Attached tidy tibble with issue metadata}
#'         \item{flags}{Global telemetry flags as named list}
#'         \item{created_at}{Timestamp when object was created}
#'         
#'         Use as_tibble() to access the tidy issues data, bid_flags() to extract flags,
#'         and legacy list access for backward compatibility.
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
  # enhanced file validation
  if (!file.exists(path)) {
    cli::cli_abort("Telemetry file not found: {path}")
  }
  
  # check file size (prevent extremely large files)
  file_info <- file.info(path)
  if (is.na(file_info$size) || file_info$size > 100 * 1024 * 1024) {  # 100MB limit
    cli::cli_abort("File size exceeds maximum limit (100MB) or cannot be accessed")
  }
  
  # validate file permissions
  if (!file.access(path, 4) == 0) {  # check read permission
    cli::cli_abort("Cannot read file: {path}. Check file permissions.")
  }
  
  # validate thresholds parameter
  if (!is.null(thresholds) && !is.list(thresholds)) {
    cli::cli_abort("thresholds parameter must be a list or NULL")
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

  # create tidy issues tibble for new API
  issues_tbl <- .create_issues_tibble(notice_issues, total_sessions, events)
  
  # extract global telemetry flags
  flags <- .flags_from_issues(issues_tbl, events, thresholds)
  
  # validate that notice_issues is a proper list before creating hybrid object
  if (!is.list(notice_issues)) {
    cli::cli_abort("Internal error: notice_issues must be a list for hybrid object creation")
  }
  
  # validate that issues_tbl is a proper tibble
  if (!tibble::is_tibble(issues_tbl)) {
    cli::cli_abort("Internal error: issues_tbl must be a tibble for hybrid object creation")
  }
  
  # validate that flags is a proper list
  if (!is.list(flags)) {
    cli::cli_abort("Internal error: flags must be a list for hybrid object creation")
  }
  
  # create hybrid object with both legacy list and new attributes
  result <- structure(
    notice_issues,
    class = c("bid_issues", "list"),
    issues_tbl = issues_tbl,
    flags = flags,
    created_at = Sys.time()
  )
  
  return(result)
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
    result$no_action_rate > 0.1 ||
      (!is.na(result$median_delay) &&
        result$median_delay > threshold_seconds) ||
      result$rate_over_threshold > 0.2
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
find_error_patterns <- function(events, threshold_rate = 0.1) {
  # filter to error events
  error_events <- events[events$event_type == "error", ]

  if (nrow(error_events) == 0) {
    return(list())
  }

  total_sessions <- length(unique(events$session_id))

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
    dplyr::filter(session_rate >= threshold_rate | count >= 5) |>
    dplyr::arrange(dplyr::desc(count))

  if (nrow(error_patterns) == 0) {
    return(list())
  }

  # find associated context (what inputs triggered errors)
  result <- lapply(seq_len(nrow(error_patterns)), function(i) {
    pattern <- error_patterns[i, ]

    # find inputs changed just before these errors
    error_sessions <- error_events[
      error_events$error_message == pattern$error_message &
        (error_events$output_id %||% "") == (pattern$output_id %||% ""),
      c("session_id", "timestamp")
    ]

    # look for inputs changed within 5 seconds before error
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
find_navigation_dropoffs <- function(events, threshold = 0.2) {
  # Filter navigation events
  nav_events <- events[events$event_type == "navigation", ]

  if (nrow(nav_events) == 0) {
    return(list())
  }

  total_sessions <- length(unique(events$session_id))

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
find_confusion_patterns <- function(events, window_seconds = 10, min_changes = 5) {
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
    input_confusion_counts >= 2
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

  # create interpret stage first, then notice stage with auto-suggested theory
  interpret <- bid_interpret(
    central_question = "How can we improve user interaction with unused inputs?"
  )
  
  notice <- bid_notice(
    previous_stage = interpret,
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

  # create interpret stage first, then notice stage
  interpret <- bid_interpret(
    central_question = "How can we reduce user interaction delays?"
  )
  
  notice <- bid_notice(
    previous_stage = interpret,
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

  # create interpret stage first, then notice stage
  interpret <- bid_interpret(
    central_question = "How can we reduce user errors and confusion?"
  )
  
  notice <- bid_notice(
    previous_stage = interpret,
    problem = problem,
    evidence = evidence_parts
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

  # create interpret stage first, then notice stage
  interpret <- bid_interpret(
    central_question = "How can we improve user navigation flow?"
  )
  
  notice <- bid_notice(
    previous_stage = interpret,
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

  # create interpret stage first, then notice stage
  interpret <- bid_interpret(
    central_question = "How can we improve user navigation flow?"
  )
  
  notice <- bid_notice(
    previous_stage = interpret,
    problem = problem,
    evidence = evidence
  )

  return(notice)
}

# ==============================================================================
# BID_ISSUES CLASS HELPER FUNCTIONS
# ==============================================================================

#' Create tidy issues tibble from notice issues list
#' @param notice_issues List of bid_stage objects from telemetry analysis
#' @param total_sessions Total number of sessions analyzed
#' @param events Raw events data frame
#' @return Tibble with structured issue metadata
#' @keywords internal
.create_issues_tibble <- function(notice_issues, total_sessions, events) {
  if (length(notice_issues) == 0) {
    return(tibble::tibble(
      issue_id = character(0),
      issue_type = character(0),
      severity = character(0),
      affected_sessions = integer(0),
      impact_rate = numeric(0),
      problem = character(0),
      evidence = character(0),
      theory = character(0),
      stage = character(0),
      created_at = as.POSIXct(character(0))
    ))
  }
  
  # extract metadata from each notice issue
  issues_data <- lapply(names(notice_issues), function(issue_key) {
    notice <- notice_issues[[issue_key]]
    
    # extract basic info from the bid_stage object
    if (is.data.frame(notice) && nrow(notice) > 0) {
      problem_text <- if ("problem" %in% names(notice)) notice$problem[1] else NA_character_
      evidence_text <- if ("evidence" %in% names(notice)) notice$evidence[1] else NA_character_
      theory_text <- if ("theory" %in% names(notice)) notice$theory[1] else NA_character_
      stage_text <- if ("stage" %in% names(notice)) notice$stage[1] else "Notice"
    } else {
      problem_text <- NA_character_
      evidence_text <- NA_character_
      theory_text <- NA_character_
      stage_text <- "Notice"
    }
    
    # infer issue type from key
    issue_type <- .classify_issue_type(issue_key)
    
    # calculate severity and impact metrics
    severity_info <- .calculate_severity_metrics(issue_key, events, total_sessions)
    
    tibble::tibble(
      issue_id = issue_key,
      issue_type = issue_type,
      severity = severity_info$severity,
      affected_sessions = severity_info$affected_sessions,
      impact_rate = severity_info$impact_rate,
      problem = problem_text,
      evidence = evidence_text,
      theory = theory_text,
      stage = stage_text,
      created_at = Sys.time()
    )
  })
  
  dplyr::bind_rows(issues_data)
}

#' Extract global telemetry flags from issues and events
#' @param issues_tbl Tidy issues tibble
#' @param events Raw events data frame  
#' @param thresholds Threshold parameters used in analysis
#' @return Named list of boolean flags
#' @keywords internal
.flags_from_issues <- function(issues_tbl, events, thresholds) {
  flags <- list(
    has_issues = nrow(issues_tbl) > 0,
    has_critical_issues = any(issues_tbl$severity == "critical", na.rm = TRUE),
    has_input_issues = any(grepl("input", issues_tbl$issue_type), na.rm = TRUE),
    has_navigation_issues = any(grepl("navigation", issues_tbl$issue_type), na.rm = TRUE),
    has_error_patterns = any(grepl("error", issues_tbl$issue_type), na.rm = TRUE),
    has_confusion_patterns = any(grepl("confusion", issues_tbl$issue_type), na.rm = TRUE),
    has_delay_issues = any(grepl("delay", issues_tbl$issue_type), na.rm = TRUE),
    session_count = length(unique(events$session_id)),
    analysis_timestamp = Sys.time()
  )
  
  # add threshold-specific flags
  flags$unused_input_threshold = thresholds$unused_input_threshold
  flags$delay_threshold_seconds = thresholds$delay_threshold_seconds
  flags$error_rate_threshold = thresholds$error_rate_threshold
  
  return(flags)
}

#' Classify issue type from issue key
#' @param issue_key String identifier for the issue
#' @return Classified issue type
#' @keywords internal
.classify_issue_type <- function(issue_key) {
  if (grepl("^unused_input", issue_key)) return("unused_input")
  if (grepl("^delayed", issue_key)) return("delayed_interaction")
  if (grepl("^error", issue_key)) return("error_pattern")
  if (grepl("^navigation", issue_key)) return("navigation_dropoff")
  if (grepl("^confusion", issue_key)) return("confusion_pattern")
  return("unknown")
}

#' Calculate severity metrics for an issue
#' @param issue_key String identifier for the issue
#' @param events Raw events data frame
#' @param total_sessions Total number of sessions
#' @return List with severity, affected_sessions, and impact_rate
#' @keywords internal
.calculate_severity_metrics <- function(issue_key, events, total_sessions) {
  # default values
  affected_sessions <- 0
  impact_rate <- 0.0
  
  # calculate metrics based on issue type
  if (grepl("^unused_input", issue_key)) {
    # for unused inputs, count sessions that never used the input
    input_id <- gsub("unused_input_", "", issue_key)
    input_id <- gsub("_", " ", input_id) # simple conversion back
    
    # secure comparison using exact match instead of regex pattern matching with user input
    input_events <- events[events$event_type == "input" & 
                          events$input_id == input_id, ]
    affected_sessions <- max(0, total_sessions - length(unique(input_events$session_id)))
    impact_rate <- if (total_sessions > 0) affected_sessions / total_sessions else 0.0
    
  } else if (grepl("^delayed", issue_key)) {
    # for delays, this affects multiple sessions
    affected_sessions <- round(total_sessions * 0.3) # estimate
    impact_rate <- 0.3
    
  } else if (grepl("^error", issue_key)) {
    # for errors, count sessions with errors
    error_events <- events[events$event_type == "error", ]
    affected_sessions <- length(unique(error_events$session_id))
    impact_rate <- if (total_sessions > 0) affected_sessions / total_sessions else 0.0
    
  } else if (grepl("^navigation", issue_key)) {
    # for navigation issues, estimate based on page visits
    nav_events <- events[events$event_type == "navigation", ]
    affected_sessions <- round(total_sessions * 0.2) # estimate
    impact_rate <- 0.2
    
  } else if (grepl("^confusion", issue_key)) {
    # for confusion patterns, count rapid change sessions
    input_events <- events[events$event_type == "input", ]
    affected_sessions <- round(length(unique(input_events$session_id)) * 0.1)
    impact_rate <- if (total_sessions > 0) affected_sessions / total_sessions else 0.0
  }
  
  # determine severity based on impact rate
  severity <- if (impact_rate >= 0.3) {
    "critical"
  } else if (impact_rate >= 0.1) {
    "high"
  } else if (impact_rate >= 0.05) {
    "medium"
  } else {
    "low"
  }
  
  list(
    severity = severity,
    affected_sessions = as.integer(affected_sessions),
    impact_rate = as.numeric(impact_rate)
  )
}

# ==============================================================================
# BID_ISSUES CLASS METHODS  
# ==============================================================================

#' Print method for bid_issues objects
#' 
#' @description
#' Displays a triage view of telemetry issues with severity-based prioritization
#' and provides a reminder about legacy list access for backward compatibility.
#' 
#' @param x A bid_issues object from bid_ingest_telemetry()
#' @param ... Additional arguments (unused)
#' @return Invisible x (for chaining)
#' @export
print.bid_issues <- function(x, ...) {
  issues_tbl <- attr(x, "issues_tbl")
  flags <- attr(x, "flags")
  created_at <- attr(x, "created_at")
  
  cli::cli_h2("BID Telemetry Issues Summary")
  
  if (nrow(issues_tbl) == 0) {
    cli::cli_alert_success("No telemetry issues detected")
    cli::cli_text("All tracked inputs are being used and no systematic problems found.")
  } else {
    # show summary stats
    cli::cli_alert_info("Found {nrow(issues_tbl)} issue{?s} from {flags$session_count} session{?s}")
    
    # group by severity for triage view
    severity_summary <- table(issues_tbl$severity)
    if ("critical" %in% names(severity_summary)) {
      cli::cli_alert_danger("Critical: {severity_summary[['critical']]} issue{?s}")
    }
    if ("high" %in% names(severity_summary)) {
      cli::cli_alert_warning("High: {severity_summary[['high']]} issue{?s}")
    }
    if ("medium" %in% names(severity_summary)) {
      cli::cli_alert_info("Medium: {severity_summary[['medium']]} issue{?s}")
    }
    if ("low" %in% names(severity_summary)) {
      cli::cli_text("Low: {severity_summary[['low']]} issue{?s}")
    }
    
    cli::cli_text("")
    
    # show top issues by severity
    top_issues <- issues_tbl[order(-match(issues_tbl$severity, c("critical", "high", "medium", "low")), 
                                   -issues_tbl$impact_rate), ][1:min(3, nrow(issues_tbl)), ]
    
    cli::cli_h3("Top Priority Issues:")
    for (i in 1:nrow(top_issues)) {
      issue <- top_issues[i, ]
      impact_pct <- round(issue$impact_rate * 100, 1)
      
      if (issue$severity == "critical") {
        cli::cli_alert_danger("{issue$issue_type}: {impact_pct}% impact ({issue$affected_sessions} sessions)")
      } else if (issue$severity == "high") {
        cli::cli_alert_warning("{issue$issue_type}: {impact_pct}% impact ({issue$affected_sessions} sessions)")
      } else {
        cli::cli_alert_info("{issue$issue_type}: {impact_pct}% impact ({issue$affected_sessions} sessions)")
      }
      
      if (!is.na(issue$problem) && nchar(issue$problem) > 0) {
        cli::cli_text("   Problem: {cli::col_silver(substr(issue$problem, 1, 80))}")
      }
    }
  }
  
  cli::cli_text("")
  cli::cli_rule()
  cli::cli_text("{cli::col_blue('Usage:')} Use {.code as_tibble()} for tidy analysis, {.code bid_flags()} for flags")
  cli::cli_text("{cli::col_silver('Legacy:')} Access as list for backward compatibility: {.code issues[[1]]}, {.code length(issues)}")
  
  if (!is.null(created_at)) {
    cli::cli_text("{cli::col_silver('Created:')} {format(created_at, '%Y-%m-%d %H:%M:%S')}")
  }
  
  invisible(x)
}

#' Convert bid_issues object to tibble
#' 
#' @description
#' Extracts the tidy issues tibble from a bid_issues object for analysis
#' and visualization. This provides a structured view of all telemetry issues
#' with metadata for prioritization and reporting.
#' 
#' @param x A bid_issues object from bid_ingest_telemetry()
#' @param ... Additional arguments (unused)
#' @return A tibble with issue metadata including severity, impact, and descriptions
#' @export
as_tibble.bid_issues <- function(x, ...) {
  issues_tbl <- attr(x, "issues_tbl")
  
  if (is.null(issues_tbl)) {
    cli::cli_abort("Invalid bid_issues object: missing issues_tbl attribute")
  }
  
  return(issues_tbl)
}

#' Extract telemetry flags from bid_issues object
#' 
#' @description
#' Extracts global telemetry flags and metadata from a bid_issues object.
#' These flags provide boolean indicators for different types of issues
#' and can be used for conditional logic in downstream BID stages.
#' 
#' @param x A bid_issues object from bid_ingest_telemetry() or any object with a flags attribute
#' @return A named list of boolean flags and metadata
#' @export
bid_flags <- function(x) {
  UseMethod("bid_flags")
}

#' @rdname bid_flags
#' @export
bid_flags.bid_issues <- function(x) {
  flags <- attr(x, "flags")
  
  if (is.null(flags)) {
    cli::cli_abort("Invalid bid_issues object: missing flags attribute")
  }
  
  return(flags)
}

#' @rdname bid_flags
#' @export
bid_flags.default <- function(x) {
  # for objects that might have flags in a different structure
  if (is.list(x) && "flags" %in% names(x)) {
    return(x$flags)
  }
  
  # check for flags attribute
  flags <- attr(x, "flags")
  if (!is.null(flags)) {
    return(flags)
  }
  
  cli::cli_abort("Object does not contain telemetry flags")
}

# ==============================================================================
# NEW CONCISE TELEMETRY API
# ==============================================================================

#' Concise telemetry analysis with tidy output
#' 
#' @description
#' Preferred modern interface for telemetry analysis. Returns a clean tibble
#' of identified issues without the legacy list structure. Use this function
#' for new workflows that don't need backward compatibility.
#' 
#' @inheritParams bid_ingest_telemetry
#' @return A tibble of class "bid_issues_tbl" with structured issue metadata
#' @export
#' @examples
#' \dontrun{
#' # Modern workflow
#' issues <- bid_telemetry("telemetry.sqlite")
#' high_priority <- issues[issues$severity %in% c("critical", "high"), ]
#' 
#' # Use with bridges for BID workflow  
#' top_issue <- issues[1, ]
#' notice <- bid_notice_issue(top_issue, previous_stage = interpret_stage)
#' }
bid_telemetry <- function(path, format = NULL, thresholds = list()) {
  # use existing ingest function but extract only the tibble
  hybrid_result <- bid_ingest_telemetry(path, format, thresholds)
  
  # extract the tidy tibble and add specific class
  issues_tbl <- attr(hybrid_result, "issues_tbl")
  class(issues_tbl) <- c("bid_issues_tbl", class(issues_tbl))
  
  # preserve flags as attribute for compatibility
  attr(issues_tbl, "flags") <- attr(hybrid_result, "flags")
  attr(issues_tbl, "created_at") <- attr(hybrid_result, "created_at")
  
  return(issues_tbl)
}

#' Create Notice stage from individual telemetry issue
#' 
#' @description
#' Bridge function that converts a single telemetry issue row into a BID Notice stage.
#' This allows seamless integration between telemetry analysis and the BID framework.
#' 
#' @param issue A single row from bid_telemetry() output or issues tibble
#' @param previous_stage Optional previous BID stage (typically from bid_interpret)
#' @param override List of values to override from the issue (problem, evidence, theory)
#' @return A bid_stage object in the Notice stage
#' @export
#' @examples
#' \dontrun{
#' issues <- bid_telemetry("data.sqlite")
#' interpret <- bid_interpret("How can we reduce user friction?")
#' 
#' # Convert first issue to Notice stage
#' notice <- bid_notice_issue(issues[1, ], previous_stage = interpret)
#' 
#' # Override problem description
#' notice <- bid_notice_issue(
#'   issues[1, ], 
#'   previous_stage = interpret,
#'   override = list(problem = "Custom problem description")
#' )
#' }
bid_notice_issue <- function(issue, previous_stage = NULL, override = list()) {
  if (!is.data.frame(issue) || nrow(issue) != 1) {
    cli::cli_abort("issue must be a single row data frame from bid_telemetry() output")
  }
  
  # extract values from issue, allowing overrides
  problem <- override$problem %||% 
    (if ("problem" %in% names(issue)) issue$problem[1] else NULL) %||% 
    "Telemetry issue identified"
  
  # Build evidence string safely
  evidence_parts <- c()
  if ("affected_sessions" %in% names(issue) && !is.na(issue$affected_sessions[1])) {
    evidence_parts <- c(evidence_parts, paste("Issue affects", issue$affected_sessions[1], "sessions"))
  }
  if ("impact_rate" %in% names(issue) && !is.na(issue$impact_rate[1])) {
    evidence_parts <- c(evidence_parts, paste0("(", round(issue$impact_rate[1] * 100, 1), "% impact)"))
  }
  
  default_evidence <- if (length(evidence_parts) > 0) paste(evidence_parts, collapse = " ") else "Telemetry issue detected"
  evidence <- override$evidence %||% 
    (if ("evidence" %in% names(issue)) issue$evidence[1] else NULL) %||% 
    default_evidence
    
  theory <- override$theory %||% 
    (if ("theory" %in% names(issue)) issue$theory[1] else NULL) %||% 
    NULL
  
  # create interpret stage if none provided
  if (is.null(previous_stage)) {
    previous_stage <- bid_interpret(
      central_question = "How can we address telemetry-identified issues?"
    )
  }
  
  notice <- bid_notice(
    previous_stage = previous_stage,
    problem = problem,
    theory = theory,
    evidence = evidence
  )
  
  return(notice)
}

#' Create multiple Notice stages from telemetry issues
#' 
#' @description
#' Bridge function that converts multiple telemetry issues into Notice stages.
#' Provides filtering and limiting options for managing large issue sets.
#' 
#' @param issues A tibble from bid_telemetry() output
#' @param filter Optional filter expression for subsetting issues (e.g., severity == "critical")
#' @param previous_stage Optional previous BID stage (typically from bid_interpret)
#' @param max_issues Maximum number of issues to convert (default: 5)
#' @param ... Additional arguments passed to bid_notice_issue()
#' @return A named list of bid_stage objects in the Notice stage
#' @export
#' @examples
#' \dontrun{
#' issues <- bid_telemetry("data.sqlite")
#' interpret <- bid_interpret("How can we reduce user friction?")
#' 
#' # Convert all critical issues
#' notices <- bid_notices(issues, filter = severity == "critical", interpret)
#' 
#' # Convert top 3 issues by impact
#' top_issues <- issues[order(-issues$impact_rate), ][1:3, ]
#' notices <- bid_notices(top_issues, previous_stage = interpret)
#' }
bid_notices <- function(issues, filter = NULL, previous_stage = NULL, max_issues = 5, ...) {
  if (!is.data.frame(issues)) {
    cli::cli_abort("issues must be a data frame from bid_telemetry() output")
  }
  
  # apply filter if provided
  if (!is.null(substitute(filter))) {
    filter_expr <- substitute(filter)
    tryCatch({
      # validate that the filter expression only contains safe operations
      expr_text <- deparse(filter_expr)
      if (grepl("system|file|eval|source|get|assign|load|save|cat|write", expr_text)) {
        cli::cli_abort("Filter expression contains potentially unsafe operations")
      }
      filter_result <- eval(filter_expr, issues, parent.frame())
      if (!is.logical(filter_result) || length(filter_result) != nrow(issues)) {
        cli::cli_abort("Filter expression must return logical vector of same length as issues data")
      }
      filtered_issues <- issues[filter_result, ]
    }, error = function(e) {
      cli::cli_abort(c(
        "Error evaluating filter expression: {e$message}",
        "i" = "Filter must be a valid logical expression using column names from issues"
      ))
    })
  } else {
    filtered_issues <- issues
  }
  
  if (nrow(filtered_issues) == 0) {
    cli::cli_warn("No issues match the specified filter")
    return(list())
  }
  
  # limit number of issues
  if (nrow(filtered_issues) > max_issues) {
    cli::cli_inform("Limiting to top {max_issues} issues (out of {nrow(filtered_issues)} matched)")
    # sort by severity then impact rate for prioritization
    severity_order <- c("critical" = 4, "high" = 3, "medium" = 2, "low" = 1)
    filtered_issues$severity_rank <- severity_order[filtered_issues$severity]
    
    # Handle impact_rate safely (may not exist or be non-numeric)
    if ("impact_rate" %in% names(filtered_issues) && is.numeric(filtered_issues$impact_rate)) {
      filtered_issues <- filtered_issues[order(-filtered_issues$severity_rank, -filtered_issues$impact_rate), ]
    } else {
      filtered_issues <- filtered_issues[order(-filtered_issues$severity_rank), ]
    }
    filtered_issues <- filtered_issues[1:max_issues, ]
  }
  
  # create interpret stage if none provided
  if (is.null(previous_stage)) {
    previous_stage <- bid_interpret(
      central_question = "How can we address multiple telemetry-identified issues?"
    )
  }
  
  # convert each issue to Notice stage
  notices <- list()
  for (i in 1:nrow(filtered_issues)) {
    issue_row <- filtered_issues[i, ]
    issue_id <- issue_row$issue_id[1] %||% paste0("issue_", i)
    
    notices[[issue_id]] <- bid_notice_issue(
      issue_row, 
      previous_stage = previous_stage, 
      ...
    )
  }
  
  return(notices)
}

#' Create Notice stage from single telemetry issue (sugar)
#' 
#' @description
#' Convenience function that combines issue selection and Notice creation in one step.
#' Useful for quick workflows where you want to address a specific issue immediately.
#' 
#' @param issue A single row from bid_telemetry() output  
#' @param previous_stage Previous BID stage (typically from bid_interpret)
#' @param ... Additional arguments passed to bid_notice_issue()
#' @return A bid_stage object in the Notice stage
#' @export
#' @examples
#' \dontrun{
#' issues <- bid_telemetry("data.sqlite")
#' interpret <- bid_interpret("How can we improve user experience?")
#' 
#' # Address the highest impact issue
#' top_issue <- issues[which.max(issues$impact_rate), ]
#' notice <- bid_address(top_issue, interpret)
#' }
bid_address <- function(issue, previous_stage, ...) {
  bid_notice_issue(issue, previous_stage, ...)
}

#' Create pipeline of Notice stages from top telemetry issues (sugar)
#' 
#' @description  
#' Convenience function that creates a pipeline of Notice stages from the highest
#' priority telemetry issues. Useful for systematic issue resolution workflows.
#' 
#' @param issues A tibble from bid_telemetry() output
#' @param previous_stage Previous BID stage (typically from bid_interpret)
#' @param max Maximum number of issues to include in pipeline (default: 3)
#' @param ... Additional arguments passed to bid_notices()
#' @return A named list of bid_stage objects in the Notice stage
#' @export
#' @examples
#' \dontrun{
#' issues <- bid_telemetry("data.sqlite")
#' interpret <- bid_interpret("How can we systematically improve UX?")
#' 
#' # Create pipeline for top 3 issues
#' notice_pipeline <- bid_pipeline(issues, interpret, max = 3)
#' 
#' # Continue with first issue in pipeline
#' anticipate <- bid_anticipate(previous_stage = notice_pipeline[[1]])
#' }
bid_pipeline <- function(issues, previous_stage, max = 3, ...) {
  if (!is.data.frame(issues)) {
    cli::cli_abort("issues must be a data frame from bid_telemetry() output")
  }
  
  # sort by priority (severity then impact)
  severity_order <- c("critical" = 4, "high" = 3, "medium" = 2, "low" = 1)
  issues$severity_rank <- severity_order[issues$severity]
  
  # Handle impact_rate safely (may not exist or be non-numeric)
  if ("impact_rate" %in% names(issues) && is.numeric(issues$impact_rate)) {
    priority_issues <- issues[order(-issues$severity_rank, -issues$impact_rate), ]
  } else {
    priority_issues <- issues[order(-issues$severity_rank), ]
  }
  
  # use bid_notices with max limit
  bid_notices(priority_issues, previous_stage = previous_stage, max_issues = max, ...)
}
