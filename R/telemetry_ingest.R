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
#' @param events_table Optional data.frame specifying custom events table when
#'        reading from SQLite. Must have columns: event_id, timestamp, event_type, user_id.
#'        If NULL, auto-detects standard table names (event_data, events).
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
    events_table = NULL,
    thresholds = list()) {
  # enhanced file validation
  if (!file.exists(path)) {
    cli::cli_abort("Telemetry file not found: {path}")
  }

  # check file size (prevent extremely large files)
  file_info <- file.info(path)
  if (is.na(file_info$size) || file_info$size > 100 * 1024 * 1024) { # 100MB limit
    cli::cli_abort("File size exceeds maximum limit (100MB) or cannot be accessed")
  }

  # validate file permissions
  if (!file.access(path, 4) == 0) { # check read permission
    cli::cli_abort("Cannot read file: {path}. Check file permissions.")
  }

  # validate events_table parameter
  if (!is.null(events_table)) {
    validate_data_frame(events_table, "events_table",
                       required_columns = c("event_id", "timestamp", "event_type", "user_id"))
  }

  # validate thresholds parameter
  if (!is.null(thresholds) && !is.list(thresholds)) {
    cli::cli_abort(standard_error_msg(
      "thresholds parameter must be a list or NULL",
      context = glue::glue("You provided: {class(thresholds)[1]}")
    ))
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
  events <- read_telemetry_data(path, format, events_table)

  if (nrow(events) == 0) {
    cli::cli_warn("No telemetry events found in {path}")
    return(list())
  }

  # get total sessions for pct calculations
  total_sessions <- get_total_sessions(events)
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
#' @param events_table Optional custom events table for SQLite
#' @return Data frame of events
#' @keywords internal
read_telemetry_data <- function(path, format, events_table = NULL) {
  if (format == "sqlite") {
    return(read_telemetry_sqlite(path, events_table))
  } else if (format == "json") {
    return(read_telemetry_json(path))
  }
}

#' Read telemetry from SQLite database
#' @param path SQLite database path
#' @param events_table Optional custom events table data.frame
#' @return Data frame of events
#' @keywords internal
read_telemetry_sqlite <- function(path, events_table = NULL) {
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

      # if custom events_table provided, use it directly
      if (!is.null(events_table)) {
        events <- events_table
        cli::cli_alert_info("Using provided events_table data.frame")
      } else {
        # discover tables and read from database
        tables <- DBI::dbListTables(con)

        # look for events table (common {shiny.telemetry} table name)
        event_table <- NULL
        if ("event_data" %in% tables) {
          event_table <- "event_data"
        } else if ("events" %in% tables) {
          event_table <- "events"
        } else if (length(tables) > 0) {
          # use first table if no standard name found
          event_table <- tables[1]
          cli::cli_warn(
            "No standard event table found, using '{event_table}'"
          )
        } else {
          cli::cli_abort(standard_error_msg(
            "No tables found in SQLite database",
            suggestions = "Ensure the database contains event data or provide events_table parameter"
          ))
        }

        events <- DBI::dbReadTable(con, event_table)
      }

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
      cli::cli_abort(c(
        "Error reading JSON file: {e$message}",
        "i" = "File: {path}",
        "i" = "Ensure the file contains valid JSON with required fields: timestamp, session_id, event_type"
      ))
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

  # required columns
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