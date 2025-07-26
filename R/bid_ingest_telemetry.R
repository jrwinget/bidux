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
  # Validate inputs
  if (!file.exists(path)) {
    cli::cli_abort("Telemetry file not found: {path}")
  }

  # Auto-detect format if not specified
  if (is.null(format)) {
    format <- detect_telemetry_format(path)
  }

  # Validate format
  if (!format %in% c("sqlite", "json")) {
    cli::cli_abort("Format must be 'sqlite' or 'json', got: {format}")
  }

  # Set default thresholds
  default_thresholds <- list(
    unused_input_threshold = 0.05,
    delay_threshold_seconds = 30,
    error_rate_threshold = 0.1,
    navigation_threshold = 0.2,
    rapid_change_window = 10,
    rapid_change_count = 5
  )

  # Merge user thresholds with defaults
  thresholds <- modifyList(default_thresholds, thresholds)

  # Read telemetry data
  cli::cli_alert_info("Reading telemetry data from {format} file...")
  events <- read_telemetry_data(path, format)

  if (nrow(events) == 0) {
    cli::cli_warn("No telemetry events found in {path}")
    return(list())
  }

  # Get total sessions for percentage calculations
  total_sessions <- length(unique(events$session_id))
  cli::cli_alert_info(
    "Analyzing {nrow(events)} events from {total_sessions} sessions..."
  )

  # Initialize results list
  notice_issues <- list()

  # 1. Find unused inputs
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

  # 2. Find delayed interactions
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

  # 3. Find error patterns
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

  # 4. Find navigation drop-offs
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

  # 5. Find rapid change patterns (confusion indicators)
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

  # Summary message
  if (length(notice_issues) == 0) {
    cli::cli_alert_success(
      "No significant UX issues identified from telemetry. All tracked inputs were used and no systematic problems detected."
    )
  } else {
    cli::cli_alert_warning(
      "Identified {length(notice_issues)} potential UX issue{?s} from telemetry analysis"
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
      "Cannot auto-detect format from extension '.{ext}'. Please specify format parameter."
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
  # Check for required packages
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
      # Connect to database
      con <- DBI::dbConnect(RSQLite::SQLite(), path)

      # List available tables
      tables <- DBI::dbListTables(con)

      # Look for events table (common shiny.telemetry table name)
      event_table <- NULL
      if ("event_data" %in% tables) {
        event_table <- "event_data"
      } else if ("events" %in% tables) {
        event_table <- "events"
      } else if (length(tables) > 0) {
        # Use first table if no standard name found
        event_table <- tables[1]
        cli::cli_warn(
          "No standard event table found, using '{event_table}'"
        )
      } else {
        cli::cli_abort("No tables found in SQLite database")
      }

      # Read events
      events <- DBI::dbReadTable(con, event_table)

      # Normalize column names
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
      # Try to read as JSON lines (one JSON object per line)
      lines <- readLines(path, warn = FALSE)

      # Filter out empty lines
      lines <- lines[nchar(trimws(lines)) > 0]

      if (length(lines) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Check if it's a JSON array
      if (substr(trimws(lines[1]), 1, 1) == "[") {
        # It's a JSON array, parse as whole
        events <- jsonlite::fromJSON(
          paste(lines, collapse = "\n"),
          flatten = TRUE
        )
      } else {
        # Try to parse each line as JSON
        events_list <- lapply(lines, function(line) {
          tryCatch(
            jsonlite::fromJSON(line, flatten = TRUE),
            error = function(e) NULL
          )
        })

        # Remove failed parses
        events_list <- events_list[!sapply(events_list, is.null)]

        if (length(events_list) == 0) {
          return(data.frame(
            timestamp = character(),
            session_id = character(),
            event_type = character(),
            stringsAsFactors = FALSE
          ))
        }

        # Combine all events into data frame
        events <- dplyr::bind_rows(events_list)
      }

      if (!is.data.frame(events)) {
        events <- as.data.frame(events)
      }

      # If empty, return empty data frame with required columns
      if (nrow(events) == 0) {
        return(data.frame(
          timestamp = character(),
          session_id = character(),
          event_type = character(),
          stringsAsFactors = FALSE
        ))
      }

      # Normalize column names
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
  # Ensure required columns exist
  required_cols <- c("timestamp", "session_id", "event_type")

  # Common column name mappings
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

  # Normalize column names
  for (target_col in names(col_mappings)) {
    if (!target_col %in% names(events)) {
      # Look for alternative names
      for (alt_name in col_mappings[[target_col]]) {
        if (alt_name %in% names(events)) {
          names(events)[names(events) == alt_name] <- target_col
          break
        }
      }
    }
  }

  # Check for required columns
  missing_cols <- setdiff(required_cols, names(events))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Required columns missing from telemetry data: {missing_cols}"
    )
  }

  # Convert timestamp to POSIXct if needed
  if (is.character(events$timestamp)) {
    events$timestamp <- as.POSIXct(
      events$timestamp,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )

    # Try alternative formats if needed
    if (any(is.na(events$timestamp))) {
      events$timestamp <- as.POSIXct(events$timestamp, tz = "UTC")
    }
  }

  # Sort by timestamp
  events <- events[order(events$timestamp), ]

  return(events)
}
