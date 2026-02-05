#' Check if OpenTelemetry packages are available
#'
#' @description
#' Internal function to check if the required OpenTelemetry packages (otel, otelsdk)
#' are installed. Provides helpful error message if not available.
#'
#' @return NULL invisibly if packages are available, otherwise throws error
#' @keywords internal
check_otel_available <- function() {
  if (!requireNamespace("otel", quietly = TRUE)) {
    cli::cli_abort(c(
      "OpenTelemetry support requires the {.pkg otel} package.",
      "i" = "Install with: {.code install.packages('otel')}"
    ))
  }
  invisible(NULL)
}

#' Parse Unix nanosecond timestamp to POSIXct
#'
#' @description
#' Converts OTLP Unix nanosecond timestamps to R POSIXct objects.
#'
#' @param span_time_unix_nano Character string or numeric Unix timestamp in nanoseconds
#' @return POSIXct timestamp in UTC timezone, or NA if parsing fails
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # parse otlp timestamp
#' parse_span_timestamp("1234567890123456789")
#' }
parse_span_timestamp <- function(span_time_unix_nano) {
  if (is.null(span_time_unix_nano) || is.na(span_time_unix_nano)) {
    return(as.POSIXct(NA))
  }

  tryCatch(
    {
      # convert from nanoseconds to seconds
      time_seconds <- as.numeric(span_time_unix_nano) / 1e9
      as.POSIXct(time_seconds, origin = "1970-01-01", tz = "UTC")
    },
    error = function(e) {
      as.POSIXct(NA)
    }
  )
}

#' Calculate span duration in milliseconds
#'
#' @description
#' Computes the duration between span start and end times in milliseconds.
#'
#' @param start_time POSIXct start timestamp
#' @param end_time POSIXct end timestamp
#' @return Numeric duration in milliseconds, or NA if either time is missing
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' start <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
#' end <- as.POSIXct("2024-01-01 12:00:01.5", tz = "UTC")
#' calculate_span_duration_ms(start, end) # returns 1500
#' }
calculate_span_duration_ms <- function(start_time, end_time) {
  if (is.na(start_time) || is.na(end_time)) {
    return(NA_real_)
  }

  # difftime returns in seconds by default, multiply by 1000 for ms
  as.numeric(difftime(end_time, start_time, units = "secs")) * 1000
}

#' Extract session ID from span attributes
#'
#' @description
#' Extracts the session.id attribute from OTLP span attributes list.
#' Handles both nested list and data frame formats.
#'
#' @param span_attributes List or data frame of span attributes
#' @return Character session ID, or NA if not found
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # from list format
#' attrs <- list(
#'   list(key = "session.id", value = list(stringValue = "abc123"))
#' )
#' extract_session_id_from_span(attrs) # returns "abc123"
#' }
extract_session_id_from_span <- function(span_attributes) {
  if (is.null(span_attributes) || length(span_attributes) == 0) {
    return(NA_character_)
  }

  # handle data frame format (flattened attributes)
  if (is.data.frame(span_attributes)) {
    if ("session.id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["session.id"]][1]))
    }
    if ("session_id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["session_id"]][1]))
    }
  }

  # handle list format (nested otlp structure or flattened named list)
  if (is.list(span_attributes)) {
    # check if this is a named list (flattened from read_otel_json)
    if (!is.null(names(span_attributes))) {
      if ("session.id" %in% names(span_attributes)) {
        return(as.character(span_attributes[["session.id"]]))
      }
      if ("session_id" %in% names(span_attributes)) {
        return(as.character(span_attributes[["session_id"]]))
      }
    }

    # otherwise it's a list of attribute objects
    for (attr in span_attributes) {
      if (is.list(attr) && !is.null(attr$key)) {
        if (attr$key == "session.id" || attr$key == "session_id") {
          # extract value from nested structure
          if (!is.null(attr$value)) {
            if (!is.null(attr$value$stringValue)) {
              return(as.character(attr$value$stringValue))
            }
            if (!is.null(attr$value$intValue)) {
              return(as.character(attr$value$intValue))
            }
          }
        }
      }
    }
  }

  return(NA_character_)
}

#' Extract input ID from span name or attributes
#'
#' @description
#' Extracts input identifier from OTLP span. Looks in span name (e.g., "reactive:input$slider1")
#' and span attributes (e.g., input_id attribute).
#'
#' @param span_name Character span name
#' @param span_attributes List or data frame of span attributes
#' @return Character input ID, or NA if not found
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # extract from span name
#' extract_input_id_from_span("reactive:input$slider1", NULL) # returns "slider1"
#'
#' # extract from attributes
#' attrs <- list(list(key = "input_id", value = list(stringValue = "text1")))
#' extract_input_id_from_span("reactive", attrs) # returns "text1"
#' }
extract_input_id_from_span <- function(span_name, span_attributes) {
  # try to extract from span name first
  # patterns: "reactive:input$id", "output:id", "observe:id"
  if (!is.null(span_name) && !is.na(span_name)) {
    # look for input$ pattern
    if (grepl("input\\$", span_name)) {
      match <- regmatches(span_name, regexec("input\\$([a-zA-Z0-9_]+)", span_name))
      if (length(match[[1]]) > 1) {
        return(match[[1]][2])
      }
    }

    # look for reactive:id or observe:id pattern
    if (grepl("^(reactive|observe):", span_name)) {
      match <- regmatches(span_name, regexec("^(?:reactive|observe):([a-zA-Z0-9_]+)", span_name))
      if (length(match[[1]]) > 1) {
        return(match[[1]][2])
      }
    }
  }

  # try to extract from attributes
  if (is.null(span_attributes) || length(span_attributes) == 0) {
    return(NA_character_)
  }

  # handle data frame format
  if (is.data.frame(span_attributes)) {
    if ("input_id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["input_id"]][1]))
    }
    if ("widget_id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["widget_id"]][1]))
    }
  }

  # handle list format (nested otlp structure or flattened named list)
  if (is.list(span_attributes)) {
    # check if this is a named list (flattened from read_otel_json)
    if (!is.null(names(span_attributes))) {
      for (key_name in c("input_id", "widget_id", "element_id")) {
        if (key_name %in% names(span_attributes)) {
          return(as.character(span_attributes[[key_name]]))
        }
      }
    }

    # otherwise it's a list of attribute objects
    for (attr in span_attributes) {
      if (is.list(attr) && !is.null(attr$key)) {
        if (attr$key %in% c("input_id", "widget_id", "element_id")) {
          if (!is.null(attr$value$stringValue)) {
            return(as.character(attr$value$stringValue))
          }
        }
      }
    }
  }

  return(NA_character_)
}

#' Extract output ID from span name or attributes
#'
#' @description
#' Extracts output identifier from OTLP span. Looks in span name (e.g., "output:plot1")
#' and span attributes (e.g., output_id attribute).
#'
#' @param span_name Character span name
#' @param span_attributes List or data frame of span attributes
#' @return Character output ID, or NA if not found
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # extract from span name
#' extract_output_id_from_span("output:plot1", NULL) # returns "plot1"
#'
#' # extract from attributes
#' attrs <- list(list(key = "output_id", value = list(stringValue = "table1")))
#' extract_output_id_from_span("output", attrs) # returns "table1"
#' }
extract_output_id_from_span <- function(span_name, span_attributes) {
  # try to extract from span name first
  # pattern: "output:id"
  if (!is.null(span_name) && !is.na(span_name)) {
    if (grepl("^output:", span_name)) {
      match <- regmatches(span_name, regexec("^output:([a-zA-Z0-9_]+)", span_name))
      if (length(match[[1]]) > 1) {
        return(match[[1]][2])
      }
    }
  }

  # try to extract from attributes
  if (is.null(span_attributes) || length(span_attributes) == 0) {
    return(NA_character_)
  }

  # handle data frame format
  if (is.data.frame(span_attributes)) {
    if ("output_id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["output_id"]][1]))
    }
    if ("target_id" %in% names(span_attributes)) {
      return(as.character(span_attributes[["target_id"]][1]))
    }
  }

  # handle list format (nested otlp structure or flattened named list)
  if (is.list(span_attributes)) {
    # check if this is a named list (flattened from read_otel_json)
    if (!is.null(names(span_attributes))) {
      for (key_name in c("output_id", "target_id", "output", "output.name")) {
        if (key_name %in% names(span_attributes)) {
          return(as.character(span_attributes[[key_name]]))
        }
      }
    }

    # otherwise it's a list of attribute objects
    for (attr in span_attributes) {
      if (is.list(attr) && !is.null(attr$key)) {
        if (attr$key %in% c("output_id", "target_id", "output", "output.name")) {
          if (!is.null(attr$value$stringValue)) {
            return(as.character(attr$value$stringValue))
          }
        }
      }
    }
  }

  return(NA_character_)
}

#' Extract navigation ID from span attributes
#'
#' @description
#' Extracts navigation identifier from OTLP span attributes.
#'
#' @param span_attributes List or data frame of span attributes
#' @return Character navigation ID, or NA if not found
#' @keywords internal
extract_navigation_id_from_span <- function(span_attributes) {
  if (is.null(span_attributes) || length(span_attributes) == 0) {
    return(NA_character_)
  }

  # handle data frame format
  if (is.data.frame(span_attributes)) {
    for (key_name in c("navigation_id", "navigation.target", "page", "target")) {
      if (key_name %in% names(span_attributes)) {
        return(as.character(span_attributes[[key_name]][1]))
      }
    }
  }

  # handle list format (nested otlp structure or flattened named list)
  if (is.list(span_attributes)) {
    # check if this is a named list (flattened from read_otel_json)
    if (!is.null(names(span_attributes))) {
      for (key_name in c("navigation_id", "navigation.target", "page", "target")) {
        if (key_name %in% names(span_attributes)) {
          return(as.character(span_attributes[[key_name]]))
        }
      }
    }

    # otherwise it's a list of attribute objects
    for (attr in span_attributes) {
      if (is.list(attr) && !is.null(attr$key)) {
        if (attr$key %in% c("navigation_id", "navigation.target", "page", "target")) {
          if (!is.null(attr$value$stringValue)) {
            return(as.character(attr$value$stringValue))
          }
        }
      }
    }
  }

  return(NA_character_)
}

#' Extract error message from span events
#'
#' @description
#' Extracts error messages from OTLP span events. Error events contain
#' the error details in their attributes.
#'
#' @param span_events List or data frame of span events
#' @return Character error message, or NA if no error found
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' events <- list(
#'   list(
#'     name = "error",
#'     attributes = list(
#'       list(key = "message", value = list(stringValue = "Division by zero"))
#'     )
#'   )
#' )
#' extract_error_message_from_span(events) # returns "Division by zero"
#' }
extract_error_message_from_span <- function(span_events) {
  if (is.null(span_events) || length(span_events) == 0) {
    return(NA_character_)
  }

  # handle list format
  if (is.list(span_events)) {
    for (event in span_events) {
      if (is.list(event) && !is.null(event$name)) {
        if (event$name == "error" || event$name == "exception") {
          # look for message in attributes
          if (!is.null(event$attributes)) {
            for (attr in event$attributes) {
              if (is.list(attr) && !is.null(attr$key)) {
                if (attr$key %in% c("message", "error.message", "exception.message")) {
                  if (!is.null(attr$value$stringValue)) {
                    return(as.character(attr$value$stringValue))
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  # handle data frame format
  if (is.data.frame(span_events)) {
    error_events <- span_events[span_events$name %in% c("error", "exception"), ]
    if (nrow(error_events) > 0) {
      # look for message column (various naming conventions)
      for (col_name in c("message", "error_message", "error.message", "exception.message")) {
        if (col_name %in% names(error_events)) {
          msg <- error_events[[col_name]][1]
          if (!is.na(msg) && nchar(trimws(msg)) > 0) {
            return(as.character(msg))
          }
        }
      }
    }
  }

  return(NA_character_)
}

#' Convert OTLP spans to bidux event schema
#'
#' @description
#' Converts OpenTelemetry Protocol (OTLP) span data to the bidux telemetry event schema.
#' This enables transparent compatibility with existing friction detection algorithms.
#' This function is called automatically by [bid_telemetry()] and [bid_ingest_telemetry()]
#' when OTLP data is detected - you rarely need to call it directly.
#'
#' **Automatic Format Detection**: When you pass OTLP JSON or SQLite to [bid_telemetry()],
#' this conversion happens automatically. The same UX friction detection algorithms work
#' seamlessly on both shiny.telemetry events and OpenTelemetry spans.
#'
#' **Span to Event Mapping**:
#' - session_start -> login
#' - output -> output
#' - reactive, observe -> input
#' - reactive_update -> synthetic timing events
#' - Error span events -> error
#'
#' @param spans_df Data frame of OTLP spans with columns:
#'   - name: span name (e.g., "session_start", "output:plot1")
#'   - startTimeUnixNano: start timestamp in Unix nanoseconds
#'   - endTimeUnixNano: end timestamp in Unix nanoseconds
#'   - attributes: list column with span attributes
#'   - events: list column with span events (for errors)
#'
#' @return Tibble with bidux event schema columns:
#'   - timestamp: POSIXct event timestamp
#'   - session_id: character session identifier
#'   - event_type: character event type (login, input, output, error)
#'   - input_id: character input identifier (NA for non-input events)
#'   - value: character/numeric value (NA for most otel spans)
#'   - error_message: character error message (NA for non-error events)
#'   - output_id: character output identifier (NA for non-output events)
#'   - navigation_id: character navigation identifier (NA for otel spans)
#'
#' @seealso
#' - [bid_telemetry()] for high-level telemetry analysis (automatic format detection)
#' - [bid_ingest_telemetry()] for legacy telemetry workflows
#' - \code{vignette("otel-integration")} for complete OTEL setup guide
#'
#' @examples
#' \dontrun{
#' # Typically you don't need to call this directly - use bid_telemetry() instead:
#' issues <- bid_telemetry("otel_spans.json")
#'
#' # Manual conversion (advanced use case):
#' # After reading otlp json file
#' spans <- read_otel_json("spans.json")
#' events <- convert_otel_spans_to_events(spans)
#'
#' # Verify schema compatibility
#' names(events)
#' # [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
#' # [7] "output_id" "navigation_id"
#'
#' # Now use standard friction detection
#' issues <- detect_telemetry_issues(events)
#' }
#'
#' @export
convert_otel_spans_to_events <- function(spans_df) {
  if (!is.data.frame(spans_df)) {
    cli::cli_abort(c(
      "spans_df must be a data frame",
      "i" = "Expected data frame with columns: name, startTimeUnixNano, endTimeUnixNano, attributes"
    ))
  }

  if (nrow(spans_df) == 0) {
    # return empty tibble with correct schema
    return(tibble::tibble(
      timestamp = as.POSIXct(character()),
      session_id = character(),
      event_type = character(),
      input_id = character(),
      value = character(),
      error_message = character(),
      output_id = character(),
      navigation_id = character()
    ))
  }

  # validate required columns
  required_cols <- c("name", "startTimeUnixNano")
  missing_cols <- setdiff(required_cols, names(spans_df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in spans_df",
      "i" = "Missing: {paste(missing_cols, collapse = ', ')}",
      "i" = "Required: {paste(required_cols, collapse = ', ')}"
    ))
  }

  # initialize result list (dynamically build without pre-allocation to avoid NULL entries)
  events_list <- list()

  # process each span
  for (i in seq_len(nrow(spans_df))) {
    span <- spans_df[i, ]

    # extract common fields
    span_name <- span$name
    timestamp <- parse_span_timestamp(span$startTimeUnixNano)

    # calculate duration if both timestamps available
    end_timestamp <- if ("endTimeUnixNano" %in% names(span)) {
      parse_span_timestamp(span$endTimeUnixNano)
    } else {
      as.POSIXct(NA)
    }
    duration_ms <- calculate_span_duration_ms(timestamp, end_timestamp)

    # extract attributes (handle both list and data frame formats)
    span_attrs <- if ("attributes" %in% names(span)) span$attributes[[1]] else NULL
    span_events <- if ("events" %in% names(span)) span$events[[1]] else NULL

    session_id <- extract_session_id_from_span(span_attrs)

    # determine event type and extract relevant fields
    event_type <- NA_character_
    input_id <- NA_character_
    output_id <- NA_character_
    error_message <- NA_character_
    value <- NA_character_
    navigation_id <- NA_character_

    # map span type to event type
    if (!is.null(span_name) && !is.na(span_name)) {
      if (span_name == "session_start") {
        event_type <- "login"
      } else if (span_name == "session_end") {
        event_type <- "logout"
      } else if (grepl("^output:", span_name) || span_name == "output") {
        event_type <- "output"
        output_id <- extract_output_id_from_span(span_name, span_attrs)
      } else if (span_name == "reactive_update") {
        # reactive updates indicate reactive recalculations
        # treat as synthetic timing events (could be input-driven)
        # check this before the generic reactive/observe regex
        event_type <- "reactive_update"
        input_id <- extract_input_id_from_span(span_name, span_attrs)
      } else if (grepl("^(reactive|observe)", span_name)) {
        event_type <- "input"
        input_id <- extract_input_id_from_span(span_name, span_attrs)
      } else if (span_name == "navigation") {
        event_type <- "navigation"
        navigation_id <- extract_navigation_id_from_span(span_attrs)
      }
    }

    # check for error events in span
    error_msg <- extract_error_message_from_span(span_events)
    has_error <- !is.na(error_msg)

    if (has_error) {
      # if this is an output/reactive with an error, attach error_message
      if (!is.na(event_type)) {
        error_message <- error_msg
      } else {
        # standalone error event
        event_type <- "error"
        error_message <- error_msg
      }
    }

    # skip spans with no recognizable event type
    if (is.na(event_type)) {
      next
    }

    # create primary event record
    events_list <- c(events_list, list(tibble::tibble(
      timestamp = timestamp,
      session_id = session_id,
      event_type = event_type,
      input_id = input_id,
      value = value,
      error_message = error_message,
      output_id = output_id,
      navigation_id = navigation_id,
      duration_ms = duration_ms
    )))

    # if this was an output/reactive with an error, also create a separate error event
    if (has_error && event_type %in% c("output", "input")) {
      error_event <- tibble::tibble(
        timestamp = timestamp,
        session_id = session_id,
        event_type = "error",
        input_id = if (event_type == "input") input_id else NA_character_,
        value = NA_character_,
        error_message = error_msg,
        output_id = if (event_type == "output") output_id else NA_character_,
        navigation_id = NA_character_,
        duration_ms = duration_ms
      )
      events_list <- c(events_list, list(error_event))
    }
  }

  # check if we have any events
  if (length(events_list) == 0) {
    # return empty tibble with correct schema
    return(tibble::tibble(
      timestamp = as.POSIXct(character()),
      session_id = character(),
      event_type = character(),
      input_id = character(),
      value = character(),
      error_message = character(),
      output_id = character(),
      navigation_id = character(),
      duration_ms = numeric()
    ))
  }

  # combine all events
  events_df <- dplyr::bind_rows(events_list)

  # sort by timestamp
  events_df <- events_df[order(events_df$timestamp), ]

  # ensure tibble format
  events_df <- tibble::as_tibble(events_df)

  return(events_df)
}
