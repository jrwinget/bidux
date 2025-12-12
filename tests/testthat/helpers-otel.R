# ============================================================================
# OPENTELEMETRY (OTEL) TEST HELPERS
# ============================================================================
# these helpers generate realistic OTLP-formatted test data for testing
# bidux's otel integration with shiny traces exported via shiny 1.12+

#' skip tests if otel integration not available
#' @keywords internal
skip_if_no_otel <- function() {
  # check if otel-specific functions are available in bidux
  # this will be updated once agent 1 implements otel functions
  if (!exists("read_otel_json", where = asNamespace("bidux"), mode = "function")) {
    skip("otel integration not yet implemented")
  }
}

#' create mock otel spans dataframe with realistic shiny telemetry
#'
#' @description
#' generates a tibble of otel spans that mimic what shiny 1.12+ exports
#' when instrumented with opentelemetry. includes all common span types:
#' session_start, session_end, reactive, observe, output, and error events.
#'
#' @param sessions number of unique sessions to generate
#' @param reactives_per_session average number of reactive executions per session
#' @param outputs_per_session average number of output renders per session
#' @param include_errors whether to include error span events
#' @param slow_rate proportion of operations that should be slow (for perf testing)
#' @param seed random seed for reproducibility
#'
#' @return tibble with columns: trace_id, span_id, parent_span_id, name,
#'         start_time_unix_nano, end_time_unix_nano, attributes (list column),
#'         events (list column)
#'
#' @keywords internal
create_mock_otel_spans <- function(sessions = 3,
                                   reactives_per_session = 10,
                                   outputs_per_session = 5,
                                   include_errors = TRUE,
                                   slow_rate = 0.2,
                                   seed = 123) {
  set.seed(seed)

  spans <- list()

  for (session_num in seq_len(sessions)) {
    session_id <- sprintf("session_%03d", session_num)
    # generate random 32-character hex string for trace id
    hex_chars <- c('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f')
    trace_id <- paste0(sample(hex_chars, 32, replace = TRUE), collapse = '')

    # session start time (unix nanoseconds)
    base_time <- as.numeric(Sys.time()) - (sessions - session_num) * 3600
    session_start_nano <- as.character(floor(base_time * 1e9))

    # session_start span
    # generate random 16-character hex string for span id
    session_span_id <- paste0(sample(hex_chars, 16, replace = TRUE), collapse = '')
    spans[[length(spans) + 1]] <- list(
      traceId = trace_id,
      spanId = session_span_id,
      parentSpanId = NA_character_,
      name = "session_start",
      startTimeUnixNano = session_start_nano,
      endTimeUnixNano = as.character(floor(base_time * 1e9) + 1e6), # 1ms later
      attributes = list(
        list(key = "session.id", value = list(stringValue = session_id)),
        list(key = "http.method", value = list(stringValue = "GET")),
        list(key = "http.target", value = list(stringValue = "/"))
      ),
      events = list()
    )

    current_time_nano <- as.numeric(session_start_nano) + 5e9 # 5 seconds after login

    # reactive spans
    num_reactives <- max(1, round(rnorm(1, reactives_per_session, 3)))
    for (reactive_num in seq_len(num_reactives)) {
      reactive_id <- sample(c("data_filter", "selected_region", "date_range", "metric_choice"), 1)
      duration_ms <- if (runif(1) < slow_rate) rnorm(1, 500, 100) else rnorm(1, 50, 10)
      duration_ns <- as.integer(max(1e6, duration_ms * 1e6))

      # generate random 16-character hex string for span id
      reactive_span_id <- paste0(sample(hex_chars, 16, replace = TRUE), collapse = '')
      spans[[length(spans) + 1]] <- list(
        traceId = trace_id,
        spanId = reactive_span_id,
        parentSpanId = session_span_id,
        name = "reactive",
        startTimeUnixNano = as.character(floor(current_time_nano)),
        endTimeUnixNano = as.character(floor(current_time_nano + duration_ns)),
        attributes = list(
          list(key = "session.id", value = list(stringValue = session_id)),
          list(key = "reactive.label", value = list(stringValue = reactive_id)),
          list(key = "input_id", value = list(stringValue = reactive_id))
        ),
        events = list()
      )

      current_time_nano <- current_time_nano + duration_ns + runif(1, 1e8, 5e8)
    }

    # output spans
    num_outputs <- max(1, round(rnorm(1, outputs_per_session, 2)))
    for (output_num in seq_len(num_outputs)) {
      output_id <- sample(c("plot1", "table1", "summary_text", "map_view"), 1)
      duration_ms <- if (runif(1) < slow_rate) rnorm(1, 1000, 200) else rnorm(1, 200, 50)
      duration_ns <- as.integer(max(1e6, duration_ms * 1e6))

      # generate random 16-character hex string for span id
      output_span_id <- paste0(sample(hex_chars, 16, replace = TRUE), collapse = '')
      output_attrs <- list(
        list(key = "session.id", value = list(stringValue = session_id)),
        list(key = "output.name", value = list(stringValue = output_id)),
        list(key = "output_id", value = list(stringValue = output_id))
      )

      # add error event to some outputs
      output_events <- list()
      if (include_errors && runif(1) < 0.1) { # 10% error rate
        error_msg <- sample(c(
          "object 'data' not found",
          "subscript out of bounds",
          "cannot open connection",
          "non-numeric argument to binary operator"
        ), 1)
        output_events <- list(
          list(
            name = "exception",
            time_unix_nano = as.character(floor(current_time_nano + duration_ns * 0.5)),
            attributes = list(
              list(key = "exception.type", value = list(stringValue = "error")),
              list(key = "exception.message", value = list(stringValue = error_msg)),
              list(key = "error.message", value = list(stringValue = error_msg))
            )
          )
        )
      }

      spans[[length(spans) + 1]] <- list(
        traceId = trace_id,
        spanId = output_span_id,
        parentSpanId = session_span_id,
        name = paste0("output:", output_id),
        startTimeUnixNano = as.character(floor(current_time_nano)),
        endTimeUnixNano = as.character(floor(current_time_nano + duration_ns)),
        attributes = output_attrs,
        events = output_events
      )

      current_time_nano <- current_time_nano + duration_ns + runif(1, 5e8, 2e9)
    }

    # occasional navigation spans
    if (runif(1) < 0.3) {
      nav_page <- sample(c("dashboard", "analysis", "settings", "help"), 1)
      # generate random 16-character hex string for span id
      nav_span_id <- paste0(sample(hex_chars, 16, replace = TRUE), collapse = '')
      spans[[length(spans) + 1]] <- list(
        traceId = trace_id,
        spanId = nav_span_id,
        parentSpanId = session_span_id,
        name = "navigation",
        startTimeUnixNano = as.character(floor(current_time_nano)),
        endTimeUnixNano = as.character(floor(current_time_nano + 1e6)),
        attributes = list(
          list(key = "session.id", value = list(stringValue = session_id)),
          list(key = "navigation.target", value = list(stringValue = nav_page)),
          list(key = "navigation_id", value = list(stringValue = nav_page))
        ),
        events = list()
      )

      current_time_nano <- current_time_nano + 1e6 + runif(1, 1e8, 5e8)
    }

    # session_end span
    # generate random 16-character hex string for span id
    end_span_id <- paste0(sample(hex_chars, 16, replace = TRUE), collapse = '')
    spans[[length(spans) + 1]] <- list(
      traceId = trace_id,
      spanId = end_span_id,
      parentSpanId = session_span_id,
      name = "session_end",
      startTimeUnixNano = as.character(floor(current_time_nano)),
      endTimeUnixNano = as.character(floor(current_time_nano + 1e6)),
      attributes = list(
        list(key = "session.id", value = list(stringValue = session_id))
      ),
      events = list()
    )
  }

  # convert to tibble
  tibble::tibble(
    traceId = vapply(spans, function(s) s$traceId, character(1)),
    spanId = vapply(spans, function(s) s$spanId, character(1)),
    parentSpanId = vapply(spans, function(s) s$parentSpanId %||% NA_character_, character(1)),
    name = vapply(spans, function(s) s$name, character(1)),
    startTimeUnixNano = vapply(spans, function(s) s$startTimeUnixNano, character(1)),
    endTimeUnixNano = vapply(spans, function(s) s$endTimeUnixNano, character(1)),
    attributes = lapply(spans, function(s) s$attributes),
    events = lapply(spans, function(s) s$events)
  )
}

#' create temporary otlp json file from spans dataframe
#'
#' @description
#' exports spans to proper otlp json format and writes to temp file.
#' follows the opentelemetry protocol specification with resourceSpans,
#' scopeSpans, and spans hierarchy.
#'
#' @param spans_df tibble from create_mock_otel_spans()
#' @param file_path optional file path (defaults to tempfile)
#'
#' @return character file path to the created json file
#'
#' @keywords internal
create_temp_otel_json <- function(spans_df, file_path = NULL) {
  if (is.null(file_path)) {
    file_path <- tempfile(fileext = ".json")
  }

  # convert spans_df to otlp json structure
  spans_list <- lapply(seq_len(nrow(spans_df)), function(i) {
    span <- spans_df[i, ]

    # get attributes list (no longer double-nested)
    attrs <- span$attributes[[1]]

    # get events list (no longer double-nested)
    evts <- span$events[[1]]
    if (is.null(evts) || length(evts) == 0) {
      evts <- list()
    }

    list(
      traceId = span$traceId,
      spanId = span$spanId,
      parentSpanId = if (is.na(span$parentSpanId)) NULL else span$parentSpanId,
      name = span$name,
      startTimeUnixNano = span$startTimeUnixNano,
      endTimeUnixNano = span$endTimeUnixNano,
      attributes = attrs,
      events = evts
    )
  })

  # wrap in otlp structure
  otlp_structure <- list(
    resourceSpans = list(
      list(
        resource = list(
          attributes = list(
            list(key = "service.name", value = list(stringValue = "shiny-app")),
            list(key = "telemetry.sdk.language", value = list(stringValue = "r"))
          )
        ),
        scopeSpans = list(
          list(
            scope = list(
              name = "shiny",
              version = "1.12.0"
            ),
            spans = spans_list
          )
        )
      )
    )
  )

  # write to file
  jsonlite::write_json(
    otlp_structure,
    file_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  return(file_path)
}

#' create temporary otel sqlite database from spans dataframe
#'
#' @description
#' creates sqlite db with proper otel schema: spans, span_events, span_attributes tables.
#' this mimics what otel collectors export to sqlite backends.
#'
#' @param spans_df tibble from create_mock_otel_spans()
#' @param db_path optional db path (defaults to tempfile)
#'
#' @return character file path to the created sqlite database
#'
#' @keywords internal
create_temp_otel_sqlite <- function(spans_df, db_path = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    skip("dbi and rsqlite required for otel sqlite tests")
  }

  if (is.null(db_path)) {
    db_path <- tempfile(fileext = ".sqlite")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  tryCatch({
    # create spans table
    spans_table <- data.frame(
      traceId = spans_df$traceId,
      spanId = spans_df$spanId,
      parentSpanId = spans_df$parentSpanId,
      name = spans_df$name,
      startTimeUnixNano = spans_df$startTimeUnixNano,
      endTimeUnixNano = spans_df$endTimeUnixNano,
      stringsAsFactors = FALSE
    )

    DBI::dbWriteTable(con, "spans", spans_table, overwrite = TRUE)

    # create span_attributes table (flattened)
    attrs_rows <- list()
    for (i in seq_len(nrow(spans_df))) {
      span_id <- spans_df$spanId[i]
      # attributes is a list of attribute objects (no longer double-nested)
      attrs <- spans_df$attributes[[i]]

      if (!is.null(attrs) && length(attrs) > 0) {
        for (attr in attrs) {
          attrs_rows[[length(attrs_rows) + 1]] <- list(
            span_id = span_id,
            key = attr$key,
            value = attr$value$stringValue %||% attr$value$intValue %||% attr$value$doubleValue %||% NA_character_
          )
        }
      }
    }

    if (length(attrs_rows) > 0) {
      attrs_table <- data.frame(
        span_id = vapply(attrs_rows, function(r) r$span_id, character(1)),
        key = vapply(attrs_rows, function(r) r$key, character(1)),
        value = vapply(attrs_rows, function(r) as.character(r$value), character(1)),
        stringsAsFactors = FALSE
      )

      DBI::dbWriteTable(con, "span_attributes", attrs_table, overwrite = TRUE)
    }

    # create span_events table (flattened)
    events_rows <- list()
    for (i in seq_len(nrow(spans_df))) {
      span_id <- spans_df$spanId[i]
      # events is a list of event objects (no longer double-nested)
      events <- spans_df$events[[i]]

      if (!is.null(events) && length(events) > 0) {
        for (event in events) {
          event_attrs <- event$attributes
          error_msg <- NA_character_

          if (!is.null(event_attrs) && length(event_attrs) > 0) {
            for (attr in event_attrs) {
              if (attr$key %in% c("error.message", "exception.message")) {
                error_msg <- attr$value$stringValue
              }
            }
          }

          events_rows[[length(events_rows) + 1]] <- list(
            span_id = span_id,
            name = event$name,
            time_unix_nano = event$time_unix_nano,
            error_message = error_msg
          )
        }
      }
    }

    if (length(events_rows) > 0) {
      events_table <- data.frame(
        span_id = vapply(events_rows, function(r) r$span_id, character(1)),
        name = vapply(events_rows, function(r) r$name, character(1)),
        time_unix_nano = vapply(events_rows, function(r) r$time_unix_nano, character(1)),
        error_message = vapply(events_rows, function(r) r$error_message %||% NA_character_, character(1)),
        stringsAsFactors = FALSE
      )

      DBI::dbWriteTable(con, "span_events", events_table, overwrite = TRUE)
    }

    DBI::dbDisconnect(con)

  }, error = function(e) {
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
    stop(e)
  })

  return(db_path)
}

#' create shiny.telemetry format json for comparison tests
#'
#' @description
#' creates traditional shiny.telemetry json format for testing
#' backward compatibility and format detection.
#'
#' @param sessions number of sessions
#' @param file_path optional file path
#'
#' @return character file path to created json file
#'
#' @keywords internal
create_temp_shiny_telemetry_json <- function(sessions = 2, file_path = NULL) {
  if (is.null(file_path)) {
    file_path <- tempfile(fileext = ".json")
  }

  events <- create_test_telemetry_events(sessions)

  # write as json lines
  lines <- vapply(events, function(e) {
    jsonlite::toJSON(e, auto_unbox = TRUE)
  }, character(1))

  writeLines(lines, file_path)

  return(file_path)
}

#' extract attribute value from otel attributes list
#'
#' @description
#' helper to extract specific attribute value from otel attributes structure.
#' handles the nested value.stringValue / value.intValue structure.
#'
#' @param attributes list of otel attributes
#' @param key attribute key to find
#' @param default default value if not found
#'
#' @return extracted value or default
#'
#' @keywords internal
extract_otel_attribute <- function(attributes, key, default = NA_character_) {
  if (is.null(attributes) || length(attributes) == 0) {
    return(default)
  }

  for (attr in attributes) {
    if (!is.null(attr$key) && attr$key == key) {
      # try different value types
      value <- attr$value$stringValue %||%
        attr$value$intValue %||%
        attr$value$doubleValue %||%
        attr$value$boolValue

      return(if (is.null(value)) default else value)
    }
  }

  return(default)
}
