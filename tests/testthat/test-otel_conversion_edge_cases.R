# ----------------------------------------------------------------------------
# PARSE_SPAN_TIMESTAMP
# ----------------------------------------------------------------------------

test_that("parse_span_timestamp handles NULL input", {
  result <- bidux:::parse_span_timestamp(NULL)
  expect_true(is.na(result))
  expect_s3_class(result, "POSIXct")
})

test_that("parse_span_timestamp handles NA input", {
  result <- bidux:::parse_span_timestamp(NA)
  expect_true(is.na(result))
})

test_that("parse_span_timestamp converts valid Unix nanoseconds", {
  # Known timestamp: 2025-01-01 00:00:00 UTC
  unix_nano <- "1735689600000000000"
  result <- bidux:::parse_span_timestamp(unix_nano)

  expect_s3_class(result, "POSIXct")
  expect_false(is.na(result))
})

test_that("parse_span_timestamp handles invalid input gracefully", {
  result <- bidux:::parse_span_timestamp("not_a_number")
  expect_true(is.na(result))
})

# ----------------------------------------------------------------------------
# CALCULATE_SPAN_DURATION_MS
# ----------------------------------------------------------------------------

test_that("calculate_span_duration_ms returns NA for NA inputs", {
  result1 <- bidux:::calculate_span_duration_ms(as.POSIXct(NA), Sys.time())
  result2 <- bidux:::calculate_span_duration_ms(Sys.time(), as.POSIXct(NA))
  result3 <- bidux:::calculate_span_duration_ms(as.POSIXct(NA), as.POSIXct(NA))

  expect_true(is.na(result1))
  expect_true(is.na(result2))
  expect_true(is.na(result3))
})

test_that("calculate_span_duration_ms calculates correctly", {
  start <- as.POSIXct("2025-01-01 12:00:00", tz = "UTC")
  end <- as.POSIXct("2025-01-01 12:00:01.5", tz = "UTC")

  result <- bidux:::calculate_span_duration_ms(start, end)

  expect_equal(result, 1500, tolerance = 1)
})

# ----------------------------------------------------------------------------
# EXTRACT_SESSION_ID_FROM_SPAN
# ----------------------------------------------------------------------------

test_that("extract_session_id_from_span handles NULL attributes", {
  result <- bidux:::extract_session_id_from_span(NULL)
  expect_true(is.na(result))
})

test_that("extract_session_id_from_span handles empty list", {
  result <- bidux:::extract_session_id_from_span(list())
  expect_true(is.na(result))
})

test_that("extract_session_id_from_span extracts from data frame", {
  attrs_df <- data.frame(`session.id` = "test_session", check.names = FALSE)
  result <- bidux:::extract_session_id_from_span(attrs_df)
  expect_equal(result, "test_session")
})

test_that("extract_session_id_from_span extracts session_id (underscore)", {
  attrs_df <- data.frame(session_id = "test_session")
  result <- bidux:::extract_session_id_from_span(attrs_df)
  expect_equal(result, "test_session")
})

test_that("extract_session_id_from_span extracts from named list", {
  attrs_list <- list(`session.id` = "test_session")
  result <- bidux:::extract_session_id_from_span(attrs_list)
  expect_equal(result, "test_session")
})

test_that("extract_session_id_from_span extracts from OTLP attribute list", {
  attrs_otlp <- list(
    list(key = "session.id", value = list(stringValue = "test_session")),
    list(key = "other", value = list(stringValue = "other_value"))
  )
  result <- bidux:::extract_session_id_from_span(attrs_otlp)
  expect_equal(result, "test_session")
})

test_that("extract_session_id_from_span handles intValue", {
  attrs_otlp <- list(
    list(key = "session.id", value = list(intValue = 12345))
  )
  result <- bidux:::extract_session_id_from_span(attrs_otlp)
  expect_equal(result, "12345")
})

# ----------------------------------------------------------------------------
# EXTRACT_INPUT_ID_FROM_SPAN
# ----------------------------------------------------------------------------

test_that("extract_input_id_from_span extracts from span name with input$", {
  result <- bidux:::extract_input_id_from_span("reactive:input$slider1", NULL)
  expect_equal(result, "slider1")
})

test_that("extract_input_id_from_span extracts from reactive: pattern", {
  result <- bidux:::extract_input_id_from_span("reactive:data_filter", NULL)
  expect_equal(result, "data_filter")
})

test_that("extract_input_id_from_span extracts from observe: pattern", {
  result <- bidux:::extract_input_id_from_span("observe:handler1", NULL)
  expect_equal(result, "handler1")
})

test_that("extract_input_id_from_span falls back to attributes", {
  attrs <- list(input_id = "attr_input")
  result <- bidux:::extract_input_id_from_span("other_span", attrs)
  expect_equal(result, "attr_input")
})

test_that("extract_input_id_from_span checks widget_id in data frame", {
  attrs_df <- data.frame(widget_id = "widget1")
  result <- bidux:::extract_input_id_from_span("some_span", attrs_df)
  expect_equal(result, "widget1")
})

test_that("extract_input_id_from_span handles OTLP format", {
  attrs_otlp <- list(
    list(key = "input_id", value = list(stringValue = "otlp_input"))
  )
  result <- bidux:::extract_input_id_from_span("span", attrs_otlp)
  expect_equal(result, "otlp_input")
})

test_that("extract_input_id_from_span returns NA for no match", {
  result <- bidux:::extract_input_id_from_span("unknown", list())
  expect_true(is.na(result))
})

# ----------------------------------------------------------------------------
# EXTRACT_OUTPUT_ID_FROM_SPAN
# ----------------------------------------------------------------------------

test_that("extract_output_id_from_span extracts from span name", {
  result <- bidux:::extract_output_id_from_span("output:plot1", NULL)
  expect_equal(result, "plot1")
})

test_that("extract_output_id_from_span falls back to attributes", {
  attrs <- list(output_id = "attr_output")
  result <- bidux:::extract_output_id_from_span("some_span", attrs)
  expect_equal(result, "attr_output")
})

test_that("extract_output_id_from_span checks target_id", {
  attrs_df <- data.frame(target_id = "target1")
  result <- bidux:::extract_output_id_from_span("span", attrs_df)
  expect_equal(result, "target1")
})

test_that("extract_output_id_from_span handles output.name key", {
  attrs <- list(`output.name` = "named_output")
  result <- bidux:::extract_output_id_from_span("span", attrs)
  expect_equal(result, "named_output")
})

test_that("extract_output_id_from_span handles OTLP format", {
  attrs_otlp <- list(
    list(key = "output_id", value = list(stringValue = "otlp_output"))
  )
  result <- bidux:::extract_output_id_from_span("span", attrs_otlp)
  expect_equal(result, "otlp_output")
})

# ----------------------------------------------------------------------------
# EXTRACT_NAVIGATION_ID_FROM_SPAN
# ----------------------------------------------------------------------------

test_that("extract_navigation_id_from_span handles NULL", {
  result <- bidux:::extract_navigation_id_from_span(NULL)
  expect_true(is.na(result))
})

test_that("extract_navigation_id_from_span handles empty list", {
  result <- bidux:::extract_navigation_id_from_span(list())
  expect_true(is.na(result))
})

test_that("extract_navigation_id_from_span extracts from data frame", {
  attrs_df <- data.frame(navigation_id = "dashboard")
  result <- bidux:::extract_navigation_id_from_span(attrs_df)
  expect_equal(result, "dashboard")
})

test_that("extract_navigation_id_from_span extracts navigation.target", {
  attrs <- list(`navigation.target` = "settings")
  result <- bidux:::extract_navigation_id_from_span(attrs)
  expect_equal(result, "settings")
})

test_that("extract_navigation_id_from_span extracts page key", {
  attrs <- list(page = "home")
  result <- bidux:::extract_navigation_id_from_span(attrs)
  expect_equal(result, "home")
})

test_that("extract_navigation_id_from_span extracts target key", {
  attrs <- list(target = "reports")
  result <- bidux:::extract_navigation_id_from_span(attrs)
  expect_equal(result, "reports")
})

test_that("extract_navigation_id_from_span handles OTLP format", {
  attrs_otlp <- list(
    list(key = "navigation_id", value = list(stringValue = "otlp_nav"))
  )
  result <- bidux:::extract_navigation_id_from_span(attrs_otlp)
  expect_equal(result, "otlp_nav")
})

# ----------------------------------------------------------------------------
# EXTRACT_ERROR_MESSAGE_FROM_SPAN
# ----------------------------------------------------------------------------

test_that("extract_error_message_from_span handles NULL", {
  result <- bidux:::extract_error_message_from_span(NULL)
  expect_true(is.na(result))
})

test_that("extract_error_message_from_span handles empty list", {
  result <- bidux:::extract_error_message_from_span(list())
  expect_true(is.na(result))
})

test_that("extract_error_message_from_span extracts from error event", {
  events <- list(
    list(
      name = "error",
      attributes = list(
        list(key = "message", value = list(stringValue = "Test error"))
      )
    )
  )
  result <- bidux:::extract_error_message_from_span(events)
  expect_equal(result, "Test error")
})

test_that("extract_error_message_from_span extracts from exception event", {
  events <- list(
    list(
      name = "exception",
      attributes = list(
        list(
          key = "exception.message",
          value = list(stringValue = "Exception occurred")
        )
      )
    )
  )
  result <- bidux:::extract_error_message_from_span(events)
  expect_equal(result, "Exception occurred")
})

test_that("extract_error_message_from_span extracts error.message", {
  events <- list(
    list(
      name = "error",
      attributes = list(
        list(key = "error.message", value = list(stringValue = "Error message"))
      )
    )
  )
  result <- bidux:::extract_error_message_from_span(events)
  expect_equal(result, "Error message")
})

test_that("extract_error_message_from_span handles data frame format", {
  events_df <- data.frame(
    name = c("error", "info"),
    message = c("Error in df", "Info message"),
    stringsAsFactors = FALSE
  )
  result <- bidux:::extract_error_message_from_span(events_df)
  expect_equal(result, "Error in df")
})

test_that("extract_error_message_from_span handles empty error message", {
  events_df <- data.frame(
    name = "error",
    message = "",
    stringsAsFactors = FALSE
  )
  result <- bidux:::extract_error_message_from_span(events_df)
  expect_true(is.na(result))
})

test_that("extract_error_message_from_span skips non-error events", {
  events <- list(
    list(name = "info", attributes = list()),
    list(name = "debug", attributes = list())
  )
  result <- bidux:::extract_error_message_from_span(events)
  expect_true(is.na(result))
})

# ----------------------------------------------------------------------------
# CONVERT_OTEL_SPANS_TO_EVENTS
# ----------------------------------------------------------------------------

test_that("convert_otel_spans_to_events validates input is data frame", {
  expect_error(
    bidux:::convert_otel_spans_to_events("not a data frame"),
    "must be a data frame"
  )
})

test_that("convert_otel_spans_to_events handles empty data frame", {
  empty_df <- data.frame(
    name = character(0),
    startTimeUnixNano = character(0),
    stringsAsFactors = FALSE
  )
  result <- bidux:::convert_otel_spans_to_events(empty_df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true("timestamp" %in% names(result))
})

test_that("convert_otel_spans_to_events validates required columns", {
  missing_cols_df <- data.frame(
    name = "test",
    stringsAsFactors = FALSE
  )

  expect_error(
    bidux:::convert_otel_spans_to_events(missing_cols_df),
    "Missing required columns"
  )
})

test_that("convert_otel_spans_to_events converts session_start to login", {
  spans_df <- tibble::tibble(
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "login")
})

test_that("convert_otel_spans_to_events converts session_end to logout", {
  spans_df <- tibble::tibble(
    name = "session_end",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "logout")
})

test_that("convert_otel_spans_to_events converts output spans", {
  spans_df <- tibble::tibble(
    name = "output:plot1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "output")
  expect_equal(result$output_id[1], "plot1")
})

test_that("convert_otel_spans_to_events converts reactive spans to input", {
  spans_df <- tibble::tibble(
    name = "reactive:input$slider1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "input")
  expect_equal(result$input_id[1], "slider1")
})

test_that("convert_otel_spans_to_events converts observe spans to input", {
  spans_df <- tibble::tibble(
    name = "observe:handler1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "input")
})

test_that("convert_otel_spans_to_events converts navigation spans", {
  spans_df <- tibble::tibble(
    name = "navigation",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1", navigation_id = "dashboard")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "navigation")
  expect_equal(result$navigation_id[1], "dashboard")
})

test_that("convert_otel_spans_to_events converts reactive_update", {
  spans_df <- tibble::tibble(
    name = "reactive_update",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_equal(result$event_type[1], "reactive_update")
})

test_that("convert_otel_spans_to_events creates error events from span events", {
  spans_df <- tibble::tibble(
    name = "output:plot1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list(
      list(
        name = "error",
        attributes = list(
          list(key = "message", value = list(stringValue = "Render failed"))
        )
      )
    ))
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  # Should have both output event and error event
  expect_true("error" %in% result$event_type)
  expect_true(any(result$error_message == "Render failed", na.rm = TRUE))
})

test_that("convert_otel_spans_to_events skips unrecognized spans", {
  spans_df <- tibble::tibble(
    name = c("session_start", "unknown_span_type"),
    startTimeUnixNano = c("1735689600000000000", "1735689601000000000"),
    endTimeUnixNano = c("1735689600100000000", "1735689601100000000"),
    attributes = list(list(`session.id` = "s1"), list(`session.id` = "s1")),
    events = list(list(), list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  # Should only have login event, not the unknown type
  expect_equal(nrow(result), 1)
  expect_equal(result$event_type[1], "login")
})

test_that("convert_otel_spans_to_events calculates duration_ms", {
  # 100ms duration (100,000,000 nanoseconds)
  spans_df <- tibble::tibble(
    name = "output:plot1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_true("duration_ms" %in% names(result))
  expect_equal(result$duration_ms[1], 100, tolerance = 1)
})

test_that("convert_otel_spans_to_events sorts by timestamp", {
  spans_df <- tibble::tibble(
    name = c("session_start", "output:plot1"),
    startTimeUnixNano = c("1735689601000000000", "1735689600000000000"),
    endTimeUnixNano = c("1735689601100000000", "1735689600100000000"),
    attributes = list(list(`session.id` = "s1"), list(`session.id` = "s1")),
    events = list(list(), list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  # Output should come first (earlier timestamp)
  expect_equal(result$event_type[1], "output")
  expect_equal(result$event_type[2], "login")
})

test_that("convert_otel_spans_to_events handles missing endTimeUnixNano", {
  spans_df <- tibble::tibble(
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    attributes = list(list(`session.id` = "s1")),
    events = list(list())
  )

  result <- bidux:::convert_otel_spans_to_events(spans_df)

  expect_true(is.data.frame(result))
  expect_true(is.na(result$duration_ms[1]))
})
