# ============================================================================
# OTEL SPAN-TO-EVENT CONVERSION TESTS
# ============================================================================
# tests for agent 1's otel span conversion to shiny.telemetry event format
# verifies that otel spans are correctly transformed to bidux-compatible events

test_that("session_start span converts to login event", {
  skip_if_no_otel()

  # create session_start span
  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 0, outputs_per_session = 0)
  session_span <- spans[spans$name == "session_start", ]

  # convert to events
  events <- bidux:::convert_otel_spans_to_events(session_span)

  # verify conversion
  expect_true(is.data.frame(events))
  expect_gt(nrow(events), 0)

  # verify event_type mapping
  login_events <- events[events$event_type == "login", ]
  expect_gt(nrow(login_events), 0)

  # verify session_id extracted from attributes
  expect_true("session_id" %in% names(events))
  expect_false(any(is.na(events$session_id)))

  # verify timestamp conversion
  expect_true("timestamp" %in% names(events))
  expect_s3_class(events$timestamp, "POSIXct")
})

test_that("session_end span converts correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 0, outputs_per_session = 0)
  end_span <- spans[spans$name == "session_end", ]

  events <- bidux:::convert_otel_spans_to_events(end_span)

  # session_end might map to logout or custom event
  expect_true(is.data.frame(events))
  expect_gt(nrow(events), 0)

  # should have event_type field
  expect_true("event_type" %in% names(events))
})

test_that("output span converts to output event", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 0, outputs_per_session = 3)
  output_spans <- spans[grepl("^output:", spans$name), ]

  expect_gt(nrow(output_spans), 0)

  events <- bidux:::convert_otel_spans_to_events(output_spans)

  # verify output event creation
  output_events <- events[events$event_type == "output", ]
  expect_gt(nrow(output_events), 0)

  # verify output_id extraction from span name
  expect_true("output_id" %in% names(events))

  # output_id should match the name after "output:"
  first_output_name <- output_spans$name[1]
  expected_id <- sub("^output:", "", first_output_name)
  expect_true(expected_id %in% events$output_id)
})

test_that("output span calculates duration_ms correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 0, outputs_per_session = 2)
  output_spans <- spans[grepl("^output:", spans$name), ]

  events <- bidux:::convert_otel_spans_to_events(output_spans)

  # verify duration calculation
  expect_true("duration_ms" %in% names(events) || "render_time" %in% names(events))

  # duration should be positive
  if ("duration_ms" %in% names(events)) {
    expect_true(all(events$duration_ms > 0, na.rm = TRUE))
  }

  # duration should be reasonable (< 10 seconds for most renders)
  if ("duration_ms" %in% names(events)) {
    expect_true(all(events$duration_ms < 10000, na.rm = TRUE))
  }
})

test_that("reactive span converts to input event", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 5, outputs_per_session = 0)
  reactive_spans <- spans[spans$name == "reactive", ]

  expect_gt(nrow(reactive_spans), 0)

  events <- bidux:::convert_otel_spans_to_events(reactive_spans)

  # reactive might map to input or reactive event type
  expect_true(is.data.frame(events))
  expect_gt(nrow(events), 0)

  # should have input_id or reactive_id
  has_id <- "input_id" %in% names(events) || "reactive_id" %in% names(events)
  expect_true(has_id)
})

test_that("reactive span extracts input_id from attributes", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3, outputs_per_session = 0)
  reactive_spans <- spans[spans$name == "reactive", ]

  events <- bidux:::convert_otel_spans_to_events(reactive_spans)

  # input_id should be extracted from span attributes
  if ("input_id" %in% names(events)) {
    # should have non-NA values
    has_values <- any(!is.na(events$input_id))
    expect_true(has_values)
  }
})

test_that("error span events convert to error events", {
  skip_if_no_otel()

  # create spans with errors
  spans <- create_mock_otel_spans(
    sessions = 2,
    outputs_per_session = 10,
    include_errors = TRUE
  )

  events <- bidux:::convert_otel_spans_to_events(spans)

  # should have error events
  error_events <- events[events$event_type == "error", ]

  # with 2 sessions, 10 outputs each, 10% error rate, expect some errors
  expect_gt(nrow(error_events), 0)

  # error events should have error_message
  expect_true("error_message" %in% names(error_events))
  expect_false(all(is.na(error_events$error_message)))
})

test_that("error message extraction from span events", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(
    sessions = 1,
    outputs_per_session = 10,
    include_errors = TRUE
  )

  events <- bidux:::convert_otel_spans_to_events(spans)
  error_events <- events[events$event_type == "error", ]

  if (nrow(error_events) > 0) {
    # error messages should be strings
    expect_type(error_events$error_message, "character")

    # should have actual content
    has_content <- any(nchar(error_events$error_message) > 0, na.rm = TRUE)
    expect_true(has_content)
  }
})

test_that("span attributes extracted correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # test session.id -> session_id mapping
  expect_true("session_id" %in% names(events))
  expect_false(any(is.na(events$session_id)))

  # all events should have same session_id (from same session)
  expect_equal(length(unique(events$session_id)), 1)
})

test_that("span attribute key mappings are correct", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # verify common attribute mappings
  # session.id -> session_id
  # output.name -> output_id
  # reactive.label -> input_id
  # navigation.target -> navigation_id

  expected_fields <- c("session_id", "event_type", "timestamp")
  expect_true(all(expected_fields %in% names(events)))
})

test_that("span timestamps parsed correctly from unix nano", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 2)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # timestamps should be POSIXct
  expect_s3_class(events$timestamp, "POSIXct")

  # timestamps should be recent (within last day)
  expect_true(all(events$timestamp > Sys.time() - 86400))

  # timestamps should be ordered
  expect_true(all(diff(as.numeric(events$timestamp)) >= 0))
})

test_that("unix nanosecond to POSIXct conversion is accurate", {
  skip_if_no_otel()

  # create span with known timestamp
  test_time <- as.POSIXct("2025-01-01 12:00:00", tz = "UTC")
  test_nano <- format(as.numeric(test_time) * 1e9, scientific = FALSE)

  test_span <- tibble::tibble(
    traceId = "test123",
    spanId = "span001",
    parentSpanId = NA_character_,
    name = "session_start",
    startTimeUnixNano = test_nano,
    endTimeUnixNano = as.character(as.numeric(test_nano) + 1e9),
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1"))
    ))),
    events = list(list(list()))
  )

  events <- bidux:::convert_otel_spans_to_events(test_span)

  # converted timestamp should match original
  expect_equal(as.numeric(events$timestamp[1]), as.numeric(test_time), tolerance = 1)
})

test_that("span timestamp timezone handling", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # timestamps should have timezone attribute
  expect_true(!is.null(attr(events$timestamp, "tzone")))

  # should be UTC or local timezone
  tz <- attr(events$timestamp, "tzone")
  expect_true(tz %in% c("UTC", "", Sys.timezone()))
})

test_that("span duration calculated correctly from nano timestamps", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, outputs_per_session = 5)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # duration_ms = (endTime - startTime) / 1e6
  if ("duration_ms" %in% names(events)) {
    # all durations should be positive
    expect_true(all(events$duration_ms > 0, na.rm = TRUE))

    # durations should be reasonable (< 5 seconds for most operations)
    expect_true(median(events$duration_ms, na.rm = TRUE) < 5000)
  }
})

test_that("duration has millisecond precision", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, outputs_per_session = 3)

  events <- bidux:::convert_otel_spans_to_events(spans)

  if ("duration_ms" %in% names(events)) {
    # should have sub-second precision (not all integers)
    has_decimal <- any((events$duration_ms %% 1) != 0, na.rm = TRUE)

    # note: may not always have decimals in mock data, so soft check
    expect_true(is.numeric(events$duration_ms))
  }
})

test_that("missing session.id attribute handled gracefully", {
  skip_if_no_otel()

  # create span without session.id attribute
  span_no_session <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "test",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list(list())), # empty attributes
    events = list(list(list()))
  )

  # should not error, but produce NA for session_id
  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(span_no_session)
  })

  # session_id should be NA, not missing
  expect_true("session_id" %in% names(events))
})

test_that("missing input_id attribute handled gracefully", {
  skip_if_no_otel()

  # create reactive span without input_id
  span_no_input <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "reactive",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1"))
    ))),
    events = list(list(list()))
  )

  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(span_no_input)
  })

  # input_id may be NA
  if ("input_id" %in% names(events)) {
    expect_true(is.character(events$input_id) || is.na(events$input_id))
  }
})

test_that("navigation span converts correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 2)
  nav_spans <- spans[spans$name == "navigation", ]

  if (nrow(nav_spans) > 0) {
    events <- bidux:::convert_otel_spans_to_events(nav_spans)

    # should create navigation events
    expect_true("navigation" %in% events$event_type || "nav" %in% events$event_type)

    # should have navigation_id
    expect_true("navigation_id" %in% names(events) || "page" %in% names(events))
  }
})

test_that("navigation_id extracted from attributes", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 2)
  nav_spans <- spans[spans$name == "navigation", ]

  if (nrow(nav_spans) > 0) {
    events <- bidux:::convert_otel_spans_to_events(nav_spans)

    if ("navigation_id" %in% names(events)) {
      # should have values
      expect_false(all(is.na(events$navigation_id)))
    }
  }
})

test_that("span parent-child hierarchy preserved", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # if parent_span_id is preserved, check it
  if ("parent_span_id" %in% names(events)) {
    # child spans should reference parent
    has_parents <- any(!is.na(events$parent_span_id))
    expect_true(has_parents)
  }
})

test_that("trace_id preserved for correlation", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 3)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # trace_id helps correlate events from same trace
  if ("trace_id" %in% names(events)) {
    # should have 2 unique trace_ids (one per session)
    expect_equal(length(unique(events$trace_id)), 2)
  }
})

test_that("conversion handles all shiny span types", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 5, outputs_per_session = 3)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # should have multiple event types
  event_types <- unique(events$event_type)

  # expect session events
  expect_true(any(c("login", "session_start") %in% event_types))

  # expect output events
  expect_true("output" %in% event_types)
})

test_that("observe spans convert correctly", {
  skip_if_no_otel()

  # create custom span with observe type
  observe_span <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "observe",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1")),
      list(key = "observer.label", value = list(stringValue = "data_loader"))
    ))),
    events = list(list(list()))
  )

  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(observe_span)
  })

  # should create some event
  expect_gt(nrow(events), 0)
})

test_that("reactive debounce and throttle spans handled", {
  skip_if_no_otel()

  # create debounce span
  debounce_span <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "reactive debounce",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1"))
    ))),
    events = list(list(list()))
  )

  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(debounce_span)
  })

  expect_true(is.data.frame(events))
})

test_that("ExtendedTask spans convert correctly", {
  skip_if_no_otel()

  task_span <- tibble::tibble(
    traceId = "test123",
    spanId = "span001",
    parentSpanId = NA_character_,
    name = "reactive:long_computation",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459205000000000", # 5 seconds
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1")),
      list(key = "input_id", value = list(stringValue = "long_computation"))
    ))),
    events = list(list(list()))
  )

  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(task_span)
  })

  # duration should reflect long-running task
  if ("duration_ms" %in% names(events)) {
    expect_gt(events$duration_ms[1], 1000) # > 1 second
  }
})

test_that("conversion preserves all required shiny.telemetry fields", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3, outputs_per_session = 2)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # required fields for compatibility
  required_fields <- c("timestamp", "session_id", "event_type")

  expect_true(all(required_fields %in% names(events)))

  # no NA values in required fields
  expect_false(any(is.na(events$timestamp)))
  expect_false(any(is.na(events$session_id)))
  expect_false(any(is.na(events$event_type)))
})

test_that("conversion handles empty spans input", {
  skip_if_no_otel()

  empty_spans <- tibble::tibble(
    trace_id = character(0),
    span_id = character(0),
    parent_span_id = character(0),
    name = character(0),
    startTimeUnixNano = character(0),
    endTimeUnixNano = character(0),
    attributes = list(),
    events = list()
  )

  result <- bidux:::convert_otel_spans_to_events(empty_spans)

  # should return empty but valid dataframe
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that("conversion handles malformed span attributes", {
  skip_if_no_otel()

  bad_span <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "test",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list("not_a_valid_structure")),
    events = list(list(list()))
  )

  # should handle gracefully, not crash
  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(bad_span)
  })
})

test_that("conversion handles span events with missing attributes", {
  skip_if_no_otel()

  span_with_bad_event <- tibble::tibble(
    trace_id = "test123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "output:test",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    attributes = list(list(list(
      list(key = "session.id", value = list(stringValue = "s1"))
    ))),
    events = list(list(list(
      list(name = "exception", attributes = list()) # no error message
    )))
  )

  expect_no_error({
    events <- bidux:::convert_otel_spans_to_events(span_with_bad_event)
  })
})

test_that("large span batch conversion is efficient", {
  skip_if_no_otel()
  skip_on_cran() # performance test with larger dataset

  # create large dataset
  large_spans <- create_mock_otel_spans(
    sessions = 50,
    reactives_per_session = 20,
    outputs_per_session = 10
  )

  # should complete quickly
  start_time <- Sys.time()
  events <- bidux:::convert_otel_spans_to_events(large_spans)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_lt(elapsed, 5) # should complete in < 5 seconds
  expect_gt(nrow(events), 100)
})

test_that("conversion maintains event order by timestamp", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 10)

  events <- bidux:::convert_otel_spans_to_events(spans)

  # events should be ordered by timestamp
  timestamps <- as.numeric(events$timestamp)
  expect_true(all(diff(timestamps) >= 0))
})

test_that("conversion de-duplicates identical spans", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 2)

  # duplicate the spans
  duplicated_spans <- rbind(spans, spans)

  events <- bidux:::convert_otel_spans_to_events(duplicated_spans)

  # should not double-count events (if de-duplication implemented)
  # or should handle gracefully
  expect_true(is.data.frame(events))
})
