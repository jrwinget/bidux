test_that("detect_telemetry_format works correctly", {
  expect_equal(detect_telemetry_format("data.sqlite"), "sqlite")
  expect_equal(detect_telemetry_format("data.sqlite3"), "sqlite")
  expect_equal(detect_telemetry_format("data.db"), "sqlite")
  expect_equal(detect_telemetry_format("data.json"), "json")
  expect_equal(detect_telemetry_format("data.log"), "json")
  expect_equal(detect_telemetry_format("data.txt"), "json")

  expect_error(detect_telemetry_format("data.csv"), "Cannot auto-detect")
  expect_error(detect_telemetry_format("data.unknown"), "Cannot auto-detect")
})

test_that("normalize_telemetry_columns handles various formats", {
  # test with properly formatted data
  events <- data.frame(
    timestamp = "2023-01-01 12:00:00",
    session_id = "session1",
    event_type = "click",
    input_id = "button1",
    stringsAsFactors = FALSE
  )

  normalized <- normalize_telemetry_columns(events)
  expect_true(is.data.frame(normalized))
  expect_true("timestamp" %in% names(normalized))
  expect_true("session_id" %in% names(normalized))
  expect_true("event_type" %in% names(normalized))

  # test with alternative column names
  alt_events <- data.frame(
    time = "2023-01-01 12:00:00",
    session = "session1",
    type = "click",
    input = "button1",
    stringsAsFactors = FALSE
  )

  alt_normalized <- normalize_telemetry_columns(alt_events)
  expect_true("timestamp" %in% names(alt_normalized))
  expect_true("session_id" %in% names(alt_normalized))
  expect_true("event_type" %in% names(alt_normalized))
  expect_true("input_id" %in% names(alt_normalized))

  # test error with missing required columns
  bad_events <- data.frame(
    some_column = "value",
    stringsAsFactors = FALSE
  )

  expect_error(
    normalize_telemetry_columns(bad_events),
    "Required columns missing"
  )

  # test filtering of invalid rows
  mixed_events <- data.frame(
    timestamp = c("2023-01-01 12:00:00", "", "2023-01-01 13:00:00"),
    session_id = c("session1", "", "session2"),
    event_type = c("click", "", "input"),
    stringsAsFactors = FALSE
  )

  filtered <- normalize_telemetry_columns(mixed_events)
  expect_equal(nrow(filtered), 2) # should filter out empty row
})

test_that("read_telemetry_json handles different JSON formats", {
  # test with empty file
  temp_file <- tempfile(fileext = ".json")
  writeLines(character(0), temp_file)

  result <- read_telemetry_json(temp_file)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("timestamp", "session_id", "event_type") %in% names(result)
  ))

  unlink(temp_file)

  # test with JSON array format
  temp_array <- tempfile(fileext = ".json")
  json_array <- '[
    {"timestamp": "2023-01-01 12:00:00", "session_id": "session1", "event_type": "click"},
    {"timestamp": "2023-01-01 12:01:00", "session_id": "session1", "event_type": "input"}
  ]'
  writeLines(json_array, temp_array)

  result_array <- read_telemetry_json(temp_array)
  expect_true(is.data.frame(result_array))
  expect_equal(nrow(result_array), 2)
  expect_equal(result_array$session_id[1], "session1")

  unlink(temp_array)

  # test with JSON lines format
  temp_lines <- tempfile(fileext = ".json")
  json_lines <- c(
    '{"timestamp": "2023-01-01 12:00:00", "session_id": "session1", "event_type": "click"}',
    '{"timestamp": "2023-01-01 12:01:00", "session_id": "session2", "event_type": "input"}'
  )
  writeLines(json_lines, temp_lines)

  result_lines <- read_telemetry_json(temp_lines)
  expect_true(is.data.frame(result_lines))
  expect_equal(nrow(result_lines), 2)
  expect_true(all(c("session1", "session2") %in% result_lines$session_id))

  unlink(temp_lines)

  # test with invalid JSON - should handle gracefully or error
  temp_invalid <- tempfile(fileext = ".json")
  writeLines("invalid json content", temp_invalid)

  # function may handle invalid JSON by returning empty data frame or error
  result_invalid <- tryCatch(
    {
      read_telemetry_json(temp_invalid)
    },
    error = function(e) {
      expect_true(grepl("Error reading JSON file", e$message))
      NULL
    }
  )

  # if no error, should return data frame
  if (!is.null(result_invalid)) {
    expect_true(is.data.frame(result_invalid))
  }

  unlink(temp_invalid)
})

test_that("telemetry analysis functions handle edge cases", {
  # test find_unused_inputs with no input events
  events_no_inputs <- data.frame(
    timestamp = "2023-01-01 12:00:00",
    session_id = "session1",
    event_type = "navigation",
    stringsAsFactors = FALSE
  )

  unused <- find_unused_inputs(events_no_inputs)
  expect_equal(length(unused), 0)

  # test find_delayed_sessions with no login events
  events_no_login <- data.frame(
    timestamp = "2023-01-01 12:00:00",
    session_id = "session1",
    event_type = "click",
    stringsAsFactors = FALSE
  )

  delays <- find_delayed_sessions(events_no_login)
  expect_null(delays)

  # test find_error_patterns with no errors
  events_no_errors <- data.frame(
    timestamp = "2023-01-01 12:00:00",
    session_id = "session1",
    event_type = "click",
    stringsAsFactors = FALSE
  )

  errors <- find_error_patterns(events_no_errors)
  expect_equal(length(errors), 0)

  # test find_confusion_patterns with no input events
  confusion <- find_confusion_patterns(events_no_inputs)
  expect_equal(length(confusion), 0)
})

test_that(".create_issues_tibble handles empty and malformed data", {
  # test with empty issues
  empty_issues <- list()
  total_sessions <- 5
  events <- data.frame(
    timestamp = "2023-01-01 12:00:00",
    session_id = "session1",
    event_type = "click"
  )

  empty_result <- .create_issues_tibble(empty_issues, total_sessions, events)
  expect_true(tibble::is_tibble(empty_result))
  expect_equal(nrow(empty_result), 0)
  expect_true(all(
    c("issue_id", "issue_type", "severity") %in% names(empty_result)
  ))
})

test_that(".calculate_severity_metrics handles zero sessions correctly", {
  # test with zero total sessions (our recent fix)
  result <- .calculate_severity_metrics("unused_input_test", data.frame(), 0)

  expect_true(is.list(result))
  expect_true("severity" %in% names(result))
  expect_true("affected_sessions" %in% names(result))
  expect_true("impact_rate" %in% names(result))
  expect_equal(result$impact_rate, 0.0) # should not cause division by zero
  expect_true(result$severity %in% c("critical", "high", "medium", "low"))
})
