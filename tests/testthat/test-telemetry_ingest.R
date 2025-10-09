test_that("detect_telemetry_format identifies formats correctly", {
  # SQLite formats
  expect_equal(detect_telemetry_format("test.sqlite"), "sqlite")
  expect_equal(detect_telemetry_format("test.sqlite3"), "sqlite")
  expect_equal(detect_telemetry_format("test.db"), "sqlite")

  # JSON formats
  expect_equal(detect_telemetry_format("test.json"), "json")
  expect_equal(detect_telemetry_format("test.log"), "json")
  expect_equal(detect_telemetry_format("test.txt"), "json")

  # Case insensitive
  expect_equal(detect_telemetry_format("test.SQLITE"), "sqlite")
  expect_equal(detect_telemetry_format("test.JSON"), "json")

  # Unknown format
  expect_error(
    detect_telemetry_format("test.unknown"),
    "Cannot auto-detect format"
  )
})

test_that("normalize_telemetry_columns handles different column mappings", {
  # Test standard columns
  events_standard <- data.frame(
    timestamp = as.POSIXct("2023-01-01 10:00:00"),
    session_id = "s1",
    event_type = "input",
    stringsAsFactors = FALSE
  )

  result <- normalize_telemetry_columns(events_standard)
  expect_true(all(c("timestamp", "session_id", "event_type") %in% names(result)))

  # Test alternative column names
  events_alt <- data.frame(
    time = as.POSIXct("2023-01-01 10:00:00"),
    session = "s1",
    type = "input",
    stringsAsFactors = FALSE
  )

  result_alt <- normalize_telemetry_columns(events_alt)
  expect_true(all(c("timestamp", "session_id", "event_type") %in% names(result_alt)))

  # Test missing required columns
  events_incomplete <- data.frame(
    timestamp = as.POSIXct("2023-01-01 10:00:00"),
    stringsAsFactors = FALSE
  )

  expect_error(
    normalize_telemetry_columns(events_incomplete),
    "Required columns missing"
  )
})

test_that("normalize_telemetry_columns handles timestamp conversion", {
  # Test character timestamp conversion
  events_char_time <- data.frame(
    timestamp = "2023-01-01 10:00:00",
    session_id = "s1",
    event_type = "input",
    stringsAsFactors = FALSE
  )

  result <- normalize_telemetry_columns(events_char_time)
  expect_true(inherits(result$timestamp, "POSIXct"))

  # Test already POSIXct timestamp
  events_posix_time <- data.frame(
    timestamp = as.POSIXct("2023-01-01 10:00:00"),
    session_id = "s1",
    event_type = "input",
    stringsAsFactors = FALSE
  )

  result_posix <- normalize_telemetry_columns(events_posix_time)
  expect_true(inherits(result_posix$timestamp, "POSIXct"))
})

test_that("normalize_telemetry_columns filters invalid rows", {
  # Test with empty values
  events_with_empty <- data.frame(
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    session_id = c("s1", ""),
    event_type = c("input", "login"),
    stringsAsFactors = FALSE
  )

  result <- normalize_telemetry_columns(events_with_empty)
  expect_equal(nrow(result), 1)
  expect_equal(result$session_id[1], "s1")

  # Test with NA values
  events_with_na <- data.frame(
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    session_id = c("s1", NA),
    event_type = c("input", "login"),
    stringsAsFactors = FALSE
  )

  result_na <- normalize_telemetry_columns(events_with_na)
  expect_equal(nrow(result_na), 1)
  expect_equal(result_na$session_id[1], "s1")
})

test_that("normalize_telemetry_columns handles list input", {
  # Test list of events converted to data frame
  events_list <- list(
    list(timestamp = "2023-01-01 10:00:00", session_id = "s1", event_type = "input"),
    list(timestamp = "2023-01-01 10:01:00", session_id = "s2", event_type = "login")
  )

  result <- normalize_telemetry_columns(events_list)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("timestamp", "session_id", "event_type") %in% names(result)))
})

test_that("normalize_telemetry_columns handles edge cases", {
  # Test empty data frame
  events_empty <- data.frame(
    timestamp = character(0),
    session_id = character(0),
    event_type = character(0),
    stringsAsFactors = FALSE
  )

  expect_error(
    normalize_telemetry_columns(events_empty),
    "No valid events found after filtering"
  )

  # Test non-data.frame input
  expect_error(
    normalize_telemetry_columns("not a data frame"),
    "Events must be a data frame"
  )
})

test_that("read_telemetry_json handles different JSON formats", {
  # Create temporary JSON files for testing
  temp_dir <- tempdir()

  # Test JSON lines format
  json_lines_file <- file.path(temp_dir, "test_lines.json")
  writeLines(c(
    '{"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "input"}',
    '{"timestamp": "2023-01-01 10:01:00", "session_id": "s2", "event_type": "login"}'
  ), json_lines_file)

  result_lines <- read_telemetry_json(json_lines_file)
  expect_true(is.data.frame(result_lines))
  expect_equal(nrow(result_lines), 2)

  # Test JSON array format
  json_array_file <- file.path(temp_dir, "test_array.json")
  writeLines(c(
    '[',
    '  {"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "input"},',
    '  {"timestamp": "2023-01-01 10:01:00", "session_id": "s2", "event_type": "login"}',
    ']'
  ), json_array_file)

  result_array <- read_telemetry_json(json_array_file)
  expect_true(is.data.frame(result_array))
  expect_equal(nrow(result_array), 2)

  # Test empty file
  empty_file <- file.path(temp_dir, "empty.json")
  writeLines("", empty_file)

  result_empty <- read_telemetry_json(empty_file)
  expect_true(is.data.frame(result_empty))
  expect_equal(nrow(result_empty), 0)

  # Clean up
  unlink(c(json_lines_file, json_array_file, empty_file))
})

test_that("read_telemetry_json handles invalid JSON", {
  temp_dir <- tempdir()

  # Test invalid JSON
  invalid_json_file <- file.path(temp_dir, "invalid.json")
  writeLines(c(
    '{"timestamp": "2023-01-01 10:00:00", "session_id": "s1"',  # missing closing brace
    '{"invalid": "json"}'
  ), invalid_json_file)

  # Should throw an error when no valid events are found
  expect_error(
    read_telemetry_json(invalid_json_file),
    "No valid events found in JSON file"
  )

  # Clean up
  unlink(invalid_json_file)
})

test_that("read_telemetry_json filters events without required fields", {
  temp_dir <- tempdir()

  # Test JSON with missing required fields
  incomplete_json_file <- file.path(temp_dir, "incomplete.json")
  writeLines(c(
    '{"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "input"}',  # valid
    '{"timestamp": "2023-01-01 10:01:00", "session_id": "s2"}',  # missing event_type
    '{"session_id": "s3", "event_type": "login"}'  # missing timestamp
  ), incomplete_json_file)

  result <- read_telemetry_json(incomplete_json_file)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)  # Only one valid event
  expect_equal(result$session_id[1], "s1")

  # Clean up
  unlink(incomplete_json_file)
})

test_that("bid_ingest_telemetry validates parameters", {
  # Test non-existent file
  expect_error(
    bid_ingest_telemetry("nonexistent.json"),
    "Telemetry file not found"
  )

  # Test invalid format
  temp_file <- tempfile(fileext = ".json")
  writeLines('{"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "input"}', temp_file)

  expect_error(
    bid_ingest_telemetry(temp_file, format = "invalid"),
    "Format must be 'sqlite' or 'json'"
  )

  # Test invalid thresholds
  expect_error(
    bid_ingest_telemetry(temp_file, thresholds = "not a list"),
    "thresholds parameter must be a list"
  )

  unlink(temp_file)
})

test_that("bid_ingest_telemetry handles empty telemetry data", {
  temp_file <- tempfile(fileext = ".json")
  writeLines("", temp_file)

  expect_warning(
    result <- bid_ingest_telemetry(temp_file),
    "No telemetry events found"
  )

  expect_equal(length(result), 0)
  unlink(temp_file)
})

test_that("bid_ingest_telemetry processes telemetry data correctly", {
  # Create sample telemetry data
  temp_file <- tempfile(fileext = ".json")
  sample_events <- c(
    '{"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    '{"timestamp": "2023-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}',
    '{"timestamp": "2023-01-01 10:01:00", "session_id": "s2", "event_type": "login"}',
    '{"timestamp": "2023-01-01 10:01:40", "session_id": "s2", "event_type": "input", "input_id": "btn1"}',
    '{"timestamp": "2023-01-01 10:02:00", "session_id": "s3", "event_type": "login"}',
    '{"timestamp": "2023-01-01 10:02:05", "session_id": "s3", "event_type": "error", "error_message": "timeout", "output_id": "plot1"}'
  )

  writeLines(sample_events, temp_file)

  result <- bid_ingest_telemetry(temp_file, thresholds = list(
    unused_input_threshold = 0.5,
    delay_threshold_seconds = 30,
    error_rate_threshold = 0.1
  ))

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))

  # Should detect delayed interaction for s2 (35 seconds delay > 30 threshold)
  expect_true("delayed_interaction" %in% names(result))

  # Should detect error pattern
  expect_true(any(grepl("error", names(result))))

  unlink(temp_file)
})

test_that("bid_ingest_telemetry creates proper hybrid object", {
  temp_file <- tempfile(fileext = ".json")
  sample_events <- c(
    '{"timestamp": "2023-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    '{"timestamp": "2023-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}'
  )

  writeLines(sample_events, temp_file)

  result <- bid_ingest_telemetry(temp_file)

  # Test hybrid object structure
  expect_s3_class(result, c("bid_issues", "list"))
  expect_true("issues_tbl" %in% names(attributes(result)))
  expect_true("flags" %in% names(attributes(result)))
  expect_true("created_at" %in% names(attributes(result)))

  # Test that attributes are proper types
  expect_true(tibble::is_tibble(attr(result, "issues_tbl")))
  expect_true(is.list(attr(result, "flags")))
  expect_true(inherits(attr(result, "created_at"), "POSIXct"))

  unlink(temp_file)
})

test_that("bid_ingest_telemetry handles custom events_table parameter", {
  # Create custom events table
  custom_events <- data.frame(
    event_id = 1:3,
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00", "2023-01-01 10:02:00")),
    event_type = c("login", "input", "error"),
    user_id = c("u1", "u1", "u1"),
    session_id = c("s1", "s1", "s1"),
    input_id = c(NA, "btn1", NA),
    error_message = c(NA, NA, "timeout"),
    stringsAsFactors = FALSE
  )

  # Create a dummy SQLite file for testing parameter validation
  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)

  # This would normally be used with SQLite, but we can test the validation
  expect_error(
    bid_ingest_telemetry(temp_db, events_table = "not a data frame"),
    "must be a data.frame"
  )

  unlink(temp_db)

  # Test with incomplete events_table
  incomplete_events <- data.frame(
    event_id = 1,
    timestamp = as.POSIXct("2023-01-01 10:00:00"),
    stringsAsFactors = FALSE
  )

  # Create another dummy file for this test
  temp_db2 <- tempfile(fileext = ".sqlite")
  file.create(temp_db2)

  expect_error(
    bid_ingest_telemetry(temp_db2, events_table = incomplete_events),
    "missing required columns"
  )

  unlink(temp_db2)
})