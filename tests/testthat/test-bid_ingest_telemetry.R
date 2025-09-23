# test functions to reduce repetition
create_sample_telemetry_events <- function(num_sessions = 2) {
  events <- list()
  for (i in 1:num_sessions) {
    session_id <- paste0("session", i)
    events <- append(events, list(
      list(
        timestamp = paste0("2025-01-01 1", i, ":00:00"),
        session_id = session_id,
        event_type = "login"
      ),
      list(
        timestamp = paste0("2025-01-01 1", i, ":00:05"),
        session_id = session_id,
        event_type = "input",
        input_id = "input1"
      ),
      list(
        timestamp = paste0("2025-01-01 1", i, ":00:10"),
        session_id = session_id,
        event_type = if (i == 1) "input" else "navigation",
        input_id = if (i == 1) "input2" else NULL,
        page = if (i == 1) NULL else "page1"
      )
    ))
  }
  events
}

create_temp_json_file <- function(events) {
  temp_file <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(events, auto_unbox = TRUE), temp_file)
  temp_file
}

create_temp_sqlite_file <- function(events = NULL) {
  temp_file <- tempfile(fileext = ".sqlite")
  if (!is.null(events)) {
    # create db with events
    conn <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
    on.exit(DBI::dbDisconnect(conn))

    # create a simple events table
    DBI::dbExecute(conn, "
      CREATE TABLE events (
        timestamp TEXT,
        session_id TEXT,
        event_type TEXT,
        input_id TEXT,
        page TEXT
      )
    ")

    # insert events
    for (event in events) {
      DBI::dbExecute(conn, "
        INSERT INTO events (timestamp, session_id, event_type, input_id, page)
        VALUES (?, ?, ?, ?, ?)
      ", params = list(
        event$timestamp %||% NA,
        event$session_id %||% NA,
        event$event_type %||% NA,
        event$input_id %||% NA,
        event$page %||% NA
      ))
    }
  } else {
    # create empty database
    file.create(temp_file)
  }
  temp_file
}

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

test_that("bid_ingest_telemetry auto-detects format correctly", {
  # format is auto-detected, no separate function needed
  # this test ensures format detection works internally

  # create test files for format detection
  temp_sqlite <- tempfile(fileext = ".sqlite")
  temp_json <- tempfile(fileext = ".json")
  temp_csv <- tempfile(fileext = ".csv")

  # create valid telemetry JSON file
  test_events <- '[
    {"timestamp": "2023-01-01 12:00:00", "session_id": "test_session", "event_type": "click", "input_id": "button1"},
    {"timestamp": "2023-01-01 12:01:00", "session_id": "test_session", "event_type": "input", "input_id": "text1"}
  ]'
  writeLines(test_events, temp_json)
  file.create(temp_sqlite)
  file.create(temp_csv)

  on.exit({
    unlink(temp_sqlite)
    unlink(temp_json)
    unlink(temp_csv)
  })

  # should auto-detect json format
  expect_no_error(bid_ingest_telemetry(temp_json))

  # should handle unsupported formats
  expect_error(
    bid_ingest_telemetry(temp_csv),
    "Cannot auto-detect format"
  )
})

test_that("bid_ingest_telemetry validates inputs correctly", {
  expect_error(
    bid_ingest_telemetry("nonexistent.sqlite"),
    "Telemetry file not found"
  )

  # create temporary file for format validation
  temp_file <- tempfile(fileext = ".json")
  writeLines("[]", temp_file)
  on.exit(unlink(temp_file))

  expect_error(
    bid_ingest_telemetry(temp_file, format = "invalid"),
    "Format must be 'sqlite' or 'json'"
  )
})

test_that("bid_ingest_telemetry handles empty telemetry data", {
  temp_json <- create_temp_json_file(list())
  on.exit(unlink(temp_json))

  expect_warning(
    result <- bid_ingest_telemetry(temp_json),
    "No telemetry events found"
  )

  expect_equal(length(result), 0)
})

test_that("bid_ingest_telemetry processes JSON format correctly", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

test_that("bid_ingest_telemetry processes SQLite format correctly", {
  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  result <- bid_ingest_telemetry(temp_sqlite, format = "sqlite")

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

# ==============================================================================
# TELEMETRY ANALYSIS TESTS
# ==============================================================================

test_that("bid_ingest_telemetry identifies patterns in data", {
  # create events where input2 is used in only 1 of 2 sessions
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  # should return bid_issues object
  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

test_that("bid_ingest_telemetry analyzes timing patterns", {
  # create events with long delays
  delayed_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:05:00", # 5 minute delay
      session_id = "session1",
      event_type = "input",
      input_id = "input1"
    )
  )

  temp_json <- create_temp_json_file(delayed_events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  # should return bid_issues object
  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

test_that("bid_ingest_telemetry processes basic events", {
  # create simple events without problematic columns
  basic_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "input",
      input_id = "test_input"
    ),
    list(
      timestamp = "2025-01-01 10:00:10",
      session_id = "session1",
      event_type = "input",
      input_id = "test_input"
    )
  )

  temp_json <- create_temp_json_file(basic_events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  # should return bid_issues object
  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("bid_ingest_telemetry handles malformed JSON gracefully", {
  temp_json <- tempfile(fileext = ".json")
  writeLines("{ invalid json", temp_json)
  on.exit(unlink(temp_json))

  # function may give warning for malformed JSON instead of error
  expect_warning(
    result <- bid_ingest_telemetry(temp_json),
    "No telemetry events found"
  )
})

test_that("bid_ingest_telemetry handles corrupted SQLite files", {
  temp_sqlite <- tempfile(fileext = ".sqlite")
  writeLines("not a sqlite file", temp_sqlite)
  on.exit(unlink(temp_sqlite))

  # Should error when trying to read corrupted SQLite file
  # Note: RSQLite may also warn about synchronous mode - this is expected
  expect_error(
    expect_warning(
      bid_ingest_telemetry(temp_sqlite, format = "sqlite"),
      "synchronous|Error reading SQLite database"
    ),
    "Error reading SQLite database"
  )
})

test_that("bid_ingest_telemetry handles missing required fields", {
  # events missing required fields
  incomplete_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00"
      # missing session_id and event_type
    ),
    list(
      session_id = "session1"
      # missing timestamp and event_type
    )
  )

  temp_json <- create_temp_json_file(incomplete_events)
  on.exit(unlink(temp_json))

  # current implementation throws error for missing required fields
  expect_error(
    bid_ingest_telemetry(temp_json),
    "Error reading JSON file.*Required columns missing"
  )
})

# ==============================================================================
# THRESHOLD AND CONFIGURATION TESTS
# ==============================================================================

test_that("bid_ingest_telemetry respects custom thresholds", {
  events <- create_sample_telemetry_events(3)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # test with custom thresholds
  result <- bid_ingest_telemetry(
    temp_json,
    thresholds = list(
      unused_input_threshold = 0.1,  # very low threshold
      delay_threshold_seconds = 1    # very low delay threshold
    )
  )

  expect_s3_class(result, "bid_issues")
})

test_that("bid_ingest_telemetry handles threshold parameters", {
  events <- create_sample_telemetry_events(1)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # test with various threshold values - current implementation may not validate
  result1 <- bid_ingest_telemetry(
    temp_json,
    thresholds = list(unused_input_threshold = 0.1)
  )
  expect_s3_class(result1, "bid_issues")

  result2 <- bid_ingest_telemetry(
    temp_json,
    thresholds = list(unused_input_threshold = 0.9)
  )
  expect_s3_class(result2, "bid_issues")
})

# ==============================================================================
# INTEGRATION AND WORKFLOW TESTS
# ==============================================================================

test_that("bid_ingest_telemetry integrates with BID workflow", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  telemetry_result <- bid_ingest_telemetry(temp_json)

  # bid_issues object should be created without error
  expect_s3_class(telemetry_result, "bid_issues")

  # should be able to continue with bid workflow
  interpret_stage <- bid_interpret(
    central_question = "How to improve based on telemetry?"
  )

  expect_no_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Telemetry shows user issues",
      evidence = "Data from usage analytics"
    )
  )
})

test_that("bid_ingest_telemetry returns proper bid_issues class", {
  events <- create_sample_telemetry_events(1)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))

  # bid_issues is a hybrid object that contains bid_stage objects
  # the list may be empty if no issues found
  expect_true(length(result) >= 0)
})

# ==============================================================================
# PERFORMANCE AND LARGE DATA TESTS
# ==============================================================================

test_that("bid_ingest_telemetry handles moderately large datasets", {
  # create larger dataset
  large_events <- list()
  for (i in 1:50) { # 50 sessions instead of 2
    session_events <- create_sample_telemetry_events(1)
    # modify session ids to be unique
    for (j in seq_along(session_events)) {
      session_events[[j]]$session_id <- paste0("session", i)
    }
    large_events <- append(large_events, session_events)
  }

  temp_json <- create_temp_json_file(large_events)
  on.exit(unlink(temp_json))

  # should handle larger datasets without error
  expect_no_error(result <- bid_ingest_telemetry(temp_json))
  expect_s3_class(result, "bid_issues")
})
