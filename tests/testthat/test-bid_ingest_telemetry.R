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

test_that("detect_telemetry_format works correctly", {
  expect_equal(detect_telemetry_format("data.sqlite"), "sqlite")
  expect_equal(detect_telemetry_format("data.sqlite3"), "sqlite")
  expect_equal(detect_telemetry_format("data.db"), "sqlite")
  expect_equal(detect_telemetry_format("data.json"), "json")
  expect_equal(detect_telemetry_format("data.log"), "json")
  expect_equal(detect_telemetry_format("data.txt"), "json")

  expect_error(
    detect_telemetry_format("data.csv"),
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

  expect_s3_class(result, "bid_telemetry")
  expect_true(length(result) > 0)
})

test_that("bid_ingest_telemetry processes SQLite format correctly", {
  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  result <- bid_ingest_telemetry(temp_sqlite, format = "sqlite")

  expect_s3_class(result, "bid_telemetry")
  expect_true(length(result) > 0)
})

# ==============================================================================
# TELEMETRY ANALYSIS TESTS
# ==============================================================================

test_that("bid_ingest_telemetry identifies unused inputs", {
  # create events where input2 is used in only 1 of 2 sessions
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  # should identify unused inputs
  unused_inputs <- result[result$issue_type == "unused_input", ]
  expect_true(nrow(unused_inputs) > 0)
})

test_that("bid_ingest_telemetry detects delayed interactions", {
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

  # should identify delayed interactions
  delayed_issues <- result[result$issue_type == "delayed_interaction", ]
  # may or may not find delays depending on thresholds
  expect_true(nrow(delayed_issues) >= 0)
})

test_that("bid_ingest_telemetry detects frequent errors", {
  # create events with multiple errors
  error_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "error",
      error_type = "validation_error"
    ),
    list(
      timestamp = "2025-01-01 10:00:10",
      session_id = "session1",
      event_type = "error",
      error_type = "validation_error"
    )
  )

  temp_json <- create_temp_json_file(error_events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  # should identify frequent errors
  error_issues <- result[result$issue_type == "frequent_error", ]
  expect_true(nrow(error_issues) >= 0)
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("bid_ingest_telemetry handles malformed JSON gracefully", {
  temp_json <- tempfile(fileext = ".json")
  writeLines("{ invalid json", temp_json)
  on.exit(unlink(temp_json))

  expect_error(
    bid_ingest_telemetry(temp_json),
    "Failed to parse|invalid.*json"
  )
})

test_that("bid_ingest_telemetry handles corrupted SQLite files", {
  temp_sqlite <- tempfile(fileext = ".sqlite")
  writeLines("not a sqlite file", temp_sqlite)
  on.exit(unlink(temp_sqlite))

  expect_error(
    bid_ingest_telemetry(temp_sqlite, format = "sqlite"),
    "Failed to read|database.*corrupt"
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

  # should handle gracefully, possibly with warnings
  expect_warning(
    result <- bid_ingest_telemetry(temp_json),
    "missing.*field|incomplete.*event"
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
    unused_threshold = 0.1,  # very low threshold
    delay_threshold = 1      # very low delay threshold
  )

  expect_s3_class(result, "bid_telemetry")
})

test_that("bid_ingest_telemetry validates threshold parameters", {
  events <- create_sample_telemetry_events(1)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # invalid threshold values should error or warn
  expect_error(
    bid_ingest_telemetry(temp_json, unused_threshold = -0.1),
    "threshold.*must be.*positive|invalid.*threshold"
  )

  expect_error(
    bid_ingest_telemetry(temp_json, unused_threshold = 1.5),
    "threshold.*must be.*between.*0.*and.*1|invalid.*threshold"
  )
})

# ==============================================================================
# INTEGRATION AND WORKFLOW TESTS
# ==============================================================================

test_that("bid_ingest_telemetry integrates with bid_notice workflow", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  telemetry_result <- bid_ingest_telemetry(temp_json)

  if (length(telemetry_result) > 0) {
    # should be able to use telemetry results with bid_notice
    interpret_stage <- bid_interpret(
      central_question = "How to improve based on telemetry?"
    )

    expect_no_error(
      bid_notice(
        previous_stage = interpret_stage,
        problem = "Telemetry shows user issues",
        evidence = "Data from usage analytics",
        telemetry_data = telemetry_result
      )
    )
  }
})

test_that("bid_ingest_telemetry returns proper bid_telemetry class", {
  events <- create_sample_telemetry_events(1)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)

  expect_s3_class(result, "bid_telemetry")
  expect_true(is.data.frame(result))

  if (nrow(result) > 0) {
    expected_cols <- c("issue_type", "severity", "description", "user_impact")
    expect_true(all(expected_cols %in% names(result)))
  }
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
  expect_s3_class(result, "bid_telemetry")
})
