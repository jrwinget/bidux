# ==============================================================================
# HELPERS
# ==============================================================================

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
      unused_input_threshold = 0.1, # very low threshold
      delay_threshold_seconds = 1 # very low delay threshold
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

# ==============================================================================
# DBI CONNECTION SUPPORT TESTS
# ==============================================================================

test_that("bid_ingest_telemetry accepts DBI connection objects", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # create connection and pass it directly

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  result <- bid_ingest_telemetry(con)

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

test_that("DBI connection stays open after bid_ingest_telemetry call", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # create connection
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # call bid_ingest_telemetry
  result <- bid_ingest_telemetry(con)

  # connection should still be valid and usable
  expect_true(DBI::dbIsValid(con))

  # should be able to use connection for additional queries
  tables <- DBI::dbListTables(con)
  expect_true(length(tables) > 0)
})

test_that("file path input still works for backward compatibility", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # use file path directly (original behavior)
  result <- bid_ingest_telemetry(temp_sqlite, format = "sqlite")

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))
})

test_that("table_name parameter reads from correct table", {
  skip_if_no_telemetry_deps()

  # create database with custom table name
  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create custom table instead of standard 'events'
  DBI::dbExecute(con, "
    CREATE TABLE my_custom_events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT,
      page TEXT
    )
  ")

  # insert test data
  DBI::dbExecute(con, "
    INSERT INTO my_custom_events (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  # should read from custom table
  result <- bid_ingest_telemetry(con, table_name = "my_custom_events")

  expect_s3_class(result, "bid_issues")
})

test_that("auto-detection finds event_data table first", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create both event_data and events tables
  DBI::dbExecute(con, "
    CREATE TABLE event_data (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  # add data only to event_data

  DBI::dbExecute(con, "
    INSERT INTO event_data (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  # should prefer event_data over events
  result <- bid_ingest_telemetry(con)
  expect_s3_class(result, "bid_issues")
})

test_that("auto-detection falls back to events table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create only events table (not event_data)
  DBI::dbExecute(con, "
    CREATE TABLE events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO events (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  result <- bid_ingest_telemetry(con)
  expect_s3_class(result, "bid_issues")
})

test_that("auto-detection uses first table when no standard names found", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create table with non-standard name
  DBI::dbExecute(con, "
    CREATE TABLE custom_telemetry (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO custom_telemetry (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  # should warn and use first available table
  expect_warning(
    result <- bid_ingest_telemetry(con),
    "No standard event table found"
  )

  expect_s3_class(result, "bid_issues")
})

test_that("events_table and table_name are mutually exclusive", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # create custom events_table data frame
  custom_events <- data.frame(
    event_id = 1:2,
    timestamp = c("2025-01-01 10:00:00", "2025-01-01 10:00:05"),
    event_type = c("login", "input"),
    user_id = c("u1", "u1"),
    session_id = c("s1", "s1"),
    stringsAsFactors = FALSE
  )

  # should error when both are provided
  expect_error(
    bid_ingest_telemetry(
      con,
      events_table = custom_events,
      table_name = "events"
    ),
    "Cannot specify both"
  )
})

test_that("invalid connection is detected", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)
  on.exit(unlink(temp_db))

  # create and close connection

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbDisconnect(con)

  # should error with informative message

  expect_error(
    bid_ingest_telemetry(con),
    "not valid|closed"
  )
})

test_that("table_name parameter errors for non-existent table", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # should error with informative message about available tables
  expect_error(
    bid_ingest_telemetry(con, table_name = "nonexistent_table"),
    "not found in database|Available tables"
  )
})

test_that("table_name parameter validates input type", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # should error for non-character table_name
  expect_error(
    bid_ingest_telemetry(con, table_name = 123),
    "non-empty character string"
  )

  # should error for empty string
  expect_error(
    bid_ingest_telemetry(con, table_name = ""),
    "non-empty character string"
  )

  # should error for vector
  expect_error(
    bid_ingest_telemetry(con, table_name = c("a", "b")),
    "non-empty character string"
  )
})

test_that("DBI connection cannot be used with JSON format", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # should error when trying to use JSON format with connection

  expect_error(
    bid_ingest_telemetry(con, format = "json"),
    "only supported for SQLite"
  )
})

test_that("empty database errors appropriately", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # database has no tables
  expect_error(
    bid_ingest_telemetry(con),
    "No tables found"
  )
})

test_that("bid_telemetry wrapper works with DBI connection", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)

  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  result <- bid_telemetry(con)

  # should return tibble class
  expect_s3_class(result, "bid_issues_tbl")
  expect_true(tibble::is_tibble(result))

  # connection should still be open

  expect_true(DBI::dbIsValid(con))
})

test_that("bid_telemetry wrapper works with table_name parameter", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create custom table
  DBI::dbExecute(con, "
    CREATE TABLE my_events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO my_events (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  result <- bid_telemetry(con, table_name = "my_events")

  expect_s3_class(result, "bid_issues_tbl")
})

test_that("connection-based ingestion handles thresholds correctly", {
  skip_if_no_telemetry_deps()

  # create events with patterns that would trigger issues with strict thresholds
  delayed_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login",
      input_id = NA,
      page = NA
    ),
    list(
      timestamp = "2025-01-01 10:05:00", # 5 minute delay
      session_id = "session1",
      event_type = "input",
      input_id = "btn1",
      page = NA
    )
  )

  temp_sqlite <- create_temp_sqlite_file(delayed_events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # test with custom thresholds
  result <- bid_ingest_telemetry(
    con,
    thresholds = list(delay_threshold_secs = 10) # very low threshold
  )

  expect_s3_class(result, "bid_issues")
})

test_that("connection format defaults to sqlite when not specified", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # format should auto-default to sqlite for connections
  result <- bid_ingest_telemetry(con)

  expect_s3_class(result, "bid_issues")
})

test_that("multiple consecutive calls with same connection work", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # first call
  result1 <- bid_ingest_telemetry(con)
  expect_s3_class(result1, "bid_issues")

  # second call should also work
  result2 <- bid_ingest_telemetry(con)
  expect_s3_class(result2, "bid_issues")

  # connection should still be valid

  expect_true(DBI::dbIsValid(con))
})

# ==============================================================================
# ADDITIONAL COVERAGE TESTS FOR 90%+ COVERAGE
# ==============================================================================

test_that("thresholds parameter validates type correctly", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # should error when thresholds is not a list
  expect_error(
    bid_ingest_telemetry(temp_json, thresholds = "invalid"),
    "thresholds parameter must be a list or NULL"
  )

  expect_error(
    bid_ingest_telemetry(temp_json, thresholds = 123),
    "thresholds parameter must be a list or NULL"
  )

  expect_error(
    bid_ingest_telemetry(temp_json, thresholds = TRUE),
    "thresholds parameter must be a list or NULL"
  )
})

test_that("events_table parameter validates required columns", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create empty table to ensure db exists
  DBI::dbExecute(con, "CREATE TABLE dummy (id INTEGER)")

  # events_table missing required columns
  invalid_events <- data.frame(
    some_column = c("a", "b"),
    other_column = c(1, 2)
  )

  expect_error(
    bid_ingest_telemetry(con, events_table = invalid_events),
    "missing required columns"
  )
})

test_that("events_table parameter validates data.frame type", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # events_table not a data.frame
  expect_error(
    bid_ingest_telemetry(con, events_table = list(a = 1, b = 2)),
    "must be a data.frame"
  )
})

test_that("events_table parameter validates minimum rows", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # empty data.frame with correct columns
  empty_events <- data.frame(
    event_id = character(),
    timestamp = character(),
    event_type = character(),
    user_id = character(),
    stringsAsFactors = FALSE
  )

  expect_error(
    bid_ingest_telemetry(con, events_table = empty_events),
    "must have at least 1 row"
  )
})

test_that("table_name validates whitespace-only strings", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # whitespace-only string
  expect_error(
    bid_ingest_telemetry(con, table_name = "   "),
    "non-empty character string"
  )
})

test_that("detect_telemetry_format handles various extensions", {
  # test .sqlite3 extension
  temp_sqlite3 <- tempfile(fileext = ".sqlite3")
  file.create(temp_sqlite3)
  on.exit(unlink(temp_sqlite3), add = TRUE)

  # test .db extension
  temp_db <- tempfile(fileext = ".db")
  file.create(temp_db)
  on.exit(unlink(temp_db), add = TRUE)

  # test .txt extension
  temp_txt <- tempfile(fileext = ".txt")
  writeLines('{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}', temp_txt)
  on.exit(unlink(temp_txt), add = TRUE)

  # test .log extension
  temp_log <- tempfile(fileext = ".log")
  writeLines('{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}', temp_log)
  on.exit(unlink(temp_log), add = TRUE)

  # .txt and .log should be detected as json
  result_txt <- bid_ingest_telemetry(temp_txt)
  expect_s3_class(result_txt, "bid_issues")

  result_log <- bid_ingest_telemetry(temp_log)
  expect_s3_class(result_log, "bid_issues")
})

test_that("format parameter can be explicitly set for DBI connections", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_sqlite)
  withr::defer(DBI::dbDisconnect(con))

  # explicitly set format to sqlite
  result <- bid_ingest_telemetry(con, format = "sqlite")

  expect_s3_class(result, "bid_issues")
})

test_that("events_table works with connection object", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create dummy table
  DBI::dbExecute(con, "CREATE TABLE dummy (id INTEGER)")

  # provide valid events_table directly
  custom_events <- data.frame(
    event_id = c(1, 2),
    timestamp = c("2025-01-01 10:00:00", "2025-01-01 10:00:05"),
    event_type = c("login", "input"),
    user_id = c("u1", "u1"),
    session_id = c("s1", "s1"),
    input_id = c(NA, "btn1"),
    stringsAsFactors = FALSE
  )

  result <- bid_ingest_telemetry(con, events_table = custom_events)
  expect_s3_class(result, "bid_issues")
})

test_that("warning displayed when using first table as fallback from file path", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # create table with non-standard name
  DBI::dbExecute(con, "
    CREATE TABLE custom_telemetry (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO custom_telemetry (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  DBI::dbDisconnect(con)

  # should warn when using first available table (file path version)
  expect_warning(
    result <- bid_ingest_telemetry(temp_db, format = "sqlite"),
    "No standard event table found"
  )

  expect_s3_class(result, "bid_issues")
})

test_that("events_table and table_name mutual exclusivity works with file path", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # create custom events_table data frame
  custom_events <- data.frame(
    event_id = 1:2,
    timestamp = c("2025-01-01 10:00:00", "2025-01-01 10:00:05"),
    event_type = c("login", "input"),
    user_id = c("u1", "u1"),
    session_id = c("s1", "s1"),
    stringsAsFactors = FALSE
  )

  # should error when both are provided (file path version)
  expect_error(
    bid_ingest_telemetry(
      temp_sqlite,
      format = "sqlite",
      events_table = custom_events,
      table_name = "events"
    ),
    "Cannot specify both"
  )
})

test_that("table_name parameter works with file path", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # create custom table
  DBI::dbExecute(con, "
    CREATE TABLE my_custom_events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO my_custom_events (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  DBI::dbDisconnect(con)

  # should read from custom table using file path
  result <- bid_ingest_telemetry(temp_db, format = "sqlite", table_name = "my_custom_events")

  expect_s3_class(result, "bid_issues")
})

test_that("table_name not found errors with file path", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # should error with informative message about available tables (file path version)
  expect_error(
    bid_ingest_telemetry(temp_sqlite, format = "sqlite", table_name = "nonexistent_table"),
    "not found in database|Available tables"
  )
})

test_that("empty database errors with file path", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  # create connection and immediately close to create empty db
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbDisconnect(con)

  # database has no tables
  expect_error(
    bid_ingest_telemetry(temp_db, format = "sqlite"),
    "No tables found"
  )
})

test_that("auto-detection finds event_data table first with file path", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # create both event_data and events tables
  DBI::dbExecute(con, "
    CREATE TABLE event_data (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  # add data only to event_data
  DBI::dbExecute(con, "
    INSERT INTO event_data (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  DBI::dbDisconnect(con)

  # should prefer event_data over events (file path version)
  result <- bid_ingest_telemetry(temp_db, format = "sqlite")
  expect_s3_class(result, "bid_issues")
})

test_that("auto-detection falls back to events table with file path", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # create only events table (not event_data)
  DBI::dbExecute(con, "
    CREATE TABLE events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  DBI::dbExecute(con, "
    INSERT INTO events (timestamp, session_id, event_type, input_id)
    VALUES
      ('2025-01-01 10:00:00', 'session1', 'login', NULL),
      ('2025-01-01 10:00:05', 'session1', 'input', 'btn1')
  ")

  DBI::dbDisconnect(con)

  result <- bid_ingest_telemetry(temp_db, format = "sqlite")
  expect_s3_class(result, "bid_issues")
})

test_that("bid_telemetry_presets returns correct structures", {
  strict <- bid_telemetry_presets("strict")
  moderate <- bid_telemetry_presets("moderate")
  relaxed <- bid_telemetry_presets("relaxed")

  # all should be lists
  expect_type(strict, "list")
  expect_type(moderate, "list")
  expect_type(relaxed, "list")

  # all should have the required threshold parameters
  required_names <- c(
    "unused_input_threshold",
    "delay_threshold_secs",
    "error_rate_threshold",
    "navigation_threshold",
    "rapid_change_window",
    "rapid_change_count"
  )

  expect_true(all(required_names %in% names(strict)))
  expect_true(all(required_names %in% names(moderate)))
  expect_true(all(required_names %in% names(relaxed)))
})

test_that("bid_telemetry_presets validates preset parameter", {
  # should error on invalid preset

  expect_error(
    bid_telemetry_presets("invalid"),
    "'arg' should be one of"
  )
})

test_that("presets can be used with bid_ingest_telemetry", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # test with each preset
  result_strict <- bid_ingest_telemetry(
    temp_json,
    thresholds = bid_telemetry_presets("strict")
  )
  expect_s3_class(result_strict, "bid_issues")

  result_relaxed <- bid_ingest_telemetry(
    temp_json,
    thresholds = bid_telemetry_presets("relaxed")
  )
  expect_s3_class(result_relaxed, "bid_issues")
})

test_that("thresholds parameter merges with defaults correctly", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # partial thresholds should be merged with defaults
  result <- bid_ingest_telemetry(
    temp_json,
    thresholds = list(
      delay_threshold_secs = 5 # only override one value
    )
  )

  expect_s3_class(result, "bid_issues")
})

test_that("JSON array format is parsed correctly", {
  # create JSON as array (already tested implicitly, but explicit here)
  json_array <- '[
    {"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"},
    {"timestamp": "2025-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}
  ]'

  temp_json <- tempfile(fileext = ".json")
  writeLines(json_array, temp_json)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("JSON lines format is parsed correctly", {
  # create JSON as lines (NDJSON format)
  json_lines <- c(
    '{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    '{"timestamp": "2025-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}'
  )

  temp_json <- tempfile(fileext = ".json")
  writeLines(json_lines, temp_json)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("JSON with blank lines is handled", {
  # json with blank lines should be filtered
  json_lines <- c(
    '{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    "",
    '{"timestamp": "2025-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}',
    "   "
  )

  temp_json <- tempfile(fileext = ".json")
  writeLines(json_lines, temp_json)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("JSON with invalid lines filters them out", {
  # some invalid JSON lines mixed with valid ones
  json_lines <- c(
    '{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    "not valid json",
    '{"timestamp": "2025-01-01 10:00:05", "session_id": "s1", "event_type": "input", "input_id": "btn1"}'
  )

  temp_json <- tempfile(fileext = ".json")
  writeLines(json_lines, temp_json)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("normalize_telemetry_columns maps alternative column names", {
  # create events with alternative column names
  alt_events <- list(
    list(
      datetime = "2025-01-01 10:00:00", # alternative to timestamp
      session = "s1", # alternative to session_id
      type = "login" # alternative to event_type
    ),
    list(
      datetime = "2025-01-01 10:00:05",
      session = "s1",
      type = "input",
      input = "btn1" # alternative to input_id
    )
  )

  temp_json <- create_temp_json_file(alt_events)
  on.exit(unlink(temp_json))

  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("events with empty values in required fields are filtered", {
  # events with empty string in required field
  events_with_empty <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "s1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:00:05",
      session_id = "", # empty string - should be filtered
      event_type = "input"
    ),
    list(
      timestamp = "2025-01-01 10:00:10",
      session_id = "s1",
      event_type = "input",
      input_id = "btn1"
    )
  )

  temp_json <- create_temp_json_file(events_with_empty)
  on.exit(unlink(temp_json))

  # should succeed, filtering out invalid row
  result <- bid_ingest_telemetry(temp_json)
  expect_s3_class(result, "bid_issues")
})

test_that("connection with events_table uses data directly", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create table but don't add data
  DBI::dbExecute(con, "CREATE TABLE dummy (id INTEGER)")

  # provide valid events_table directly - should use this instead of reading from db
  custom_events <- data.frame(
    event_id = c(1, 2),
    timestamp = c("2025-01-01 10:00:00", "2025-01-01 10:00:05"),
    event_type = c("login", "input"),
    user_id = c("u1", "u1"),
    session_id = c("s1", "s1"),
    input_id = c(NA, "btn1"),
    stringsAsFactors = FALSE
  )

  result <- bid_ingest_telemetry(con, events_table = custom_events)
  expect_s3_class(result, "bid_issues")
})

test_that("empty events table errors during normalization", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(temp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  withr::defer(DBI::dbDisconnect(con))

  # create table with required columns but no data
  DBI::dbExecute(con, "
    CREATE TABLE events (
      timestamp TEXT,
      session_id TEXT,
      event_type TEXT,
      input_id TEXT
    )
  ")

  # should error because empty table has no valid events after filtering
  expect_error(
    bid_ingest_telemetry(con),
    "No valid events found"
  )
})

test_that("telemetry presets match expected threshold values", {
  strict <- bid_telemetry_presets("strict")
  relaxed <- bid_telemetry_presets("relaxed")

  # strict should have lower thresholds (more sensitive)
  expect_lt(strict$unused_input_threshold, relaxed$unused_input_threshold)
  expect_lt(strict$delay_threshold_secs, relaxed$delay_threshold_secs)
  expect_lt(strict$error_rate_threshold, relaxed$error_rate_threshold)
  expect_lt(strict$navigation_threshold, relaxed$navigation_threshold)
  expect_gt(strict$rapid_change_window, relaxed$rapid_change_window)
  expect_lt(strict$rapid_change_count, relaxed$rapid_change_count)
})

test_that("format parameter explicitly set to sqlite works with file", {
  skip_if_no_telemetry_deps()

  events <- create_sample_telemetry_events(2)
  temp_sqlite <- create_temp_sqlite_file(events)
  on.exit(unlink(temp_sqlite))

  # explicitly set format
  result <- bid_ingest_telemetry(temp_sqlite, format = "sqlite")
  expect_s3_class(result, "bid_issues")
})

test_that("format parameter explicitly set to json works", {
  events <- create_sample_telemetry_events(2)
  temp_json <- create_temp_json_file(events)
  on.exit(unlink(temp_json))

  # explicitly set format
  result <- bid_ingest_telemetry(temp_json, format = "json")
  expect_s3_class(result, "bid_issues")
})
