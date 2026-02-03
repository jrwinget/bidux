# ----------------------------------------------------------------------------
# FILE URL REJECTION
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry rejects file:// URLs", {
  expect_error(
    bid_ingest_telemetry("file:///path/to/file.json"),
    "file:// URLs are not supported"
  )

  expect_error(
    bid_ingest_telemetry("FILE:///path/to/file.json"),
    "file:// URLs are not supported"
  )
})

# ----------------------------------------------------------------------------
# FILE SIZE AND PERMISSION VALIDATION
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry validates file exists", {
  expect_error(
    bid_ingest_telemetry("/nonexistent/path/file.json"),
    "Telemetry file not found"
  )
})

# ----------------------------------------------------------------------------
# EVENTS_TABLE AND TABLE_NAME MUTUAL EXCLUSION
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry rejects both events_table and table_name", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)

  events_df <- data.frame(
    event_id = 1,
    timestamp = as.POSIXct("2025-01-01 10:00:00"),
    event_type = "login",
    user_id = "u1",
    session_id = "s1",
    stringsAsFactors = FALSE
  )

  expect_error(
    bid_ingest_telemetry(
      temp_db,
      events_table = events_df,
      table_name = "events"
    ),
    "Cannot specify both"
  )

  unlink(temp_db)
})

test_that("bid_ingest_telemetry validates events_table structure", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)

  # Missing required columns
  incomplete_events <- data.frame(
    event_id = 1,
    timestamp = as.POSIXct("2025-01-01 10:00:00"),
    stringsAsFactors = FALSE
  )

  expect_error(
    bid_ingest_telemetry(temp_db, events_table = incomplete_events),
    "missing required columns"
  )

  unlink(temp_db)
})

test_that("bid_ingest_telemetry validates table_name parameter", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  file.create(temp_db)

  # Empty string

  expect_error(
    bid_ingest_telemetry(temp_db, table_name = ""),
    "non-empty character string"
  )

  # Non-character
  expect_error(
    bid_ingest_telemetry(temp_db, table_name = 123),
    "non-empty character string"
  )

  # Vector of length > 1
  expect_error(
    bid_ingest_telemetry(temp_db, table_name = c("a", "b")),
    "non-empty character string"
  )

  unlink(temp_db)
})

# ----------------------------------------------------------------------------
# DBI CONNECTION HANDLING
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry rejects JSON format with DBI connection", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  expect_error(
    bid_ingest_telemetry(con, format = "json"),
    "DBI connections are only supported for SQLite format"
  )

  DBI::dbDisconnect(con)
  unlink(temp_db)
})

test_that("bid_ingest_telemetry validates DBI connection is open", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbDisconnect(con) # Close it

  expect_error(
    bid_ingest_telemetry(con),
    "not valid or has been closed"
  )

  unlink(temp_db)
})

test_that("bid_ingest_telemetry works with DBI connection", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # Create event_data table
  events_df <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00", "2025-01-01 10:00:05")),
    session_id = c("s1", "s1"),
    event_type = c("login", "input"),
    input_id = c(NA, "btn1"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "event_data", events_df)

  result <- bid_ingest_telemetry(con)

  expect_s3_class(result, "bid_issues")
  expect_true(DBI::dbIsValid(con)) # Connection should remain open

  DBI::dbDisconnect(con)
  unlink(temp_db)
})

# ----------------------------------------------------------------------------
# TABLE NAME AUTO-DETECTION
# ----------------------------------------------------------------------------

test_that("read_telemetry_sqlite auto-detects event_data table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  events_df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00"),
    session_id = "s1",
    event_type = "login",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "event_data", events_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_telemetry_sqlite(temp_db, NULL, NULL)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)

  unlink(temp_db)
})

test_that("read_telemetry_sqlite auto-detects events table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  events_df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00"),
    session_id = "s1",
    event_type = "login",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "events", events_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_telemetry_sqlite(temp_db, NULL, NULL)
  expect_true(is.data.frame(result))

  unlink(temp_db)
})

test_that("read_telemetry_sqlite falls back to first table with warning", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  events_df <- data.frame(
    timestamp = as.POSIXct("2025-01-01 10:00:00"),
    session_id = "s1",
    event_type = "login",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "custom_table_name", events_df)
  DBI::dbDisconnect(con)

  expect_warning(
    result <- bidux:::read_telemetry_sqlite(temp_db, NULL, NULL),
    "No standard event table found"
  )

  expect_true(is.data.frame(result))

  unlink(temp_db)
})

test_that("read_telemetry_sqlite errors on empty database", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbDisconnect(con)

  expect_error(
    bidux:::read_telemetry_sqlite(temp_db, NULL, NULL),
    "No tables found"
  )

  unlink(temp_db)
})

test_that("read_telemetry_sqlite errors on nonexistent table_name", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbWriteTable(con, "other_table", data.frame(x = 1))
  DBI::dbDisconnect(con)

  expect_error(
    bidux:::read_telemetry_sqlite(temp_db, NULL, "nonexistent"),
    "Table .* not found"
  )

  unlink(temp_db)
})

# ----------------------------------------------------------------------------
# CHECK_JSON_DEPTH SECURITY VALIDATION
# ----------------------------------------------------------------------------

test_that("check_json_depth passes for valid depth", {
  simple_json <- list(a = 1, b = list(c = 2))
  expect_true(bidux:::check_json_depth(simple_json, max_depth = 50))
})

test_that("check_json_depth rejects excessive nesting", {
  # create deeply nested structure using recursive approach
  # r copies lists so we need to build from inside out
  deeply_nested <- list()
  for (i in 60:1) {
    deeply_nested <- list(deeply_nested)
  }

  expect_error(
    bidux:::check_json_depth(deeply_nested, max_depth = 50),
    "nesting depth exceeds security limit"
  )
})

test_that("check_json_depth handles non-list objects", {
  # Non-list objects should pass
  expect_true(bidux:::check_json_depth("string", max_depth = 50))
  expect_true(bidux:::check_json_depth(123, max_depth = 50))
  expect_true(bidux:::check_json_depth(c(1, 2, 3), max_depth = 50))
})

# ----------------------------------------------------------------------------
# THRESHOLDS PARAMETER VALIDATION
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry validates thresholds parameter", {
  temp_file <- tempfile(fileext = ".json")
  writeLines(
    '{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    temp_file
  )

  expect_error(
    bid_ingest_telemetry(temp_file, thresholds = "not a list"),
    "thresholds parameter must be a list"
  )

  unlink(temp_file)
})

# ----------------------------------------------------------------------------
# FORMAT PARAMETER VALIDATION
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry validates format parameter", {
  temp_file <- tempfile(fileext = ".json")
  writeLines(
    '{"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"}',
    temp_file
  )

  expect_error(
    bid_ingest_telemetry(temp_file, format = "xml"),
    "Format must be 'sqlite' or 'json'"
  )

  unlink(temp_file)
})

# ----------------------------------------------------------------------------
# NORMALIZE_TELEMETRY_COLUMNS EDGE CASES
# ----------------------------------------------------------------------------

test_that("normalize_telemetry_columns handles all valid rows being filtered", {
  events_all_empty <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:00:00")),
    session_id = c(" "), # whitespace only
    event_type = c("login"),
    stringsAsFactors = FALSE
  )

  expect_error(
    bidux:::normalize_telemetry_columns(events_all_empty),
    "No valid events found after filtering"
  )
})

test_that("normalize_telemetry_columns converts nested list to data frame", {
  events_list <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "s1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:01:00",
      session_id = "s2",
      event_type = "input"
    )
  )

  result <- bidux:::normalize_telemetry_columns(events_list)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
})

test_that("normalize_telemetry_columns sorts by timestamp", {
  events_unsorted <- data.frame(
    timestamp = as.POSIXct(c("2025-01-01 10:01:00", "2025-01-01 10:00:00")),
    session_id = c("s1", "s2"),
    event_type = c("input", "login"),
    stringsAsFactors = FALSE
  )

  result <- bidux:::normalize_telemetry_columns(events_unsorted)

  # First event should be the earlier one
  expect_equal(result$session_id[1], "s2")
})

# ----------------------------------------------------------------------------
# READ_TELEMETRY_JSON ERROR HANDLING
# ----------------------------------------------------------------------------

test_that("read_telemetry_json handles JSON array format", {
  temp_file <- tempfile(fileext = ".json")
  json_content <- '[
    {"timestamp": "2025-01-01 10:00:00", "session_id": "s1", "event_type": "login"},
    {"timestamp": "2025-01-01 10:01:00", "session_id": "s2", "event_type": "input"}
  ]'
  writeLines(json_content, temp_file)

  result <- bidux:::read_telemetry_json(temp_file)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)

  unlink(temp_file)
})

test_that("read_telemetry_json handles completely invalid JSON", {
  temp_file <- tempfile(fileext = ".json")
  writeLines("not json at all {{{", temp_file)

  expect_error(
    bidux:::read_telemetry_json(temp_file),
    "Error reading JSON file"
  )

  unlink(temp_file)
})

# ----------------------------------------------------------------------------
# DETECT_OTEL_JSON EDGE CASES
# ----------------------------------------------------------------------------

test_that("detect_otel_json returns FALSE for invalid JSON", {
  temp_file <- tempfile(fileext = ".json")
  writeLines("{invalid json", temp_file)

  result <- bidux:::detect_otel_json(temp_file)
  expect_false(result)

  unlink(temp_file)
})

test_that("detect_otel_json returns FALSE for non-OTLP JSON", {
  temp_file <- tempfile(fileext = ".json")
  writeLines('{"key": "value"}', temp_file)

  result <- bidux:::detect_otel_json(temp_file)
  expect_false(result)

  unlink(temp_file)
})

test_that("detect_otel_json returns FALSE for empty resourceSpans", {
  temp_file <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(resourceSpans = list()),
    temp_file,
    auto_unbox = TRUE
  )

  result <- bidux:::detect_otel_json(temp_file)
  expect_false(result)

  unlink(temp_file)
})

test_that("detect_otel_json returns FALSE for missing scopeSpans", {
  temp_file <- tempfile(fileext = ".json")
  otlp_partial <- list(
    resourceSpans = list(
      list(resource = list())
    )
  )
  jsonlite::write_json(otlp_partial, temp_file, auto_unbox = TRUE)

  result <- bidux:::detect_otel_json(temp_file)
  expect_false(result)

  unlink(temp_file)
})

test_that("detect_otel_json returns TRUE for valid OTLP structure", {
  temp_file <- tempfile(fileext = ".json")
  otlp_valid <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc",
                spanId = "123",
                name = "test"
              )
            )
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_valid, temp_file, auto_unbox = TRUE)

  result <- bidux:::detect_otel_json(temp_file)
  expect_true(result)

  unlink(temp_file)
})
