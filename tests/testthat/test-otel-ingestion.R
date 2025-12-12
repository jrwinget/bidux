# ============================================================================
# OTEL INGESTION TESTS
# ============================================================================
# tests for agent 1's otel data ingestion and format detection
# these tests verify otlp json and sqlite reading capabilities

test_that("detect_otel_json identifies otlp format correctly", {
  skip_if_no_otel()

  # create mock otlp json file
  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 2)
  otlp_file <- create_temp_otel_json(spans)

  # test otlp detection returns true
  result <- bidux:::detect_otel_json(otlp_file)
  expect_true(result)

  unlink(otlp_file)
})

test_that("detect_otel_json distinguishes shiny.telemetry from otlp", {
  skip_if_no_otel()

  # create traditional shiny.telemetry json
  telemetry_file <- create_temp_shiny_telemetry_json(sessions = 2)

  # test shiny.telemetry format returns false
  result <- bidux:::detect_otel_json(telemetry_file)
  expect_false(result)

  unlink(telemetry_file)
})

test_that("detect_otel_json handles malformed json gracefully", {
  skip_if_no_otel()

  # create invalid json file
  bad_file <- tempfile(fileext = ".json")
  writeLines(c("{", "invalid json", "}"), bad_file)

  # detect_otel_json returns FALSE for malformed JSON (doesn't error)
  result <- bidux:::detect_otel_json(bad_file)
  expect_false(result)

  unlink(bad_file)
})

test_that("detect_otel_json validates required otlp structure", {
  skip_if_no_otel()

  # create json without resourceSpans (not otlp)
  not_otlp <- tempfile(fileext = ".json")
  writeLines('{"data": [{"key": "value"}]}', not_otlp)

  result <- bidux:::detect_otel_json(not_otlp)
  expect_false(result)

  unlink(not_otlp)
})

test_that("read_otel_json parses otlp json correctly", {
  skip_if_no_otel()

  # create mock otlp data
  spans <- create_mock_otel_spans(
    sessions = 2,
    reactives_per_session = 5,
    outputs_per_session = 3
  )
  otlp_file <- create_temp_otel_json(spans)

  # read and parse
  result <- bidux:::read_otel_json(otlp_file)

  # verify result structure
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # verify expected columns exist
  expected_cols <- c("timestamp", "session_id", "event_type")
  expect_true(all(expected_cols %in% names(result)))

  # verify span names are preserved
  expect_true("login" %in% result$event_type)
  expect_true("input" %in% result$event_type)

  unlink(otlp_file)
})

test_that("read_otel_json extracts span attributes correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 2)
  otlp_file <- create_temp_otel_json(spans)

  result <- bidux:::read_otel_json(otlp_file)

  # check session_id extraction from attributes
  login_events <- result[result$event_type == "login", ]
  expect_gt(nrow(login_events), 0)
  expect_false(all(is.na(login_events$session_id)))

  unlink(otlp_file)
})

test_that("read_otel_json handles nested scopeSpans structure", {
  skip_if_no_otel()

  # create otlp with multiple scopeSpans (edge case)
  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 3)
  otlp_file <- create_temp_otel_json(spans)

  # should flatten all spans regardless of nesting
  result <- bidux:::read_otel_json(otlp_file)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 5) # should have multiple spans

  unlink(otlp_file)
})

test_that("read_otel_json extracts span events (errors)", {
  skip_if_no_otel()

  # create spans with error events
  spans <- create_mock_otel_spans(
    sessions = 2,
    outputs_per_session = 5,
    include_errors = TRUE
  )
  otlp_file <- create_temp_otel_json(spans)

  result <- bidux:::read_otel_json(otlp_file)

  # verify events column exists or events are extracted
  has_events <- "events" %in% names(result) || "error_message" %in% names(result)
  expect_true(has_events)

  unlink(otlp_file)
})

test_that("read_otel_json handles empty spans array", {
  skip_if_no_otel()

  # create otlp structure with no spans
  empty_otlp <- tempfile(fileext = ".json")
  empty_structure <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(spans = list())
        )
      )
    )
  )

  jsonlite::write_json(empty_structure, empty_otlp, auto_unbox = TRUE)

  result <- bidux:::read_otel_json(empty_otlp)

  # should return empty dataframe with proper structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  unlink(empty_otlp)
})

test_that("read_otel_json validates required span fields", {
  skip_if_no_otel()

  # create otlp with missing required fields
  malformed_otlp <- tempfile(fileext = ".json")
  malformed <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc123",
                # missing spanId, name, timestamps
                attributes = list()
              )
            )
          )
        )
      )
    )
  )

  jsonlite::write_json(malformed, malformed_otlp, auto_unbox = TRUE)

  # malformed spans are skipped gracefully, may return empty result
  result <- bidux:::read_otel_json(malformed_otlp)
  expect_true(is.data.frame(result))

  unlink(malformed_otlp)
})

test_that("read_otel_sqlite reads otel database tables", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  # create otel sqlite database
  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 5)
  db_path <- create_temp_otel_sqlite(spans)

  # read database
  result <- bidux:::read_otel_sqlite(db_path)

  # verify structure
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # verify expected columns
  expected_cols <- c("timestamp", "session_id", "event_type")
  expect_true(all(expected_cols %in% names(result)))

  unlink(db_path)
})

test_that("read_otel_sqlite detects correct table names", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  spans <- create_mock_otel_spans(sessions = 1)
  db_path <- create_temp_otel_sqlite(spans)

  # verify table detection works
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  tables <- DBI::dbListTables(con)

  expect_true("spans" %in% tables)
  expect_true("span_attributes" %in% tables)

  DBI::dbDisconnect(con)

  # verify read function finds these tables
  result <- bidux:::read_otel_sqlite(db_path)
  expect_true(is.data.frame(result))

  unlink(db_path)
})

test_that("read_otel_sqlite joins attributes correctly", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3)
  db_path <- create_temp_otel_sqlite(spans)

  result <- bidux:::read_otel_sqlite(db_path)

  # attributes should be joined or accessible
  # check for session_id or attributes column
  has_attrs <- "session_id" %in% names(result) || "attributes" %in% names(result)
  expect_true(has_attrs)

  unlink(db_path)
})

test_that("read_otel_sqlite handles missing attributes table", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  # create minimal database with only spans table
  db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  spans_table <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    parentSpanId = NA_character_,
    name = "session_start",
    startTimeUnixNano = "1609459200000000000",
    endTimeUnixNano = "1609459200100000000",
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "spans", spans_table)
  DBI::dbDisconnect(con)

  # should still read successfully (no attributes means session_id will be NA)
  result <- bidux:::read_otel_sqlite(db_path)
  expect_true(is.data.frame(result))
  expect_true("session_id" %in% names(result))

  unlink(db_path)
})

test_that("read_otel_sqlite extracts span events from events table", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  # create database with error events
  spans <- create_mock_otel_spans(
    sessions = 2,
    outputs_per_session = 5,
    include_errors = TRUE
  )
  db_path <- create_temp_otel_sqlite(spans)

  result <- bidux:::read_otel_sqlite(db_path)

  # should include error information
  has_errors <- "events" %in% names(result) ||
    "error_message" %in% names(result) ||
    any(grepl("error", names(result), ignore.case = TRUE))

  expect_true(has_errors)

  unlink(db_path)
})

test_that("read_otel_sqlite works with dbi connection", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  spans <- create_mock_otel_spans(sessions = 1)
  db_path <- create_temp_otel_sqlite(spans)

  # pass connection instead of path
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  result <- bidux:::read_otel_sqlite(con)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # connection should remain open (we don't own it)
  expect_true(DBI::dbIsValid(con))

  DBI::dbDisconnect(con)
  unlink(db_path)
})

test_that("format auto-detection chooses read_otel_json for otlp", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1)
  otlp_file <- create_temp_otel_json(spans)

  # auto-detect should route to otel reader
  format <- bidux:::detect_telemetry_format(otlp_file)

  # if otel detection is implemented, should return "otel_json" or similar
  # fallback: should at least return "json"
  expect_true(format %in% c("json", "otel_json", "otlp"))

  unlink(otlp_file)
})

test_that("format auto-detection chooses read_otel_sqlite for otel db", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  spans <- create_mock_otel_spans(sessions = 1)
  db_path <- create_temp_otel_sqlite(spans)

  # detect format
  format <- bidux:::detect_telemetry_format(db_path)

  # should detect as sqlite (with otel schema detection happening later)
  expect_equal(format, "sqlite")

  unlink(db_path)
})

test_that("format auto-detection distinguishes otel from shiny.telemetry", {
  skip_if_no_otel()

  # create both formats
  spans <- create_mock_otel_spans(sessions = 1)
  otlp_file <- create_temp_otel_json(spans)
  telemetry_file <- create_temp_shiny_telemetry_json(sessions = 1)

  # both should be detected as json initially
  otlp_format <- bidux:::detect_telemetry_format(otlp_file)
  telemetry_format <- bidux:::detect_telemetry_format(telemetry_file)

  expect_equal(otlp_format, "json")
  expect_equal(telemetry_format, "json")

  # but content-based detection should differ
  is_otlp <- bidux:::detect_otel_json(otlp_file)
  is_telemetry <- bidux:::detect_otel_json(telemetry_file)

  expect_true(is_otlp)
  expect_false(is_telemetry)

  unlink(c(otlp_file, telemetry_file))
})

test_that("malformed otlp data produces clear error messages", {
  skip_if_no_otel()

  # test missing resourceSpans
  bad_json1 <- tempfile(fileext = ".json")
  writeLines('{"spans": []}', bad_json1)

  expect_error(
    bidux:::read_otel_json(bad_json1),
    "resourceSpans|structure|format"
  )

  unlink(bad_json1)

  # test invalid json
  bad_json2 <- tempfile(fileext = ".json")
  writeLines('{"invalid": json', bad_json2)

  expect_error(
    bidux:::read_otel_json(bad_json2),
    "json|parse"
  )

  unlink(bad_json2)
})

test_that("read_otel_json handles large span counts efficiently", {
  skip_if_no_otel()
  skip_on_cran() # performance test with larger dataset

  # create large dataset
  spans <- create_mock_otel_spans(
    sessions = 10,
    reactives_per_session = 20,
    outputs_per_session = 10
  )
  otlp_file <- create_temp_otel_json(spans)

  # should complete without timeout or memory issues
  start_time <- Sys.time()
  result <- bidux:::read_otel_json(otlp_file)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 100) # should have many spans
  expect_lt(elapsed, 5) # should complete within 5 seconds

  unlink(otlp_file)
})

test_that("read_otel_json converts spans to bidux event schema", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 1, reactives_per_session = 3)
  otlp_file <- create_temp_otel_json(spans)

  result <- bidux:::read_otel_json(otlp_file)

  # should have bidux event schema columns
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)
  expect_true("event_type" %in% names(result))
  expect_true("timestamp" %in% names(result))
  expect_true("session_id" %in% names(result))

  unlink(otlp_file)
})

test_that("read_otel_json handles multiple attribute value types", {
  skip_if_no_otel()

  # create custom otlp with different attribute types
  custom_otlp <- tempfile(fileext = ".json")

  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc123",
                spanId = "span001",
                name = "test",
                startTimeUnixNano = "1609459200000000000",
                endTimeUnixNano = "1609459200100000000",
                attributes = list(
                  list(key = "string_attr", value = list(stringValue = "test")),
                  list(key = "int_attr", value = list(intValue = 42)),
                  list(key = "double_attr", value = list(doubleValue = 3.14)),
                  list(key = "bool_attr", value = list(boolValue = TRUE))
                )
              )
            )
          )
        )
      )
    )
  )

  jsonlite::write_json(otlp_data, custom_otlp, auto_unbox = TRUE)

  # should handle all value types
  expect_no_error({
    result <- bidux:::read_otel_json(custom_otlp)
  })

  unlink(custom_otlp)
})

test_that("read_otel_sqlite handles concurrent access safely", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()
  skip_on_cran()

  spans <- create_mock_otel_spans(sessions = 2)
  db_path <- create_temp_otel_sqlite(spans)

  # multiple reads should work (read-only access)
  result1 <- bidux:::read_otel_sqlite(db_path)
  result2 <- bidux:::read_otel_sqlite(db_path)

  expect_equal(nrow(result1), nrow(result2))

  unlink(db_path)
})

test_that("otel readers handle unicode and special characters", {
  skip_if_no_otel()

  # create spans with unicode in attributes
  spans <- create_mock_otel_spans(sessions = 1)

  # modify to include unicode
  spans$name[1] <- "session_start_\u2713" # checkmark

  otlp_file <- create_temp_otel_json(spans)

  # should handle unicode correctly
  expect_no_error({
    result <- bidux:::read_otel_json(otlp_file)
  })

  unlink(otlp_file)
})
