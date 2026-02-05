# ----------------------------------------------------------------------------
# READ_OTEL_SQLITE
# ----------------------------------------------------------------------------

test_that("read_otel_sqlite errors on database without spans table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbWriteTable(con, "other_table", data.frame(x = 1))
  DBI::dbDisconnect(con)

  expect_error(
    bidux:::read_otel_sqlite(temp_db),
    "does not contain OTEL span data"
  )

  unlink(temp_db)
})

test_that("read_otel_sqlite warns on empty spans table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = character(0),
    spanId = character(0),
    name = character(0),
    startTimeUnixNano = character(0),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)
  DBI::dbDisconnect(con)

  expect_warning(
    result <- bidux:::read_otel_sqlite(temp_db),
    "No spans found"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  unlink(temp_db)
})

test_that("read_otel_sqlite handles spans without attributes table", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_otel_sqlite(temp_db)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  unlink(temp_db)
})

test_that("read_otel_sqlite joins span_attributes correctly", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  attrs_df <- data.frame(
    span_id = c("span001", "span001"),
    key = c("session.id", "http.method"),
    value = c("test_session", "GET"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_otel_sqlite(temp_db)

  expect_true(is.data.frame(result))
  # Session ID should be extracted from attributes
  expect_false(all(is.na(result$session_id)))

  unlink(temp_db)
})

test_that("read_otel_sqlite joins span_events correctly", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    name = "output:plot1",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  attrs_df <- data.frame(
    span_id = "span001",
    key = "session.id",
    value = "test_session",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)

  events_df <- data.frame(
    span_id = "span001",
    name = "error",
    time_unix_nano = "1735689600050000000",
    error_message = "Test error",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_events", events_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_otel_sqlite(temp_db)

  expect_true(is.data.frame(result))
  # Should have error events
  expect_true(
    "error" %in% result$event_type || any(!is.na(result$error_message))
  )

  unlink(temp_db)
})

test_that("read_otel_sqlite handles duplicate attribute keys with warning", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  # Duplicate keys for same span
  attrs_df <- data.frame(
    span_id = c("span001", "span001", "span001"),
    key = c("session.id", "session.id", "other"),
    value = c("session1", "session2", "value"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)
  DBI::dbDisconnect(con)

  expect_warning(
    result <- bidux:::read_otel_sqlite(temp_db),
    "Duplicate attribute keys"
  )

  expect_true(is.data.frame(result))

  unlink(temp_db)
})

test_that("read_otel_sqlite handles snake_case column names", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # Use snake_case instead of camelCase
  spans_df <- data.frame(
    trace_id = "abc123",
    span_id = "span001",
    parent_span_id = NA_character_,
    name = "session_start",
    start_time = as.numeric(Sys.time()),
    end_time = as.numeric(Sys.time()) + 0.1,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  attrs_df <- data.frame(
    span_id = "span001",
    key = "session.id",
    value = "test_session",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)
  DBI::dbDisconnect(con)

  result <- bidux:::read_otel_sqlite(temp_db)

  expect_true(is.data.frame(result))

  unlink(temp_db)
})

test_that("read_otel_sqlite works with DBI connection object", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = "abc123",
    spanId = "span001",
    name = "session_start",
    startTimeUnixNano = "1735689600000000000",
    endTimeUnixNano = "1735689600100000000",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  attrs_df <- data.frame(
    span_id = "span001",
    key = "session.id",
    value = "test_session",
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)

  # Pass connection instead of path
  result <- bidux:::read_otel_sqlite(con)

  expect_true(is.data.frame(result))
  # Connection should remain open
  expect_true(DBI::dbIsValid(con))

  DBI::dbDisconnect(con)
  unlink(temp_db)
})

# ----------------------------------------------------------------------------
# READ_OTEL_JSON
# ----------------------------------------------------------------------------

test_that("read_otel_json errors on invalid OTLP structure", {
  temp_file <- tempfile(fileext = ".json")
  writeLines('{"not_resource_spans": []}', temp_file)

  expect_error(
    bidux:::read_otel_json(temp_file),
    "Invalid OTLP JSON structure"
  )

  unlink(temp_file)
})

test_that("read_otel_json warns on empty spans", {
  temp_file <- tempfile(fileext = ".json")
  otlp_empty <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(spans = list())
        )
      )
    )
  )
  jsonlite::write_json(otlp_empty, temp_file, auto_unbox = TRUE)

  expect_warning(
    result <- bidux:::read_otel_json(temp_file),
    "No spans found"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)

  unlink(temp_file)
})

test_that("read_otel_json handles spans with all value types", {
  temp_file <- tempfile(fileext = ".json")
  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc123",
                spanId = "span001",
                name = "session_start",
                startTimeUnixNano = "1735689600000000000",
                endTimeUnixNano = "1735689600100000000",
                attributes = list(
                  list(key = "string_val", value = list(stringValue = "test")),
                  list(key = "int_val", value = list(intValue = 42)),
                  list(key = "double_val", value = list(doubleValue = 3.14)),
                  list(key = "bool_val", value = list(boolValue = TRUE)),
                  list(key = "session.id", value = list(stringValue = "s1"))
                ),
                events = list()
              )
            )
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = TRUE)

  result <- bidux:::read_otel_json(temp_file)

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  unlink(temp_file)
})

test_that("read_otel_json handles spans without parentSpanId", {
  temp_file <- tempfile(fileext = ".json")
  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc123",
                spanId = "span001",
                # no parentSpanId
                name = "session_start",
                startTimeUnixNano = "1735689600000000000",
                endTimeUnixNano = "1735689600100000000",
                attributes = list(
                  list(key = "session.id", value = list(stringValue = "s1"))
                ),
                events = list()
              )
            )
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = TRUE)

  result <- bidux:::read_otel_json(temp_file)

  expect_true(is.data.frame(result))

  unlink(temp_file)
})

test_that("read_otel_json handles list-typed IDs", {
  temp_file <- tempfile(fileext = ".json")
  # Create span where IDs might be parsed as lists due to special characters
  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = list("abc123"),
                spanId = list("span001"),
                parentSpanId = list("parent001"),
                name = "session_start",
                startTimeUnixNano = list("1735689600000000000"),
                endTimeUnixNano = list("1735689600100000000"),
                attributes = list(
                  list(key = "session.id", value = list(stringValue = "s1"))
                ),
                events = list()
              )
            )
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = FALSE)

  result <- bidux:::read_otel_json(temp_file)

  expect_true(is.data.frame(result))

  unlink(temp_file)
})

test_that("read_otel_json handles scopeSpans without spans key", {
  temp_file <- tempfile(fileext = ".json")
  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            scope = list(name = "test")
            # no spans key
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = TRUE)

  expect_warning(
    result <- bidux:::read_otel_json(temp_file),
    "No spans found"
  )

  unlink(temp_file)
})

test_that("read_otel_json handles resourceSpans without scopeSpans", {
  temp_file <- tempfile(fileext = ".json")
  otlp_data <- list(
    resourceSpans = list(
      list(
        resource = list(attributes = list())
        # no scopeSpans key
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = TRUE)

  expect_warning(
    result <- bidux:::read_otel_json(temp_file),
    "No spans found"
  )

  unlink(temp_file)
})

# ----------------------------------------------------------------------------
# OTEL FORMAT DETECTION IN BID_INGEST_TELEMETRY
# ----------------------------------------------------------------------------

test_that("bid_ingest_telemetry detects OTEL JSON format automatically", {
  temp_file <- tempfile(fileext = ".json")
  otlp_data <- list(
    resourceSpans = list(
      list(
        scopeSpans = list(
          list(
            spans = list(
              list(
                traceId = "abc123",
                spanId = "span001",
                name = "session_start",
                startTimeUnixNano = "1735689600000000000",
                endTimeUnixNano = "1735689600100000000",
                attributes = list(
                  list(key = "session.id", value = list(stringValue = "s1"))
                ),
                events = list()
              ),
              list(
                traceId = "abc123",
                spanId = "span002",
                name = "output:plot1",
                startTimeUnixNano = "1735689601000000000",
                endTimeUnixNano = "1735689601100000000",
                attributes = list(
                  list(key = "session.id", value = list(stringValue = "s1"))
                ),
                events = list()
              )
            )
          )
        )
      )
    )
  )
  jsonlite::write_json(otlp_data, temp_file, auto_unbox = TRUE)

  result <- bid_ingest_telemetry(temp_file)

  expect_s3_class(result, "bid_issues")

  unlink(temp_file)
})

test_that("bid_ingest_telemetry detects OTEL SQLite format automatically", {
  skip_if_no_telemetry_deps()

  temp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  spans_df <- data.frame(
    traceId = c("abc123", "abc123"),
    spanId = c("span001", "span002"),
    name = c("session_start", "output:plot1"),
    startTimeUnixNano = c("1735689600000000000", "1735689601000000000"),
    endTimeUnixNano = c("1735689600100000000", "1735689601100000000"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "spans", spans_df)

  attrs_df <- data.frame(
    span_id = c("span001", "span002"),
    key = c("session.id", "session.id"),
    value = c("s1", "s1"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "span_attributes", attrs_df)
  DBI::dbDisconnect(con)

  result <- bid_ingest_telemetry(temp_db)

  expect_s3_class(result, "bid_issues")

  unlink(temp_db)
})
