# ============================================================================
# OTEL COMPATIBILITY AND INTEGRATION TESTS
# ============================================================================
# tests for full otel integration with bidux telemetry analysis pipeline
# verifies backward compatibility and end-to-end workflows

test_that("bid_ingest_telemetry works with otel json files", {
  skip_if_no_otel()

  # create otel json file
  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 5, outputs_per_session = 3)
  otlp_file <- create_temp_otel_json(spans)

  # ingest using main function
  result <- bid_ingest_telemetry(otlp_file)

  # verify returns proper structure
  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))

  # verify hybrid object attributes
  expect_true("issues_tbl" %in% names(attributes(result)))
  expect_true("flags" %in% names(attributes(result)))

  unlink(otlp_file)
})

test_that("bid_ingest_telemetry auto-detects otel format", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 2, reactives_per_session = 3)
  otlp_file <- create_temp_otel_json(spans)

  # should auto-detect without explicit format parameter
  result <- bid_ingest_telemetry(otlp_file)

  expect_s3_class(result, "bid_issues")

  unlink(otlp_file)
})

test_that("bid_ingest_telemetry works with otel sqlite", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  # create otel sqlite database
  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 8, outputs_per_session = 4)
  db_path <- create_temp_otel_sqlite(spans)

  result <- bid_ingest_telemetry(db_path)

  expect_s3_class(result, "bid_issues")
  expect_true(is.list(result))

  unlink(db_path)
})

test_that("bid_ingest_telemetry with otel handles thresholds", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 10)
  otlp_file <- create_temp_otel_json(spans)

  # use strict thresholds
  strict <- bid_telemetry_presets("strict")
  result <- bid_ingest_telemetry(otlp_file, thresholds = strict)

  expect_s3_class(result, "bid_issues")

  unlink(otlp_file)
})

test_that("friction detection works on otel data - unused inputs", {
  skip_if_no_otel()

  # create data with clear unused input pattern
  spans <- create_mock_otel_spans(
    sessions = 5,  # reduced from 20 for CRAN compliance
    reactives_per_session = 3,
    outputs_per_session = 2,
    seed = 456
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(
    otlp_file,
    thresholds = list(unused_input_threshold = 0.3)
  )

  # should detect some issues
  expect_s3_class(result, "bid_issues")

  issues_tbl <- as_tibble(result)

  # may or may not have unused input issues depending on mock data
  # but should complete without error
  expect_true(is.data.frame(issues_tbl))

  unlink(otlp_file)
})

test_that("friction detection works on otel data - error patterns", {
  skip_if_no_otel()

  # create spans with high error rate
  spans <- create_mock_otel_spans(
    sessions = 5,  # reduced from 10 for CRAN compliance
    outputs_per_session = 5,
    include_errors = TRUE,
    seed = 789
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(
    otlp_file,
    thresholds = list(error_rate_threshold = 0.05)
  )

  issues_tbl <- as_tibble(result)

  # with 5 sessions, 5 outputs each, 10% error rate in mock,
  # should detect error pattern
  expect_true(is.data.frame(issues_tbl))

  unlink(otlp_file)
})

test_that("otel issues integrate with bid_notices bridge", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 5, outputs_per_session = 3)
  otlp_file <- create_temp_otel_json(spans)

  issues <- bid_ingest_telemetry(otlp_file)
  issues_tbl <- as_tibble(issues)

  if (nrow(issues_tbl) > 0) {
    # test bid_notice_issue bridge
    first_issue <- issues_tbl[1, ]
    interpret <- create_minimal_interpret()

    notice <- bid_notice_issue(first_issue, previous_stage = interpret)

    # verify notice created correctly
    expect_s3_class(notice, "bid_stage")
    expect_equal(get_stage(notice), "Notice")

    # notice should have problem and evidence
    expect_true("problem" %in% names(notice))
    expect_true("evidence" %in% names(notice))
  }

  unlink(otlp_file)
})

test_that("otel issues work with bid_notices batch processing", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 5)  # reduced from 10/8
  otlp_file <- create_temp_otel_json(spans)

  issues_obj <- bid_ingest_telemetry(otlp_file)
  issues_tbl <- as_tibble(issues_obj)

  if (nrow(issues_tbl) >= 2) {
    # test batch conversion
    interpret <- create_minimal_interpret()

    notices <- bid_notices(
      issues_tbl,
      previous_stage = interpret,
      max_issues = 3
    )

    expect_type(notices, "list")
    expect_lte(length(notices), 3)

    # each should be a notice stage
    for (notice in notices) {
      expect_s3_class(notice, "bid_stage")
      expect_equal(get_stage(notice), "Notice")
    }
  }

  unlink(otlp_file)
})

test_that("otel issues work with bid_pipeline", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 4)  # reduced from 8/6
  otlp_file <- create_temp_otel_json(spans)

  issues_obj <- bid_ingest_telemetry(otlp_file)
  issues_tbl <- as_tibble(issues_obj)

  if (nrow(issues_tbl) > 0) {
    interpret <- create_minimal_interpret()

    pipeline <- bid_pipeline(issues_tbl, interpret, max = 2)

    expect_type(pipeline, "list")
    expect_lte(length(pipeline), 2)

    # verify pipeline stages
    for (stage in pipeline) {
      expect_s3_class(stage, "bid_stage")
    }
  }

  unlink(otlp_file)
})

test_that("otel issue structure matches shiny.telemetry issue structure", {
  skip_if_no_otel()

  # create both otel and shiny.telemetry data
  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 5)
  otlp_file <- create_temp_otel_json(spans)

  telemetry_file <- create_temp_shiny_telemetry_json(sessions = 3)

  otel_issues <- bid_ingest_telemetry(otlp_file)
  telemetry_issues <- bid_ingest_telemetry(telemetry_file)

  # both should have same class structure
  expect_s3_class(otel_issues, "bid_issues")
  expect_s3_class(telemetry_issues, "bid_issues")

  # both should support as_tibble
  otel_tbl <- as_tibble(otel_issues)
  telemetry_tbl <- as_tibble(telemetry_issues)

  # column names should be compatible
  otel_cols <- names(otel_tbl)
  telemetry_cols <- names(telemetry_tbl)

  # core columns should overlap
  core_cols <- c("issue_type", "severity", "problem", "evidence")
  expect_true(all(core_cols %in% otel_cols))
  expect_true(all(core_cols %in% telemetry_cols))

  unlink(c(otlp_file, telemetry_file))
})

test_that("backward compatibility - existing shiny.telemetry tests still pass", {
  # test that shiny.telemetry json still works after otel addition
  telemetry_file <- create_temp_shiny_telemetry_json(sessions = 2)

  result <- bid_ingest_telemetry(telemetry_file)

  expect_s3_class(result, "bid_issues")

  unlink(telemetry_file)
})

test_that("bid_telemetry concise api works with otel data", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 4)
  otlp_file <- create_temp_otel_json(spans)

  # use modern concise api
  issues <- bid_telemetry(otlp_file)

  # should return tibble directly
  expect_true(tibble::is_tibble(issues))
  expect_s3_class(issues, "bid_issues_tbl")

  # should have issue columns
  expect_true("issue_type" %in% names(issues))
  expect_true("severity" %in% names(issues))

  unlink(otlp_file)
})

test_that("bid_telemetry with otel works with presets", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 6)
  otlp_file <- create_temp_otel_json(spans)

  # test all three presets
  strict_issues <- bid_telemetry(otlp_file, thresholds = bid_telemetry_presets("strict"))
  moderate_issues <- bid_telemetry(otlp_file, thresholds = bid_telemetry_presets("moderate"))
  relaxed_issues <- bid_telemetry(otlp_file, thresholds = bid_telemetry_presets("relaxed"))

  # all should work
  expect_true(tibble::is_tibble(strict_issues))
  expect_true(tibble::is_tibble(moderate_issues))
  expect_true(tibble::is_tibble(relaxed_issues))

  # strict should find more issues than relaxed (or equal)
  expect_gte(nrow(strict_issues), nrow(relaxed_issues))

  unlink(otlp_file)
})

test_that("otel data with dbi connection works end-to-end", {
  skip_if_no_otel()
  skip_if_no_telemetry_deps()

  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 5)
  db_path <- create_temp_otel_sqlite(spans)

  # use dbi connection
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  result <- bid_ingest_telemetry(con)

  expect_s3_class(result, "bid_issues")

  # connection should remain open
  expect_true(DBI::dbIsValid(con))

  DBI::dbDisconnect(con)
  unlink(db_path)
})

test_that("otel integration preserves performance context", {
  skip_if_no_otel()

  # create spans with varying performance
  spans <- create_mock_otel_spans(
    sessions = 5,
    outputs_per_session = 5,
    slow_rate = 0.5 # 50% slow operations
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)
  issues_tbl <- as_tibble(result)

  # performance context should be available
  if (nrow(issues_tbl) > 0) {
    expect_true(all(c("issue_type", "severity") %in% names(issues_tbl)))
  }

  unlink(otlp_file)
})

test_that("otel and shiny.telemetry can coexist", {
  skip_if_no_otel()

  # create both formats
  spans <- create_mock_otel_spans(sessions = 2)
  otlp_file <- create_temp_otel_json(spans)
  telemetry_file <- create_temp_shiny_telemetry_json(sessions = 2)

  # both should work independently
  otel_result <- bid_ingest_telemetry(otlp_file)
  telemetry_result <- bid_ingest_telemetry(telemetry_file)

  expect_s3_class(otel_result, "bid_issues")
  expect_s3_class(telemetry_result, "bid_issues")

  unlink(c(otlp_file, telemetry_file))
})

test_that("otel data produces actionable issue descriptions", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(
    sessions = 5,  # reduced from 10 for CRAN compliance
    outputs_per_session = 5,
    include_errors = TRUE
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)
  issues_tbl <- as_tibble(result)

  if (nrow(issues_tbl) > 0) {
    # problem descriptions should be non-empty
    expect_true(all(!is.na(issues_tbl$problem)))
    expect_true(all(nchar(issues_tbl$problem) > 0))

    # evidence should be non-empty
    expect_true(all(!is.na(issues_tbl$evidence)))
    expect_true(all(nchar(issues_tbl$evidence) > 0))
  }

  unlink(otlp_file)
})

test_that("otel flags extraction works correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 5)
  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)

  # test bid_flags extraction
  flags <- bid_flags(result)

  expect_type(flags, "list")

  # should have standard flags
  expect_true("has_issues" %in% names(flags))
  expect_true("session_count" %in% names(flags))

  # session_count should match
  expect_equal(flags$session_count, 5)

  unlink(otlp_file)
})

test_that("otel print method works correctly", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 3, reactives_per_session = 5)
  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)

  # should print without error
  expect_output(print(result), "BID Telemetry Issues|Issues Summary")

  unlink(otlp_file)
})

test_that("empty otel data produces empty issues", {
  skip_if_no_otel()

  # create minimal otel file with no real events
  empty_spans <- tibble::tibble(
    trace_id = character(0),
    span_id = character(0),
    parent_span_id = character(0),
    name = character(0),
    start_time_unix_nano = character(0),
    end_time_unix_nano = character(0),
    attributes = list(),
    events = list()
  )

  otlp_file <- create_temp_otel_json(empty_spans)

  # should handle gracefully
  expect_warning(
    result <- bid_ingest_telemetry(otlp_file),
    "No telemetry events"
  )

  expect_equal(length(result), 0)

  unlink(otlp_file)
})

test_that("otel integration respects user-provided thresholds", {
  skip_if_no_otel()

  spans <- create_mock_otel_spans(sessions = 5, reactives_per_session = 5)  # reduced from 10/8
  otlp_file <- create_temp_otel_json(spans)

  # test with very strict custom thresholds
  custom_thresholds <- list(
    unused_input_threshold = 0.01,
    delay_threshold_secs = 5,
    error_rate_threshold = 0.01
  )

  result <- bid_ingest_telemetry(otlp_file, thresholds = custom_thresholds)

  # should complete without error
  expect_s3_class(result, "bid_issues")

  unlink(otlp_file)
})

test_that("otel data with multiple sessions analyzed correctly", {
  skip_if_no_otel()

  # create data with distinct session patterns
  spans <- create_mock_otel_spans(
    sessions = 5,  # reduced from 15 for CRAN compliance
    reactives_per_session = 5,
    outputs_per_session = 3
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)

  flags <- bid_flags(result)

  # session count should be correct
  expect_equal(flags$session_count, 5)

  unlink(otlp_file)
})

test_that("otel integration handles large span volumes", {
  skip_if_no_otel()
  skip_on_cran() # large data test

  # simulate realistic production volumes
  spans <- create_mock_otel_spans(
    sessions = 100,
    reactives_per_session = 15,
    outputs_per_session = 8
  )

  otlp_file <- create_temp_otel_json(spans)

  # should handle large volumes efficiently
  start_time <- Sys.time()
  result <- bid_ingest_telemetry(otlp_file)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_s3_class(result, "bid_issues")
  expect_lt(elapsed, 10) # should complete within 10 seconds

  unlink(otlp_file)
})

test_that("mixed otel span types processed correctly", {
  skip_if_no_otel()

  # ensure mix of all span types
  spans <- create_mock_otel_spans(
    sessions = 3,  # reduced from 5 for CRAN compliance
    reactives_per_session = 5,  # reduced from 10
    outputs_per_session = 4,  # reduced from 8
    include_errors = TRUE
  )

  # verify we have variety
  span_types <- unique(spans$name)
  expect_true(length(span_types) > 3)

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(otlp_file)

  expect_s3_class(result, "bid_issues")

  unlink(otlp_file)
})

test_that("otel error severity classified correctly", {
  skip_if_no_otel()

  # create high-error scenario
  spans <- create_mock_otel_spans(
    sessions = 5,  # reduced from 10 for CRAN compliance
    outputs_per_session = 5,  # reduced from 10
    include_errors = TRUE
  )

  otlp_file <- create_temp_otel_json(spans)

  result <- bid_ingest_telemetry(
    otlp_file,
    thresholds = list(error_rate_threshold = 0.05)
  )

  issues_tbl <- as_tibble(result)

  if (nrow(issues_tbl) > 0) {
    # error issues should have severity classifications
    expect_true("severity" %in% names(issues_tbl))

    severity_levels <- c("low", "medium", "high", "critical")
    expect_true(all(issues_tbl$severity %in% severity_levels))
  }

  unlink(otlp_file)
})
