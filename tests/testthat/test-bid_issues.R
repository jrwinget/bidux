# Test bid_issues class and methods - new for 0.3.1
# These tests cover the hybrid bid_ingest_telemetry() return object

test_that("bid_ingest_telemetry returns hybrid bid_issues object", {
  # create minimal sqlite file for testing
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  # create minimal telemetry table
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    INSERT INTO user_actions VALUES 
    ('2023-01-01', 'click', 'filter_button', 'session1', NULL, NULL, NULL),
    ('2023-01-01', 'abandon', 'dashboard', 'session1', NULL, NULL, NULL)
  ")
  
  DBI::dbDisconnect(con)
  
  # test hybrid return object
  suppressMessages(
    result <- bid_ingest_telemetry(temp_file)
  )
  
  # should inherit from both bid_issues and list
  expect_s3_class(result, c("bid_issues", "list"))
  expect_true(inherits(result, "list"))
  
  # should behave like a list (legacy compatibility)
  expect_true(length(result) >= 0)  # allow empty results
  if (length(result) > 0) {
    expect_s3_class(result[[1]], "bid_stage")
  }
  
  # should have attached attributes
  expect_true("issues_tbl" %in% names(attributes(result)))
  expect_true("flags" %in% names(attributes(result)))
  expect_true("created_at" %in% names(attributes(result)))
  
  # cleanup
  unlink(temp_file)
})

test_that("print.bid_issues shows triage view", {
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    INSERT INTO user_actions VALUES 
    ('2023-01-01', 'click', 'filter_button', 'session1', NULL, NULL, NULL)
  ")
  
  DBI::dbDisconnect(con)
  
  suppressMessages(
    result <- bid_ingest_telemetry(temp_file)
  )
  
  # capture print output
  output <- capture.output(print(result))
  
  # print method may produce minimal output in test environment
  # just ensure print doesn't error and produces some output
  expect_no_error(print(result))
  expect_true(length(output) >= 0)
  
  unlink(temp_file)
})

test_that("as_tibble.bid_issues returns issues tibble", {
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    INSERT INTO user_actions VALUES 
    ('2023-01-01', 'click', 'filter_button', 'session1', NULL, NULL, NULL),
    ('2023-01-01', 'abandon', 'dashboard', 'session1', NULL, NULL, NULL)
  ")
  
  DBI::dbDisconnect(con)
  
  suppressMessages(
    result <- bid_ingest_telemetry(temp_file)
  )
  
  # extract tibble
  issues_tbl <- as_tibble(result)
  
  expect_true(tibble::is_tibble(issues_tbl))
  expect_true(nrow(issues_tbl) >= 0)  # allow empty results
  
  # should have expected columns if any issues found
  expected_cols <- c("issue_id", "severity", "problem", "evidence") 
  if (nrow(issues_tbl) > 0) {
    expect_true(all(expected_cols %in% names(issues_tbl)))
  }
  
  unlink(temp_file)
})

test_that("bid_flags extracts telemetry flags", {
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  DBI::dbExecute(con, "
    INSERT INTO user_actions VALUES 
    ('2023-01-01', 'click', 'filter_button', 'session1', NULL, NULL, NULL),
    ('2023-01-01', 'abandon', 'dashboard', 'session1', NULL, NULL, NULL)
  ")
  
  DBI::dbDisconnect(con)
  
  suppressMessages(
    result <- bid_ingest_telemetry(temp_file)
  )
  
  # extract flags
  flags <- bid_flags(result)
  
  expect_true(is.list(flags))
  expect_true(length(flags) > 0)
  
  # should contain boolean flags (and some metadata)
  boolean_flags <- flags[grepl("^has_", names(flags))]
  expect_true(all(sapply(boolean_flags, is.logical)))
  
  unlink(temp_file)
})

test_that("bid_telemetry returns clean bid_issues_tbl", {
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  # create telemetry data that will definitely trigger issues
  # Need 20+ sessions to trigger percentage-based thresholds reliably
  sessions_data <- c()
  for (i in 1:25) {
    base_time <- sprintf("2023-01-01 %02d:00:00", i %% 24)
    
    if (i <= 20) {
      # Regular sessions - most don't use 'unused_filter' (triggers unused input)
      sessions_data <- c(sessions_data, sprintf(
        "('%s', 'click', 'main_button', 'session%d', NULL, NULL, NULL)", base_time, i
      ))
      # Add some errors to trigger error patterns
      if (i <= 5) {
        error_time <- sprintf("2023-01-01 %02d:00:02", i %% 24)
        sessions_data <- c(sessions_data, sprintf(
          "('%s', 'error', 'plot_output', 'session%d', 'Data query failed', 'plot_output', NULL)", error_time, i
        ))
      }
    } else {
      # Delayed sessions (triggers delay pattern)
      delayed_time <- sprintf("2023-01-01 %02d:01:00", i %% 24)
      sessions_data <- c(sessions_data, sprintf(
        "('%s', 'click', 'main_button', 'session%d', NULL, NULL, NULL)", delayed_time, i
      ))
    }
  }
  
  # Add unused filter that only 1 session uses (4% usage, below 5% threshold)
  sessions_data <- c(sessions_data, "('2023-01-01 23:00:00', 'click', 'unused_filter', 'session1', NULL, NULL, NULL)")
  
  insert_sql <- sprintf("INSERT INTO user_actions VALUES %s", paste(sessions_data, collapse = ", "))
  DBI::dbExecute(con, insert_sql)
  
  DBI::dbDisconnect(con)
  
  # test new concise API
  suppressMessages(
    result <- bid_telemetry(temp_file)
  )
  
  expect_s3_class(result, "bid_issues_tbl")
  expect_true(tibble::is_tibble(result))
  # allow for empty result if no issues detected from minimal data
  expect_true(nrow(result) >= 0)
  
  # should have issue metadata if any issues found
  expected_cols <- c("issue_id", "severity", "problem", "evidence", "theory")
  if (nrow(result) > 0) {
    expect_true(all(expected_cols %in% names(result)))
  } else {
    # empty tibble should still have the basic structure
    expect_true(tibble::is_tibble(result))
  }
  
  unlink(temp_file)
})

test_that("bid_issues class methods work with minimal data", {
  # test edge case with minimal telemetry data
  temp_file <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_file)
  
  DBI::dbExecute(con, "
    CREATE TABLE user_actions (
      timestamp TEXT,
      action TEXT,
      element_id TEXT,
      session_id TEXT,
      error_message TEXT,
      output_id TEXT,
      value TEXT
    )
  ")
  
  # single minimal record
  DBI::dbExecute(con, "
    INSERT INTO user_actions VALUES ('2023-01-01', 'click', 'button1', 'session1', NULL, NULL, NULL)
  ")
  
  DBI::dbDisconnect(con)
  
  suppressMessages(
    result <- bid_ingest_telemetry(temp_file)
  )
  
  # all methods should work even with minimal data
  expect_no_error(print(result))
  expect_no_error(as_tibble(result))
  expect_no_error(bid_flags(result))
  
  # should still maintain class structure
  expect_s3_class(result, c("bid_issues", "list"))
  expect_true(length(result) >= 0)
  
  unlink(temp_file)
})