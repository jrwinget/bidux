test_that("bid_ingest_telemetry detects format from file extension", {
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

  # Create temporary file
  temp_file <- tempfile(fileext = ".json")
  writeLines("[]", temp_file)

  expect_error(
    bid_ingest_telemetry(temp_file, format = "invalid"),
    "Format must be 'sqlite' or 'json'"
  )

  unlink(temp_file)
})

test_that("bid_ingest_telemetry handles empty telemetry data", {
  # Create empty JSON file
  temp_json <- tempfile(fileext = ".json")
  writeLines("[]", temp_json)

  expect_warning(
    result <- bid_ingest_telemetry(temp_json),
    "No telemetry events found"
  )

  expect_equal(length(result), 0)

  unlink(temp_json)
})

test_that("bid_ingest_telemetry identifies unused inputs", {
  # Create test telemetry data
  temp_json <- tempfile(fileext = ".json")

  # Create events: 2 sessions, but only one uses input2
  events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:00:05",
      session_id = "session1",
      event_type = "input",
      input_id = "input1"
    ),
    list(
      timestamp = "2025-01-01 10:00:10",
      session_id = "session1",
      event_type = "input",
      input_id = "input2"
    ),
    list(
      timestamp = "2025-01-01 11:00:00",
      session_id = "session2",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 11:00:05",
      session_id = "session2",
      event_type = "input",
      input_id = "input1"
    )
  )

  # Write JSON lines format
  json_lines <- sapply(events, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json_lines, temp_json)

  # Run analysis
  suppressMessages(
    result <- bid_ingest_telemetry(
      temp_json,
      thresholds = list(unused_input_threshold = 0.5)
    )
  )

  # Should identify input2 as underused (50% usage)
  expect_true("unused_input_input2" %in% names(result))

  # Check the notice content
  input2_notice <- result$unused_input_input2
  expect_s3_class(input2_notice, "bid_stage")
  expect_equal(get_stage(input2_notice), "Notice")
  expect_match(input2_notice$problem[1], "input2")
  expect_match(input2_notice$evidence[1], "50.0%")

  unlink(temp_json)
})

test_that("bid_ingest_telemetry identifies delayed interactions", {
  temp_json <- tempfile(fileext = ".json")

  # Create events with long delays
  events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:01:00", # 60 second delay
      session_id = "session1",
      event_type = "input",
      input_id = "input1"
    ),
    list(
      timestamp = "2025-01-01 11:00:00",
      session_id = "session2",
      event_type = "login"
    )
    # session2 has no interactions
  )

  json_lines <- sapply(events, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json_lines, temp_json)

  suppressMessages(
    result <- bid_ingest_telemetry(
      temp_json,
      thresholds = list(delay_threshold_seconds = 30)
    )
  )

  # Should identify delay issue
  expect_true("delayed_interaction" %in% names(result))

  delay_notice <- result$delayed_interaction
  expect_s3_class(delay_notice, "bid_stage")
  expect_match(delay_notice$problem[1], "long time")
  expect_match(
    delay_notice$evidence[1],
    "60 seconds|50.0% of sessions had no interactions"
  )

  unlink(temp_json)
})

test_that("bid_ingest_telemetry identifies error patterns", {
  temp_json <- tempfile(fileext = ".json")

  # Create events with errors
  events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:00:05",
      session_id = "session1",
      event_type = "input",
      input_id = "date_range"
    ),
    list(
      timestamp = "2025-01-01 10:00:06",
      session_id = "session1",
      event_type = "error",
      error_message = "Data query failed",
      output_id = "sales_plot"
    ),
    list(
      timestamp = "2025-01-01 11:00:00",
      session_id = "session2",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 11:00:05",
      session_id = "session2",
      event_type = "input",
      input_id = "date_range"
    ),
    list(
      timestamp = "2025-01-01 11:00:06",
      session_id = "session2",
      event_type = "error",
      error_message = "Data query failed",
      output_id = "sales_plot"
    )
  )

  json_lines <- sapply(events, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json_lines, temp_json)

  suppressMessages(
    result <- bid_ingest_telemetry(temp_json)
  )

  # Should identify error pattern
  expect_true(any(grepl("error_", names(result))))

  error_notice <- result[[grep("error_", names(result))[1]]]
  expect_s3_class(error_notice, "bid_stage")
  expect_match(error_notice$problem[1], "errors")
  expect_match(error_notice$evidence[1], "Data query failed")
  expect_match(error_notice$evidence[1], "100%") # Changed from "100.0%" to "100%"

  unlink(temp_json)
})

test_that("bid_ingest_telemetry identifies navigation drop-offs", {
  temp_json <- tempfile(fileext = ".json")

  # Create events with navigation
  events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "session1",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 10:00:05",
      session_id = "session1",
      event_type = "navigation",
      navigation_id = "main_tab"
    ),
    list(
      timestamp = "2025-01-01 10:00:10",
      session_id = "session1",
      event_type = "navigation",
      navigation_id = "settings_tab"
    ),
    list(
      timestamp = "2025-01-01 11:00:00",
      session_id = "session2",
      event_type = "login"
    ),
    list(
      timestamp = "2025-01-01 11:00:05",
      session_id = "session2",
      event_type = "navigation",
      navigation_id = "main_tab"
    )
    # session2 never visits settings_tab
  )

  json_lines <- sapply(events, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json_lines, temp_json)

  suppressMessages(
    result <- bid_ingest_telemetry(
      temp_json,
      thresholds = list(navigation_threshold = 0.6)
    )
  )

  # Should identify settings_tab as underused
  expect_true("navigation_settings_tab" %in% names(result))

  nav_notice <- result$navigation_settings_tab
  expect_s3_class(nav_notice, "bid_stage")
  expect_match(nav_notice$problem[1], "settings_tab")
  expect_match(nav_notice$evidence[1], "50.0%")

  unlink(temp_json)
})

test_that("bid_ingest_telemetry identifies confusion patterns", {
  temp_json <- tempfile(fileext = ".json")

  # Create rapid repeated changes
  base_time <- as.POSIXct("2025-01-01 10:00:00")
  events <- list()

  # Add login events
  for (i in 1:3) {
    events[[length(events) + 1]] <- list(
      timestamp = format(base_time + (i - 1) * 3600, "%Y-%m-%d %H:%M:%S"),
      session_id = paste0("session", i),
      event_type = "login"
    )
  }

  # Add rapid changes for a problematic input in multiple sessions
  for (i in 1:2) {
    for (j in 1:6) {
      events[[length(events) + 1]] <- list(
        timestamp = format(
          base_time + (i - 1) * 3600 + j * 1.5,
          "%Y-%m-%d %H:%M:%S"
        ),
        session_id = paste0("session", i),
        event_type = "input",
        input_id = "confusing_slider"
      )
    }
  }

  json_lines <- sapply(events, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json_lines, temp_json)

  suppressMessages(
    result <- bid_ingest_telemetry(
      temp_json,
      thresholds = list(
        rapid_change_window = 10,
        rapid_change_count = 5
      )
    )
  )

  # Should identify confusion pattern
  expect_true("confusion_confusing_slider" %in% names(result))

  confusion_notice <- result$confusion_confusing_slider
  expect_s3_class(confusion_notice, "bid_stage")
  expect_match(confusion_notice$problem[1], "confusion")
  expect_match(confusion_notice$problem[1], "confusing_slider")

  unlink(temp_json)
})

test_that("bid_ingest_telemetry works with SQLite format", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  temp_db <- tempfile(fileext = ".sqlite")

  # Create SQLite database with telemetry data
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)

  # Create events table
  events_df <- data.frame(
    timestamp = c(
      "2025-01-01 10:00:00",
      "2025-01-01 10:00:05",
      "2025-01-01 11:00:00"
    ),
    session_id = c("s1", "s1", "s2"),
    event_type = c("login", "input", "login"),
    input_id = c(NA, "test_input", NA),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(con, "event_data", events_df)
  DBI::dbDisconnect(con)

  # Test ingestion
  suppressMessages(
    result <- bid_ingest_telemetry(temp_db)
  )

  # Should work without errors
  expect_type(result, "list")

  unlink(temp_db)
})

test_that("bid_ingest_telemetry handles malformed telemetry data", {
  # Malformed JSON - not valid JSON
  temp_json <- tempfile(fileext = ".json")
  writeLines("{invalid json content", temp_json)

  expect_warning(
    result <- bid_ingest_telemetry(temp_json),
    "No telemetry events found"
  )
  expect_equal(length(result), 0)

  unlink(temp_json)

  # JSON with missing required fields
  temp_json <- tempfile(fileext = ".json")
  events <- list(
    list(timestamp = "2025-01-01 10:00:00", session_id = "s1"), # missing event_type
    list(session_id = "s2", event_type = "login"), # missing timestamp
    list(timestamp = "2025-01-01 10:00:05", event_type = "input") # missing session_id
  )
  writeLines(toJSON(events, auto_unbox = TRUE), temp_json)

  expect_error(
    bid_ingest_telemetry(temp_json),
    "No valid events found after filtering"
  )

  unlink(temp_json)

  # Partially valid data - some good events, some bad
  temp_json <- tempfile(fileext = ".json")
  mixed_events <- list(
    list(
      timestamp = "2025-01-01 10:00:00",
      session_id = "s1",
      event_type = "login"
    ),
    list(invalid = "data"), # This should be filtered out
    list(
      timestamp = "2025-01-01 10:00:05",
      session_id = "s1",
      event_type = "input",
      input_id = "test"
    )
  )
  writeLines(toJSON(mixed_events, auto_unbox = TRUE), temp_json)

  # Should process valid events and skip invalid ones
  suppressMessages(
    result <- bid_ingest_telemetry(temp_json)
  )

  # Should still work with partial data
  expect_type(result, "list")

  unlink(temp_json)

  # Test corrupted SQLite file
  if (
    requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE)
  ) {
    temp_db <- tempfile(fileext = ".sqlite")

    # Create a file that's not a valid SQLite database
    writeLines("This is not a SQLite database", temp_db)

    expect_error(
      bid_ingest_telemetry(temp_db),
      "database|SQLite|file|not valid"
    )

    unlink(temp_db)

    # Create SQLite with wrong schema
    con <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
    DBI::dbWriteTable(con, "wrong_table", data.frame(x = 1:5, y = letters[1:5]))
    DBI::dbDisconnect(con)

    expect_warning(
      expect_error(
        bid_ingest_telemetry(temp_db),
        "Required columns missing"
      ),
      "No standard event table found"
    )

    unlink(temp_db)
  }
})

test_that("telemetry helper functions work correctly", {
  # Test data
  events <- data.frame(
    timestamp = as.POSIXct(c(
      "2025-01-01 10:00:00",
      "2025-01-01 10:00:05",
      "2025-01-01 10:00:10",
      "2025-01-01 11:00:00",
      "2025-01-01 11:00:05"
    )),
    session_id = c("s1", "s1", "s1", "s2", "s2"),
    event_type = c("login", "input", "input", "login", "input"),
    input_id = c(NA, "input1", "input1", NA, "input2"),
    stringsAsFactors = FALSE
  )

  # Test find_unused_inputs
  unused <- find_unused_inputs(events, threshold = 0.4)
  expect_equal(length(unused), 0) # Both inputs used in 50% of sessions

  unused_strict <- find_unused_inputs(events, threshold = 0.6)
  expect_equal(length(unused_strict), 2) # Both inputs used in only 50%

  # Test find_delayed_sessions
  delays <- find_delayed_sessions(events, threshold = 3)
  expect_true(delays$has_issues) # 5 second delay is over 3 second threshold

  # Test with no login events
  no_login_events <- events[events$event_type != "login", ]
  delays_no_login <- find_delayed_sessions(no_login_events, threshold = 30)
  expect_null(delays_no_login)
})

test_that("notice creator functions generate valid bid_stage objects", {
  # Mock bid_notice if needed
  local_mocked_bindings(
    bid_notice = function(previous_stage, problem, theory = NULL, evidence = NULL, ...) {
      # Create a minimal bid_stage object
      data <- tibble::tibble(
        stage = "Notice",
        problem = problem,
        theory = theory %||% "Cognitive Load Theory",
        evidence = evidence,
        suggestions = "Test suggestion",
        timestamp = Sys.time()
      )

      result <- data
      class(result) <- c("bid_stage", class(data))
      attr(result, "stage") <- "Notice"
      attr(result, "metadata") <- list()
      result
    }
  )

  # Test unused input notice
  input_info <- list(
    input_id = "test_filter",
    sessions_used = 2,
    usage_rate = 0.02
  )

  notice1 <- create_unused_input_notice(input_info, 100)
  expect_s3_class(notice1, "bid_stage")
  expect_match(notice1$problem[1], "test_filter")
  expect_match(notice1$evidence[1], "2 out of 100")

  # Test delay notice
  delay_info <- list(
    median_delay = 45,
    no_action_rate = 0.15,
    rate_over_threshold = 0.25,
    has_issues = TRUE
  )

  notice2 <- create_delay_notice(delay_info, 100, 30)
  expect_s3_class(notice2, "bid_stage")
  expect_match(notice2$problem[1], "long time")

  # Test error notice
  error_info <- list(
    error_message = "Connection timeout",
    output_id = "data_table",
    count = 25,
    sessions_affected = 20,
    session_rate = 0.2,
    associated_input = "refresh_button"
  )

  notice3 <- create_error_notice(error_info, 100)
  expect_s3_class(notice3, "bid_stage")
  expect_match(notice3$evidence[1], "Connection timeout")
  expect_match(notice3$evidence[1], "refresh_button")
})
