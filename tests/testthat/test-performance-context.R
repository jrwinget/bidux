test_that("add_performance_context handles missing duration_ms gracefully", {
  # events without duration_ms should return evidence unchanged
  events <- data.frame(
    event_type = "input",
    input_id = "slider1",
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00",
      "2023-01-01 10:00:01",
      "2023-01-01 10:00:02"
    )),
    stringsAsFactors = FALSE
  )

  evidence <- "Users frequently change this input"
  result <- add_performance_context(evidence, events, NULL)

  expect_equal(result, evidence)
})

test_that("add_performance_context adds timing data when available", {
  # events with duration_ms should add performance context
  # using 120, 150, 180 ms -> avg 150ms (0.15s) -> shows as 0.1s or 0.2s
  events <- data.frame(
    event_type = "input",
    input_id = "slider1",
    duration_ms = c(120, 150, 180),
    stringsAsFactors = FALSE
  )

  evidence <- "Users frequently change this input"
  result <- add_performance_context(evidence, events, NULL)

  # should contain timing information
  expect_true(grepl("Average duration:", result))
  expect_true(grepl("p95:", result))
  # 150ms average is >= 100ms, so should show as seconds with one decimal
  expect_true(grepl("0\\.\\ds", result))
})

test_that("add_performance_context formats different time scales correctly", {
  # sub-100ms - should show milliseconds
  events_fast <- data.frame(
    event_type = "input",
    duration_ms = c(20, 30, 40),
    stringsAsFactors = FALSE
  )

  result_fast <- add_performance_context("Evidence", events_fast, NULL)
  expect_true(grepl("ms", result_fast))
  expect_true(grepl("Average duration: 30ms", result_fast))

  # 0.1s to 10s - should show one decimal
  events_medium <- data.frame(
    event_type = "input",
    duration_ms = c(1200, 1500, 1800),
    stringsAsFactors = FALSE
  )

  result_medium <- add_performance_context("Evidence", events_medium, NULL)
  expect_true(grepl("1\\.\\ds", result_medium)) # one decimal place

  # 10s+ - should show whole seconds
  events_slow <- data.frame(
    event_type = "input",
    duration_ms = c(12000, 15000, 18000),
    stringsAsFactors = FALSE
  )

  result_slow <- add_performance_context("Evidence", events_slow, NULL)
  expect_true(grepl("15s", result_slow)) # whole seconds
  expect_false(grepl("15\\.0s", result_slow)) # no decimals
})

test_that("add_performance_context requires minimum sample size", {
  # less than 3 measurements should return evidence unchanged
  events <- data.frame(
    event_type = "input",
    duration_ms = c(120, 150),
    stringsAsFactors = FALSE
  )

  evidence <- "Users frequently change this input"
  result <- add_performance_context(evidence, events, NULL)

  expect_equal(result, evidence)
})

test_that("add_performance_context handles NA and infinite values", {
  # events with NA or infinite duration_ms should be filtered out
  events <- data.frame(
    event_type = "input",
    duration_ms = c(120, NA, 150, Inf, 180, -Inf),
    stringsAsFactors = FALSE
  )

  evidence <- "Users frequently change this input"
  result <- add_performance_context(evidence, events, NULL)

  # should only use the 3 valid values (120, 150, 180)
  expect_true(grepl("Average duration:", result))
})

test_that("add_performance_context applies event filters correctly", {
  # events with multiple inputs - filter should select specific one
  events <- data.frame(
    event_type = c("input", "input", "input", "input", "input", "input"),
    input_id = c("slider1", "slider1", "slider1", "slider2", "slider2", "slider2"),
    duration_ms = c(50, 60, 70, 500, 550, 600),
    stringsAsFactors = FALSE
  )

  evidence <- "Slider1 is fast"
  event_filter <- events$input_id == "slider1"
  result <- add_performance_context(evidence, events, event_filter)

  # should only use slider1 durations (50, 60, 70) - average 60ms (sub-100ms)
  # should not use slider2 durations (500, 550, 600)
  expect_true(grepl("60ms", result))
  expect_false(grepl("5\\d\\dms", result)) # should not contain 500+ ms values

  # also test with second input
  evidence2 <- "Slider2 is slow"
  event_filter2 <- events$input_id == "slider2"
  result2 <- add_performance_context(evidence2, events, event_filter2)

  # should use slider2 durations (500, 550, 600) - average 550ms (0.5s or 0.6s)
  expect_true(grepl("0\\.\\ds", result2))
  expect_false(grepl("60ms", result2)) # should not contain slider1 values
})

test_that("friction detection works with OTEL-like duration fields", {
  # create events with duration_ms like OTEL would add
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("input", "input", "input", "login", "input"),
    input_id = c("btn1", "btn2", "btn1", NA, "btn3"),
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00", "2023-01-01 10:01:00",
      "2023-01-01 10:02:00", "2023-01-01 10:03:00",
      "2023-01-01 10:04:00"
    )),
    duration_ms = c(120, 150, 130, 200, 140),
    stringsAsFactors = FALSE
  )

  # test unused inputs detection still works
  result <- find_unused_inputs(events, threshold = 0.5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)
  expect_true(any(sapply(result, function(x) x$input_id == "btn3")))

  # test that duration_ms field doesn't break the analysis
  expect_true("duration_ms" %in% names(events))
})

test_that("confusion patterns work with duration data", {
  # simulate rapid input changes with timing data
  base_time <- as.POSIXct("2023-01-01 10:00:00")
  events <- data.frame(
    session_id = c(rep("s1", 6), rep("s2", 6)),
    event_type = c(rep("input", 12)),
    input_id = c(rep("confused_input", 12)),
    timestamp = c(base_time + c(0, 1, 2, 3, 4, 5), base_time + c(10, 11, 12, 13, 14, 15)),
    duration_ms = c(1200, 1300, 1400, 1500, 1600, 1700, 1100, 1200, 1300, 1400, 1500, 1600),
    stringsAsFactors = FALSE
  )

  result <- find_confusion_patterns(events, window_seconds = 10, min_changes = 5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)
  confused_input_found <- any(sapply(result, function(x) x$input_id == "confused_input"))
  expect_true(confused_input_found)
})

test_that("error patterns work with duration data", {
  # simulate errors with timing data
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3", "s3"),
    event_type = c("input", "error", "input", "error", "error", "login"),
    error_message = c(NA, "timeout", NA, "timeout", "connection", NA),
    output_id = c(NA, "plot1", NA, "plot1", "plot2", NA),
    input_id = c("btn1", NA, "btn1", NA, NA, NA),
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:03",
      "2023-01-01 10:01:00", "2023-01-01 10:01:03",
      "2023-01-01 10:02:00", "2023-01-01 10:02:03"
    )),
    duration_ms = c(NA, 2500, NA, 2800, 3000, NA),
    stringsAsFactors = FALSE
  )

  result <- find_error_patterns(events, threshold_rate = 0.1)

  expect_true(is.list(result))
  expect_true(length(result) > 0)

  # should find timeout error pattern
  timeout_pattern <- result[[which(sapply(result, function(x) x$error_message == "timeout"))]]
  expect_equal(timeout_pattern$count, 2)
  expect_equal(timeout_pattern$sessions_affected, 2)
})

test_that("delayed sessions work with duration data", {
  # simulate login events with timing data
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("login", "input", "login", "navigation", "login"),
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:05",
      "2023-01-01 10:01:00", "2023-01-01 10:01:35",
      "2023-01-01 10:02:00"
    )),
    input_id = c(NA, "btn1", NA, NA, NA),
    navigation_id = c(NA, NA, NA, "page1", NA),
    duration_ms = c(50, NA, 55, NA, 60),
    stringsAsFactors = FALSE
  )

  result <- find_delayed_sessions(events, threshold_seconds = 30)

  expect_true(is.list(result))
  expect_true("total_sessions" %in% names(result))
  expect_true("median_delay" %in% names(result))
  expect_equal(result$total_sessions, 3)
  expect_equal(result$no_action_sessions, 1) # s3 has no actions
})

test_that("navigation dropoffs work with duration data", {
  # simulate navigation events with timing data
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3", "s4", "s5"),
    event_type = c(
      "navigation", "navigation", "navigation", "navigation",
      "navigation", "navigation", "login"
    ),
    navigation_id = c(
      "home", "rare_page", "home", "popular_page",
      "rare_page", "popular_page", NA
    ),
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00", "2023-01-01 10:00:30",
      "2023-01-01 10:01:00", "2023-01-01 10:01:30",
      "2023-01-01 10:02:00", "2023-01-01 10:02:30",
      "2023-01-01 10:03:00"
    )),
    duration_ms = c(100, 150, 110, 120, 140, 130, NA),
    stringsAsFactors = FALSE
  )

  result <- find_navigation_dropoffs(events, threshold = 0.5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)

  # rare_page should be flagged (2/5 sessions = 40% < 50% threshold)
  rare_page_found <- any(sapply(result, function(x) x$page == "rare_page"))
  expect_true(rare_page_found)
})

test_that("notice creation includes performance context when events provided", {
  # create full events with timing data
  events <- data.frame(
    session_id = c("s1", "s2", "s3", "s4", "s5"),
    event_type = rep("input", 5),
    input_id = rep("slow_slider", 5),
    timestamp = as.POSIXct(paste("2023-01-01 10:0", 0:4, ":00", sep = "")),
    duration_ms = c(1200, 1500, 1800, 2000, 1600),
    stringsAsFactors = FALSE
  )

  # test unused input notice creation
  input_info <- list(
    input_id = "slow_slider",
    sessions_used = 2,
    usage_rate = 0.4
  )

  # suppress warnings about deprecated data_story format
  suppressWarnings({
    notice <- create_unused_input_notice(input_info, 5, events)
  })

  # extract evidence from notice
  evidence <- if (is.data.frame(notice) && "evidence" %in% names(notice)) {
    notice$evidence[1]
  } else {
    NA_character_
  }

  # should contain performance context since sessions_used > 0 and duration_ms available
  expect_true(!is.na(evidence))
  # performance metrics should be added
  expect_true(grepl("Average duration:|p95:", evidence))
})

test_that("backward compatibility - notice creation works without events", {
  # create notice without events parameter (backward compatible)
  input_info <- list(
    input_id = "btn3",
    sessions_used = 1,
    usage_rate = 0.33
  )

  # suppress warnings about deprecated data_story format
  suppressWarnings({
    notice <- create_unused_input_notice(input_info, 3, NULL)
  })

  expect_true(inherits(notice, "bid_stage"))

  # extract evidence
  evidence <- if (is.data.frame(notice) && "evidence" %in% names(notice)) {
    notice$evidence[1]
  } else {
    NA_character_
  }

  # should NOT contain performance context when events is NULL
  expect_true(!is.na(evidence))
  expect_false(grepl("Average duration:", evidence))
})

test_that("backward compatibility - algorithms work without duration_ms", {
  # events without duration_ms should work exactly as before
  events_legacy <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("input", "input", "input", "login", "input"),
    input_id = c("btn1", "btn2", "btn1", NA, "btn3"),
    timestamp = as.POSIXct(c(
      "2023-01-01 10:00:00", "2023-01-01 10:01:00",
      "2023-01-01 10:02:00", "2023-01-01 10:03:00",
      "2023-01-01 10:04:00"
    )),
    stringsAsFactors = FALSE
  )

  # test all friction detection algorithms work without duration_ms
  unused <- find_unused_inputs(events_legacy, threshold = 0.5)
  expect_true(is.list(unused))

  delays <- find_delayed_sessions(events_legacy, threshold_seconds = 30)
  expect_true(is.null(delays) || is.list(delays))

  errors <- find_error_patterns(events_legacy, threshold_rate = 0.1)
  expect_true(is.list(errors))

  confusion <- find_confusion_patterns(events_legacy)
  expect_true(is.list(confusion))
})
