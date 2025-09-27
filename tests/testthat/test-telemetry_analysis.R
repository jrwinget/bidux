test_that("find_unused_inputs identifies unused inputs correctly", {
  # Create sample events
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("input", "input", "input", "login", "input"),
    input_id = c("btn1", "btn2", "btn1", NA, "btn3"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00",
                            "2023-01-01 10:02:00", "2023-01-01 10:03:00",
                            "2023-01-01 10:04:00")),
    stringsAsFactors = FALSE
  )

  # Test with 50% threshold (btn3 should be unused)
  result <- find_unused_inputs(events, threshold = 0.5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)
  expect_true(any(sapply(result, function(x) x$input_id == "btn3")))

  # Test with 0% threshold (no inputs should be unused)
  result_none <- find_unused_inputs(events, threshold = 0)
  expect_equal(length(result_none), 0)
})

test_that("find_unused_inputs handles edge cases", {
  # Empty events
  empty_events <- data.frame(
    session_id = character(0),
    event_type = character(0),
    input_id = character(0),
    timestamp = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )

  result_empty <- find_unused_inputs(empty_events)
  expect_equal(length(result_empty), 0)

  # No input events
  no_input_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("login", "navigation"),
    input_id = c(NA, NA),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_no_input <- find_unused_inputs(no_input_events)
  expect_equal(length(result_no_input), 0)
})

test_that("find_delayed_sessions calculates delays correctly", {
  # Create events with login and actions
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("login", "input", "login", "navigation", "login"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:00:05",
                            "2023-01-01 10:01:00", "2023-01-01 10:01:35",
                            "2023-01-01 10:02:00")),
    input_id = c(NA, "btn1", NA, NA, NA),
    navigation_id = c(NA, NA, NA, "page1", NA),
    stringsAsFactors = FALSE
  )

  result <- find_delayed_sessions(events, threshold_seconds = 30)

  expect_true(is.list(result))
  expect_true("total_sessions" %in% names(result))
  expect_true("median_delay" %in% names(result))
  expect_true("has_issues" %in% names(result))
  expect_equal(result$total_sessions, 3)
  expect_equal(result$no_action_sessions, 1) # s3 has no actions
})

test_that("find_delayed_sessions handles edge cases", {
  # No login events
  no_login_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("input", "navigation"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_no_login <- find_delayed_sessions(no_login_events)
  expect_null(result_no_login)

  # Only login events (no actions)
  only_login_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("login", "login"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_only_login <- find_delayed_sessions(only_login_events)
  expect_equal(result_only_login$no_action_sessions, 2)
  expect_true(result_only_login$has_issues)
})

test_that("find_error_patterns identifies error patterns", {
  # Create events with errors
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3", "s3"),
    event_type = c("input", "error", "input", "error", "error", "login"),
    error_message = c(NA, "timeout", NA, "timeout", "connection", NA),
    output_id = c(NA, "plot1", NA, "plot1", "plot2", NA),
    input_id = c("btn1", NA, "btn1", NA, NA, NA),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:00:03",
                            "2023-01-01 10:01:00", "2023-01-01 10:01:03",
                            "2023-01-01 10:02:00", "2023-01-01 10:02:03")),
    stringsAsFactors = FALSE
  )

  result <- find_error_patterns(events, threshold_rate = 0.1)

  expect_true(is.list(result))
  expect_true(length(result) > 0)

  # Should find timeout error pattern
  timeout_pattern <- result[[which(sapply(result, function(x) x$error_message == "timeout"))]]
  expect_equal(timeout_pattern$count, 2)
  expect_equal(timeout_pattern$sessions_affected, 2)
  expect_equal(timeout_pattern$associated_input, "btn1")
})

test_that("find_error_patterns handles edge cases", {
  # No error events
  no_error_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("input", "navigation"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_no_errors <- find_error_patterns(no_error_events)
  expect_equal(length(result_no_errors), 0)

  # Errors below threshold
  low_error_events <- data.frame(
    session_id = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10"),
    event_type = c("error", rep("login", 9)),
    error_message = c("rare_error", rep(NA, 9)),
    output_id = c("plot1", rep(NA, 9)),
    timestamp = as.POSIXct(paste("2023-01-01 10:0", 0:9, ":00", sep = "")),
    stringsAsFactors = FALSE
  )

  result_low_errors <- find_error_patterns(low_error_events, threshold_rate = 0.2)
  expect_equal(length(result_low_errors), 0)
})

test_that("find_navigation_dropoffs identifies underused pages", {
  # Create navigation events
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3", "s4", "s5"),
    event_type = c("navigation", "navigation", "navigation", "navigation",
                   "navigation", "navigation", "login"),
    navigation_id = c("home", "rare_page", "home", "popular_page",
                      "rare_page", "popular_page", NA),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:00:30",
                            "2023-01-01 10:01:00", "2023-01-01 10:01:30",
                            "2023-01-01 10:02:00", "2023-01-01 10:02:30",
                            "2023-01-01 10:03:00")),
    stringsAsFactors = FALSE
  )

  result <- find_navigation_dropoffs(events, threshold = 0.5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)

  # rare_page should be flagged (2/5 sessions = 40% < 50% threshold)
  rare_page_found <- any(sapply(result, function(x) x$page == "rare_page"))
  expect_true(rare_page_found)
})

test_that("find_navigation_dropoffs handles edge cases", {
  # No navigation events
  no_nav_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("input", "login"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_no_nav <- find_navigation_dropoffs(no_nav_events)
  expect_equal(length(result_no_nav), 0)

  # All pages above threshold
  high_usage_events <- data.frame(
    session_id = c("s1", "s2", "s3", "s4"),
    event_type = rep("navigation", 4),
    navigation_id = c("page1", "page1", "page1", "page1"),
    timestamp = as.POSIXct(paste("2023-01-01 10:0", 0:3, ":00", sep = "")),
    stringsAsFactors = FALSE
  )

  result_high_usage <- find_navigation_dropoffs(high_usage_events, threshold = 0.5)
  expect_equal(length(result_high_usage), 0)
})

test_that("find_confusion_patterns identifies rapid input changes", {
  # Create events with rapid input changes in multiple sessions
  base_time <- as.POSIXct("2023-01-01 10:00:00")
  events <- data.frame(
    session_id = c(rep("s1", 6), rep("s2", 6)),
    event_type = c(rep("input", 12)),
    input_id = c(rep("confused_input", 12)),
    timestamp = c(base_time + c(0, 1, 2, 3, 4, 5), base_time + c(10, 11, 12, 13, 14, 15)),
    stringsAsFactors = FALSE
  )

  result <- find_confusion_patterns(events, window_seconds = 10, min_changes = 5)

  expect_true(is.list(result))
  expect_true(length(result) > 0)

  confused_input_found <- any(sapply(result, function(x) x$input_id == "confused_input"))
  expect_true(confused_input_found)
})

test_that("find_confusion_patterns handles edge cases", {
  # No input events
  no_input_events <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("login", "navigation"),
    timestamp = as.POSIXct(c("2023-01-01 10:00:00", "2023-01-01 10:01:00")),
    stringsAsFactors = FALSE
  )

  result_no_input <- find_confusion_patterns(no_input_events)
  expect_equal(length(result_no_input), 0)

  # Changes too slow
  base_time <- as.POSIXct("2023-01-01 10:00:00")
  slow_changes <- data.frame(
    session_id = rep("s1", 5),
    event_type = rep("input", 5),
    input_id = rep("slow_input", 5),
    timestamp = base_time + c(0, 30, 60, 90, 120), # 30 seconds apart
    stringsAsFactors = FALSE
  )

  result_slow <- find_confusion_patterns(slow_changes, window_seconds = 10, min_changes = 5)
  expect_equal(length(result_slow), 0)

  # Not enough systematic patterns (only one session)
  single_session <- data.frame(
    session_id = rep("s1", 5),
    event_type = rep("input", 5),
    input_id = rep("single_input", 5),
    timestamp = base_time + c(0, 1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  result_single <- find_confusion_patterns(single_session, window_seconds = 10, min_changes = 5)
  expect_equal(length(result_single), 0)
})