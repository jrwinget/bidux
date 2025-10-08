# ==============================================================================
# TELEMETRY UTILITIES TESTS
# ==============================================================================

test_that("get_total_sessions counts unique sessions correctly", {
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s3", "s2"),
    event_type = c("click", "input", "click", "input", "navigation"),
    stringsAsFactors = FALSE
  )

  result <- bidux:::get_total_sessions(events)

  expect_equal(result, 3L)
  expect_true(is.integer(result))
})

test_that("get_total_sessions handles empty data frame", {
  empty_df <- data.frame(session_id = character(0))

  result <- bidux:::get_total_sessions(empty_df)

  expect_equal(result, 0L)
})

test_that("get_total_sessions handles missing session_id column", {
  bad_df <- data.frame(
    timestamp = "2023-01-01",
    event_type = "click"
  )

  result <- bidux:::get_total_sessions(bad_df)

  expect_equal(result, 0L)
})

test_that("get_total_sessions handles NULL input", {
  result <- bidux:::get_total_sessions(NULL)

  expect_equal(result, 0L)
})

test_that("na_safe_equal handles NA values correctly", {
  # both NA
  expect_true(bidux:::na_safe_equal(NA, NA))
  expect_true(bidux:::na_safe_equal(NA_character_, NA_character_))
  expect_true(bidux:::na_safe_equal(NA_real_, NA_real_))

  # one NA, one not
  expect_false(bidux:::na_safe_equal(NA, 5))
  expect_false(bidux:::na_safe_equal("test", NA))

  # both non-NA and equal
  expect_true(bidux:::na_safe_equal(5, 5))
  expect_true(bidux:::na_safe_equal("test", "test"))

  # both non-NA and not equal
  expect_false(bidux:::na_safe_equal(5, 10))
  expect_false(bidux:::na_safe_equal("test", "other"))
})

test_that("na_safe_equal handles vectors", {
  result <- bidux:::na_safe_equal(c(1, NA, 3), c(1, NA, 4))

  expect_equal(length(result), 3)
  expect_equal(result, c(TRUE, TRUE, FALSE))
})

test_that("calculate_severity returns correct levels", {
  # critical (>= 0.3)
  expect_equal(bidux:::calculate_severity(0.35), "critical")
  expect_equal(bidux:::calculate_severity(0.30), "critical")

  # high (>= 0.1, < 0.3)
  expect_equal(bidux:::calculate_severity(0.25), "high")
  expect_equal(bidux:::calculate_severity(0.10), "high")

  # medium (>= 0.05, < 0.1)
  expect_equal(bidux:::calculate_severity(0.08), "medium")
  expect_equal(bidux:::calculate_severity(0.05), "medium")

  # low (< 0.05)
  expect_equal(bidux:::calculate_severity(0.04), "low")
  expect_equal(bidux:::calculate_severity(0.01), "low")
  expect_equal(bidux:::calculate_severity(0.00), "low")
})

test_that("calculate_severity handles custom thresholds", {
  # custom thresholds
  result <- bidux:::calculate_severity(
    0.15,
    critical_threshold = 0.20,
    high_threshold = 0.10,
    medium_threshold = 0.05
  )

  expect_equal(result, "high")

  # very strict thresholds
  result_strict <- bidux:::calculate_severity(
    0.02,
    critical_threshold = 0.03,
    high_threshold = 0.02,
    medium_threshold = 0.01
  )

  expect_equal(result_strict, "high")
})

test_that("calculate_severity handles edge cases", {
  # exactly at thresholds
  expect_equal(bidux:::calculate_severity(0.30), "critical")
  expect_equal(bidux:::calculate_severity(0.10), "high")
  expect_equal(bidux:::calculate_severity(0.05), "medium")

  # just below thresholds
  expect_equal(bidux:::calculate_severity(0.29999), "high")
  expect_equal(bidux:::calculate_severity(0.09999), "medium")
  expect_equal(bidux:::calculate_severity(0.04999), "low")

  # extreme values
  expect_equal(bidux:::calculate_severity(1.0), "critical")
  expect_equal(bidux:::calculate_severity(0.0), "low")
})

test_that("calculate_session_rates calculates rates correctly", {
  session_counts <- 25
  total_sessions <- 100

  result <- bidux:::calculate_session_rates(session_counts, total_sessions)

  expect_equal(result, 0.25)
  expect_true(is.numeric(result))
})

test_that("calculate_session_rates handles zero total_sessions", {
  result <- bidux:::calculate_session_rates(10, 0)

  expect_equal(result, 0.0)
  # should not error or produce NaN/Inf
  expect_false(is.nan(result))
  expect_false(is.infinite(result))
})

test_that("calculate_session_rates handles vectors", {
  session_counts <- c(10, 20, 5)
  total_sessions <- 100

  result <- bidux:::calculate_session_rates(session_counts, total_sessions)

  expect_equal(length(result), 3)
  expect_equal(result, c(0.10, 0.20, 0.05))
})

test_that("calculate_session_rates handles edge cases", {
  # all sessions
  expect_equal(bidux:::calculate_session_rates(100, 100), 1.0)

  # no sessions
  expect_equal(bidux:::calculate_session_rates(0, 100), 0.0)

  # more counts than total (shouldn't happen but handle gracefully)
  result_overflow <- bidux:::calculate_session_rates(150, 100)
  expect_true(result_overflow >= 1.0)
})

test_that("calculate_session_rates validates input types", {
  # non-numeric inputs should error
  expect_error(
    bidux:::calculate_session_rates(NA, 100),
    "must be numeric"
  )

  expect_error(
    bidux:::calculate_session_rates(10, NA),
    "must be numeric"
  )

  expect_error(
    bidux:::calculate_session_rates("10", 100),
    "must be numeric"
  )

  expect_error(
    bidux:::calculate_session_rates(10, c(100, 200)),
    "must be a single"
  )

  expect_error(
    bidux:::calculate_session_rates(10, -5),
    "non-negative"
  )
})

test_that("telemetry utils work together in realistic workflow", {
  # simulate realistic telemetry data
  events <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3", "s4", "s4"),
    event_type = c("login", "click", "login", "error", "login", "login", "click"),
    input_id = c(NA, "btn1", NA, NA, NA, NA, "btn2"),
    timestamp = seq(as.POSIXct("2023-01-01 10:00:00"), by = "5 min", length.out = 7),
    stringsAsFactors = FALSE
  )

  # get total sessions
  total <- bidux:::get_total_sessions(events)
  expect_equal(total, 4L)

  # calculate error rate
  error_sessions <- length(unique(events$session_id[events$event_type == "error"]))
  error_rate <- bidux:::calculate_session_rates(error_sessions, total)
  expect_equal(error_rate, 0.25) # 1 of 4 sessions had errors

  # determine severity
  severity <- bidux:::calculate_severity(error_rate)
  expect_equal(severity, "high") # 0.25 is high severity

  # use NA-safe comparison for filtering
  click_events <- events[bidux:::na_safe_equal(events$event_type, "click"), ]
  expect_equal(nrow(click_events), 2)
})

test_that("calculate_severity vectorization behavior", {
  # test with vector input
  impact_rates <- c(0.40, 0.20, 0.08, 0.02)

  result <- sapply(impact_rates, bidux:::calculate_severity)

  expect_equal(length(result), 4)
  expect_equal(result, c("critical", "high", "medium", "low"))
})
