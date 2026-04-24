# tests for the peak-end pattern detector and its notice constructor.
# covers: detector aggregation logic, window arithmetic, negative-end
# classification, threshold gating, notice concept routing, and a couple
# of defensive edge cases (empty events, NA timestamps).

# ==============================================================================
# HELPERS
# ==============================================================================

# build a controlled events data frame from a list of session specs.
# each spec: list(session_id, events = list(list(offset_secs, event_type,
# error_message)))
# offset_secs is the offset from a shared anchor; higher offset = later.
make_peak_end_events <- function(sessions, anchor = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")) {
  rows <- list()
  for (s in sessions) {
    for (ev in s$events) {
      rows[[length(rows) + 1L]] <- data.frame(
        timestamp = anchor + ev$offset_secs,
        session_id = s$session_id,
        event_type = ev$event_type,
        error_message = ev$error_message %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# build N sessions that each end with an error in the last `window_secs`.
negative_sessions <- function(n, window_secs = 60L, prefix = "neg") {
  lapply(seq_len(n), function(i) {
    list(
      session_id = paste0(prefix, "_", i),
      events = list(
        list(offset_secs = 0, event_type = "login"),
        list(offset_secs = 100, event_type = "input"),
        list(offset_secs = 100 + window_secs - 5, event_type = "error",
             error_message = "oops")
      )
    )
  })
}

# build N "happy" sessions that end on a non-error event.
happy_sessions <- function(n, prefix = "pos") {
  lapply(seq_len(n), function(i) {
    list(
      session_id = paste0(prefix, "_", i),
      events = list(
        list(offset_secs = 0, event_type = "login"),
        list(offset_secs = 50, event_type = "input"),
        list(offset_secs = 120, event_type = "input")
      )
    )
  })
}

# ==============================================================================
# find_peak_end_patterns()
# ==============================================================================

test_that("fires when enough sessions end with an error in the final window", {
  # 3 of 10 sessions end negatively; threshold 0.2 -> fires.
  events <- make_peak_end_events(c(
    negative_sessions(3L),
    happy_sessions(7L)
  ))
  res <- find_peak_end_patterns(events, window_secs = 60L, negative_rate_threshold = 0.2)
  expect_true(res$has_issues)
  expect_equal(res$total_sessions, 10L)
  expect_equal(res$negative_end_count, 3L)
  expect_equal(res$negative_end_rate, 0.3)
})

test_that("does not fire when negative rate is below threshold", {
  events <- make_peak_end_events(c(
    negative_sessions(1L),
    happy_sessions(9L)
  ))
  res <- find_peak_end_patterns(events, window_secs = 60L, negative_rate_threshold = 0.2)
  expect_false(res$has_issues)
  expect_equal(res$negative_end_rate, 0.1)
})

test_that("a session where the last event is an error counts as negative even outside the window", {
  # last event is 1000s after the previous activity (far outside any
  # 60s window), but it IS the last event -> negative by the
  # "last event is error" rule.
  events <- make_peak_end_events(list(
    list(
      session_id = "late_error",
      events = list(
        list(offset_secs = 0, event_type = "login"),
        list(offset_secs = 10, event_type = "input"),
        list(offset_secs = 1000, event_type = "error", error_message = "late")
      )
    )
  ))
  res <- find_peak_end_patterns(events, window_secs = 60L, negative_rate_threshold = 0.5)
  expect_equal(res$negative_end_count, 1L)
})

test_that("errors outside the final window and before a later non-error are not flagged", {
  events <- make_peak_end_events(list(
    list(
      session_id = "recovered",
      events = list(
        list(offset_secs = 0, event_type = "login"),
        list(offset_secs = 10, event_type = "error", error_message = "early"),
        list(offset_secs = 300, event_type = "input")
      )
    )
  ))
  res <- find_peak_end_patterns(events, window_secs = 60L, negative_rate_threshold = 0.5)
  expect_equal(res$negative_end_count, 0L)
})

test_that("returns NULL on empty events", {
  empty <- make_peak_end_events(list(
    list(session_id = "s", events = list(list(offset_secs = 0, event_type = "login")))
  ))[0, ]
  expect_null(find_peak_end_patterns(empty))
})

test_that("drops rows with NA session_id or timestamp before aggregation", {
  events <- make_peak_end_events(negative_sessions(2L))
  # append a junk row
  junk <- events[1L, ]
  junk$session_id <- NA_character_
  res <- find_peak_end_patterns(rbind(events, junk), negative_rate_threshold = 0.5)
  # still 2 sessions, both negative
  expect_equal(res$total_sessions, 2L)
  expect_equal(res$negative_end_count, 2L)
})

test_that("accepts character timestamps and coerces to POSIXct", {
  events <- make_peak_end_events(negative_sessions(2L))
  events$timestamp <- as.character(events$timestamp)
  res <- find_peak_end_patterns(events, negative_rate_threshold = 0.5)
  expect_equal(res$negative_end_count, 2L)
})

# ==============================================================================
# create_peak_end_notice()
# ==============================================================================

test_that("notice resolves to Peak-End Rule via the signal matcher", {
  info <- list(
    has_issues = TRUE,
    negative_end_rate = 0.35,
    negative_end_count = 7L,
    total_sessions = 20L,
    window_secs = 60
  )
  notice <- create_peak_end_notice(info)
  expect_s3_class(notice, "bid_stage")
  expect_identical(notice$theory[1L], "Peak-End Rule")
})

test_that("notice evidence reports the negative-end rate clearly", {
  info <- list(
    has_issues = TRUE,
    negative_end_rate = 0.25,
    negative_end_count = 5L,
    total_sessions = 20L,
    window_secs = 60
  )
  notice <- create_peak_end_notice(info)
  expect_match(notice$evidence[1L], "5 of 20")
  expect_match(notice$evidence[1L], "25.0%")
  expect_match(notice$evidence[1L], "60 seconds")
})
