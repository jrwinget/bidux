# tests for the entry-anchoring pattern detector and its notice constructor.
# covers: detector per-session fixation computation, qualifying-session
# gate (min_navs), aggregate firing threshold, notice concept routing to
# Anchoring Effect, and defensive edge cases.

# ==============================================================================
# HELPERS
# ==============================================================================

# build nav-only events: for each (session_id, nav_id_sequence), emit one
# navigation event per id in sequence.
make_nav_events <- function(sessions, anchor = as.POSIXct("2026-01-01 12:00:00", tz = "UTC")) {
  rows <- list()
  for (s in sessions) {
    for (i in seq_along(s$nav_ids)) {
      rows[[length(rows) + 1L]] <- data.frame(
        timestamp = anchor + i,
        session_id = s$session_id,
        event_type = "navigation",
        navigation_id = s$nav_ids[[i]],
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# ==============================================================================
# find_entry_anchoring()
# ==============================================================================

test_that("fires when a high share of qualifying sessions anchor on the entry", {
  # 7 sessions of 10 have all 3 nav events on the same page; 3 explore.
  # anchored rate = 7/10 = 0.7, far above default 0.3 threshold.
  sessions <- c(
    lapply(seq_len(7L), function(i) {
      list(session_id = paste0("anchor", i), nav_ids = c("home", "home", "home"))
    }),
    lapply(seq_len(3L), function(i) {
      list(session_id = paste0("explorer", i), nav_ids = c("home", "about", "contact"))
    })
  )
  events <- make_nav_events(sessions)
  res <- find_entry_anchoring(events, min_navs = 3L, rate_threshold = 0.3)
  expect_true(res$has_issues)
  expect_equal(res$qualifying_sessions, 10L)
  expect_equal(res$anchored_count, 7L)
  expect_equal(res$anchored_rate, 0.7)
})

test_that("does not fire when few sessions anchor", {
  sessions <- c(
    lapply(seq_len(1L), function(i) {
      list(session_id = paste0("anchor", i), nav_ids = c("home", "home", "home"))
    }),
    lapply(seq_len(9L), function(i) {
      list(session_id = paste0("explorer", i), nav_ids = c("home", "about", "contact"))
    })
  )
  events <- make_nav_events(sessions)
  res <- find_entry_anchoring(events, min_navs = 3L, rate_threshold = 0.3)
  expect_false(res$has_issues)
  expect_equal(res$anchored_rate, 0.1)
})

test_that("per-session fixation threshold is 0.7 (anchored iff >=70% on first page)", {
  # session with 3 events on "home" and 1 on "elsewhere" -> 75% on first page.
  # should be flagged anchored.
  sessions <- list(
    list(
      session_id = "borderline",
      nav_ids = c("home", "home", "home", "elsewhere")
    ),
    list(
      session_id = "just_below",
      # 2 on "home" and 2 on "elsewhere" = 50% -> not anchored
      nav_ids = c("home", "home", "elsewhere", "elsewhere")
    )
  )
  events <- make_nav_events(sessions)
  res <- find_entry_anchoring(events, min_navs = 3L, rate_threshold = 0.1)
  expect_equal(res$qualifying_sessions, 2L)
  expect_equal(res$anchored_count, 1L)
})

test_that("sessions below min_navs are not counted in the denominator", {
  sessions <- list(
    list(session_id = "short1", nav_ids = c("home", "home")),
    list(session_id = "short2", nav_ids = c("home")),
    list(session_id = "long_anchored", nav_ids = c("home", "home", "home"))
  )
  events <- make_nav_events(sessions)
  res <- find_entry_anchoring(events, min_navs = 3L, rate_threshold = 0.5)
  expect_equal(res$qualifying_sessions, 1L)
  expect_equal(res$anchored_count, 1L)
  expect_equal(res$anchored_rate, 1.0)
})

test_that("returns NULL when no sessions meet min_navs", {
  sessions <- list(
    list(session_id = "short", nav_ids = c("home", "home"))
  )
  events <- make_nav_events(sessions)
  expect_null(find_entry_anchoring(events, min_navs = 3L))
})

test_that("returns NULL on events with no navigation rows", {
  events <- data.frame(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    session_id = "s1",
    event_type = "login",
    navigation_id = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_null(find_entry_anchoring(events))
})

test_that("returns NULL on empty events", {
  empty <- make_nav_events(list(
    list(session_id = "s", nav_ids = c("a"))
  ))[0, ]
  expect_null(find_entry_anchoring(empty))
})

test_that("drops nav rows with NA navigation_id before aggregation", {
  sessions <- list(
    list(session_id = "s1", nav_ids = c("home", "home", "home"))
  )
  events <- make_nav_events(sessions)
  # inject an NA navigation_id row
  junk <- events[1L, ]
  junk$navigation_id <- NA_character_
  res <- find_entry_anchoring(rbind(events, junk), min_navs = 3L, rate_threshold = 0.5)
  expect_equal(res$qualifying_sessions, 1L)
})

test_that("accepts character timestamps and coerces to POSIXct", {
  sessions <- list(
    list(session_id = "s1", nav_ids = c("home", "home", "home"))
  )
  events <- make_nav_events(sessions)
  events$timestamp <- as.character(events$timestamp)
  res <- find_entry_anchoring(events, min_navs = 3L, rate_threshold = 0.5)
  expect_equal(res$qualifying_sessions, 1L)
})

# ==============================================================================
# create_entry_anchoring_notice()
# ==============================================================================

test_that("notice resolves to Anchoring Effect via the signal matcher", {
  info <- list(
    has_issues = TRUE,
    anchored_rate = 0.5,
    anchored_count = 5L,
    qualifying_sessions = 10L,
    min_navs = 3L,
    fixation_threshold = 0.7
  )
  notice <- create_entry_anchoring_notice(info)
  expect_s3_class(notice, "bid_stage")
  expect_identical(notice$theory[1L], "Anchoring Effect")
})

test_that("notice evidence names counts and the fixation percentage", {
  info <- list(
    has_issues = TRUE,
    anchored_rate = 0.5,
    anchored_count = 5L,
    qualifying_sessions = 10L,
    min_navs = 3L,
    fixation_threshold = 0.7
  )
  notice <- create_entry_anchoring_notice(info)
  expect_match(notice$evidence[1L], "5 of 10")
  expect_match(notice$evidence[1L], "50.0%")
  expect_match(notice$evidence[1L], "70%")
})
