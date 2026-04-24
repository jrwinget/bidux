# tests for unused-input subtyping introduced in the signal-routing work.
# covers: classify_unused_input_subtype() decisions across telemetry shapes,
# find_unused_inputs() emitting subtype + features, and
# create_unused_input_notice() routing to the correct theory via the
# signal matcher.

# ==============================================================================
# HELPERS
# ==============================================================================

# build a small events data frame from a list of (session_id, input_id,
# value, n_touches) tuples, plus an optional vector of session ids that
# logged in but did not touch the input. login rows are always added so
# get_total_sessions() picks them up.
make_unused_input_events <- function(touches, idle_sessions = character()) {
  rows <- lapply(touches, function(t) {
    data.frame(
      timestamp = Sys.time() + seq_len(t$n_touches),
      session_id = t$session_id,
      event_type = "input",
      input_id = t$input_id,
      value = as.character(t$value %||% NA),
      stringsAsFactors = FALSE
    )
  })
  events <- do.call(rbind, rows)

  all_sessions <- unique(c(events$session_id, idle_sessions))
  login_rows <- data.frame(
    timestamp = Sys.time(),
    session_id = all_sessions,
    event_type = "login",
    input_id = NA_character_,
    value = NA_character_,
    stringsAsFactors = FALSE
  )
  rbind(login_rows, events)
}

# ==============================================================================
# classify_unused_input_subtype()
# ==============================================================================

test_that("default_kept fires when all touching sessions land on a single value", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "default", n_touches = 1)
    ),
    idle_sessions = paste0("s", 2:40)
  )
  res <- classify_unused_input_subtype("x", events, sessions_used = 1L, total_sessions = 40L)
  expect_identical(res$subtype, "default_kept")
  expect_equal(res$features$unique_value_count, 1L)
  expect_equal(res$features$sessions_used, 1L)
})

test_that("default_kept fires across multiple sessions that share one value", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "default", n_touches = 2),
      list(session_id = "s2", input_id = "x", value = "default", n_touches = 1)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  res <- classify_unused_input_subtype("x", events, sessions_used = 2L, total_sessions = 40L)
  expect_identical(res$subtype, "default_kept")
})

test_that("touched_once_abandoned fires when users touch once each with varied values", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "a", n_touches = 1),
      list(session_id = "s2", input_id = "x", value = "b", n_touches = 1)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  res <- classify_unused_input_subtype("x", events, sessions_used = 2L, total_sessions = 40L)
  expect_identical(res$subtype, "touched_once_abandoned")
})

test_that("rarely_used fires for low-adoption inputs with multi-touch sessions", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "a", n_touches = 3),
      list(session_id = "s2", input_id = "x", value = "b", n_touches = 5)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  res <- classify_unused_input_subtype("x", events, sessions_used = 2L, total_sessions = 40L)
  expect_identical(res$subtype, "rarely_used")
})

test_that("classifier tolerates a missing value column", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", n_touches = 1)
    ),
    idle_sessions = paste0("s", 2:40)
  )
  events$value <- NULL
  res <- classify_unused_input_subtype("x", events, sessions_used = 1L, total_sessions = 40L)
  # without value, default_kept cannot fire; single touch falls through to
  # touched_once_abandoned.
  expect_identical(res$subtype, "touched_once_abandoned")
})

test_that("classifier treats an all-NA value column like an absent one", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = NA, n_touches = 3)
    ),
    idle_sessions = paste0("s", 2:40)
  )
  res <- classify_unused_input_subtype("x", events, sessions_used = 1L, total_sessions = 40L)
  # no usable value info, median touches == 3 -> rarely_used
  expect_identical(res$subtype, "rarely_used")
})

# ==============================================================================
# find_unused_inputs() enriches each record with subtype + features
# ==============================================================================

test_that("find_unused_inputs emits subtype and features per unused input", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "defaulted", value = "def", n_touches = 1),
      list(session_id = "s2", input_id = "glanced", value = "a", n_touches = 1),
      list(session_id = "s3", input_id = "glanced", value = "b", n_touches = 1)
    ),
    idle_sessions = paste0("s", 4:60)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)

  expect_equal(length(unused), 2L)
  subtypes <- vapply(unused, `[[`, character(1L), "subtype")
  ids <- vapply(unused, `[[`, character(1L), "input_id")
  expect_setequal(subtypes, c("default_kept", "touched_once_abandoned"))
  expect_setequal(ids, c("defaulted", "glanced"))

  for (rec in unused) {
    expect_true(is.list(rec$features))
    expect_equal(rec$features$sessions_used, rec$sessions_used)
  }
})

# ==============================================================================
# create_unused_input_notice() maps subtype -> theory via signal matcher
# ==============================================================================

test_that("default_kept subtype resolves to Default Effect", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "default", n_touches = 1)
    ),
    idle_sessions = paste0("s", 2:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_s3_class(notice, "bid_stage")
  expect_identical(notice$theory[1L], "Default Effect")
})

test_that("touched_once_abandoned subtype resolves to Processing Fluency", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "a", n_touches = 1),
      list(session_id = "s2", input_id = "x", value = "b", n_touches = 1)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_identical(notice$theory[1L], "Processing Fluency")
})

test_that("rarely_used subtype falls back to Cognitive Load Theory", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "x", value = "a", n_touches = 3),
      list(session_id = "s2", input_id = "x", value = "b", n_touches = 5)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_identical(notice$theory[1L], "Cognitive Load Theory")
})

test_that("create_unused_input_notice accepts legacy input_info without subtype", {
  # back-compat: orchestrators that bypass find_unused_inputs must still work.
  legacy <- list(input_id = "x", sessions_used = 0, usage_rate = 0)
  notice <- create_unused_input_notice(
    legacy, total_sessions = 40L, events = NULL
  )
  expect_s3_class(notice, "bid_stage")
})

# ==============================================================================
# Problem wording is customized per subtype
# ==============================================================================

test_that("default_kept problem mentions default-value behavior", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "my_slider", value = "d", n_touches = 1)
    ),
    idle_sessions = paste0("s", 2:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_match(notice$problem[1L], "default", ignore.case = TRUE)
  expect_match(notice$problem[1L], "my_slider")
})

test_that("touched_once_abandoned problem mentions abandonment", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "my_slider", value = "a", n_touches = 1),
      list(session_id = "s2", input_id = "my_slider", value = "b", n_touches = 1)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_match(notice$problem[1L], "abandon|once", ignore.case = TRUE)
})

test_that("rarely_used problem retains the generic interaction wording", {
  events <- make_unused_input_events(
    touches = list(
      list(session_id = "s1", input_id = "my_slider", value = "a", n_touches = 3),
      list(session_id = "s2", input_id = "my_slider", value = "b", n_touches = 5)
    ),
    idle_sessions = paste0("s", 3:40)
  )
  unused <- find_unused_inputs(events, threshold = 0.05)
  notice <- create_unused_input_notice(
    unused[[1L]], total_sessions = 40L, events = events
  )
  expect_match(notice$problem[1L], "not interacting", ignore.case = TRUE)
})
