# ============================================================================
# SHARED TEST UTILITIES AND OBJECTS
# ============================================================================

# validation helper that reduces repetitive S3 class checks
validate_bid_stage_structure <- function(
    result,
    expected_stage,
    required_columns = NULL) {
  expect_s3_class(result, "bid_stage")
  expect_s3_class(result, "tbl_df")
  expect_equal(get_stage(result), expected_stage)
  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))

  if (!is.null(required_columns)) {
    expect_true(all(required_columns %in% names(result)))
  }

  # validate metadata structure
  metadata <- get_metadata(result)
  expect_type(metadata, "list")

  invisible(TRUE)
}

# create minimal test BID stages for workflow testing
create_minimal_interpret <- function() {
  bid_interpret(central_question = "Test question?")
}

create_minimal_notice <- function(previous_stage = NULL) {
  if (is.null(previous_stage)) {
    previous_stage <- create_minimal_interpret()
  }
  bid_notice(
    previous_stage = previous_stage,
    problem = "Test problem",
    evidence = "Test evidence"
  )
}

create_minimal_workflow <- function(stages = c("Interpret", "Notice")) {
  result <- list()

  if ("Interpret" %in% stages) {
    result$interpret <- create_minimal_interpret()
  }

  if ("Notice" %in% stages && "Interpret" %in% stages) {
    result$notice <- create_minimal_notice(result$interpret)
  }

  result
}

# test data objects
sample_data_story <- list(
  hook = "Users struggle with interface",
  context = "Current design is complex",
  tension = "Performance issues arise",
  resolution = "Simplify and optimize"
)

sample_personas <- list(
  list(
    name = "Data Analyst",
    goals = "Quick data analysis",
    pain_points = "Complex navigation",
    technical_level = "Advanced"
  )
)

# telemetry test helpers
create_test_telemetry_events <- function(sessions = 2) {
  events <- list()
  for (i in 1:sessions) {
    session_id <- paste0("session", i)
    events <- append(events, list(
      list(
        timestamp = paste0("2025-01-01 10:0", i, ":00"),
        session_id = session_id,
        event_type = "login"
      ),
      list(
        timestamp = paste0("2025-01-01 10:0", i, ":05"),
        session_id = session_id,
        event_type = "input",
        input_id = "test_input"
      )
    ))
  }
  events
}

# common expectation helpers
expect_valid_timestamp <- function(timestamp) {
  expect_s3_class(timestamp, "POSIXct")
  expect_true(timestamp <= Sys.time())
}

expect_non_empty_character <- function(value, min_length = 1) {
  expect_type(value, "character")
  expect_false(is.na(value))
  expect_gte(nchar(value), min_length)
}

# skip helpers for conditional testing
skip_if_no_telemetry_deps <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE)) {
    skip("Telemetry dependencies not available")
  }
}
