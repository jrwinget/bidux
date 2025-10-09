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

  if ("Anticipate" %in% stages && "Notice" %in% stages) {
    result$anticipate <- create_minimal_anticipate(result$notice)
  }

  if ("Structure" %in% stages && "Anticipate" %in% stages) {
    result$structure <- create_minimal_structure(result$anticipate)
  }

  if ("Validate" %in% stages && "Structure" %in% stages) {
    result$validate <- create_minimal_validate(result$structure)
  }

  result
}

create_minimal_anticipate <- function(previous_stage = NULL) {
  if (is.null(previous_stage)) {
    previous_stage <- create_minimal_notice()
  }
  bid_anticipate(
    previous_stage = previous_stage,
    bias_mitigations = list(
      anchoring = "Provide context",
      framing = "Use neutral language"
    )
  )
}

create_minimal_structure <- function(previous_stage = NULL) {
  if (is.null(previous_stage)) {
    previous_stage <- create_minimal_anticipate()
  }
  bid_structure(previous_stage = previous_stage)
}

create_minimal_validate <- function(previous_stage = NULL) {
  if (is.null(previous_stage)) {
    previous_stage <- create_minimal_structure()
  }
  bid_validate(
    previous_stage = previous_stage,
    summary_panel = "Test summary",
    next_steps = c("Test step 1", "Test step 2")
  )
}

# Create full workflow with all stages
create_full_workflow <- function(override_params = list()) {
  # Create Interpret stage
  interpret_params <- list(
    central_question = "How can we improve user engagement?",
    data_story = list(
      hook = "User engagement declining",
      context = "Recent changes to UI",
      tension = "Users finding features",
      resolution = "Simplify navigation"
    )
  )
  interpret_params <- utils::modifyList(interpret_params, override_params$interpret %||% list())
  interpret_stage <- do.call(bid_interpret, interpret_params)

  # Create Notice stage
  notice_params <- list(
    previous_stage = interpret_stage,
    problem = "Navigation is confusing",
    evidence = "User testing shows 60% struggle to find key features"
  )
  notice_params <- utils::modifyList(notice_params, override_params$notice %||% list())
  notice_stage <- do.call(bid_notice, notice_params)

  # Create Anticipate stage
  anticipate_params <- list(
    previous_stage = notice_stage,
    bias_mitigations = list(
      anchoring = "Show relative comparisons",
      framing = "Use both positive and negative framing"
    )
  )
  anticipate_params <- utils::modifyList(anticipate_params, override_params$anticipate %||% list())
  anticipate_stage <- do.call(bid_anticipate, anticipate_params)

  # Create Structure stage
  structure_params <- list(previous_stage = anticipate_stage)
  structure_params <- utils::modifyList(structure_params, override_params$structure %||% list())
  structure_stage <- do.call(bid_structure, structure_params)

  # Create Validate stage
  validate_params <- list(
    previous_stage = structure_stage,
    summary_panel = "Dashboard shows key metrics with clear navigation",
    next_steps = c(
      "Conduct user testing with 10 participants",
      "Measure time-to-task completion",
      "Collect qualitative feedback"
    )
  )
  validate_params <- utils::modifyList(validate_params, override_params$validate %||% list())
  validate_stage <- do.call(bid_validate, validate_params)

  list(
    interpret = interpret_stage,
    notice = notice_stage,
    anticipate = anticipate_stage,
    structure = structure_stage,
    validate = validate_stage
  )
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

# Validate complete BID workflow chain
expect_valid_bid_chain <- function(workflow) {
  expect_type(workflow, "list")

  # Check that workflow has expected stages
  if ("interpret" %in% names(workflow)) {
    expect_s3_class(workflow$interpret, "bid_stage")
    expect_equal(get_stage(workflow$interpret), "Interpret")
  }

  if ("notice" %in% names(workflow)) {
    expect_s3_class(workflow$notice, "bid_stage")
    expect_equal(get_stage(workflow$notice), "Notice")
  }

  if ("anticipate" %in% names(workflow)) {
    expect_s3_class(workflow$anticipate, "bid_stage")
    expect_equal(get_stage(workflow$anticipate), "Anticipate")
  }

  if ("structure" %in% names(workflow)) {
    expect_s3_class(workflow$structure, "bid_stage")
    expect_equal(get_stage(workflow$structure), "Structure")
  }

  if ("validate" %in% names(workflow)) {
    expect_s3_class(workflow$validate, "bid_stage")
    expect_equal(get_stage(workflow$validate), "Validate")
  }

  invisible(TRUE)
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
