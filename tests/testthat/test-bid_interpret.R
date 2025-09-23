# Optimized tests for bid_interpret function
# focused on core functionality and essential edge cases

# Test fixtures to reduce repetition
create_sample_data_story <- function() {
  list(
    hook = "Users are confused by the current interface",
    context = "Dashboard has grown complex over time",
    tension = "Performance metrics are hard to find",
    resolution = "Simplify and reorganize for clarity"
  )
}

create_sample_personas <- function() {
  list(
    list(
      name = "Data Analyst",
      goals = "Quickly identify trends and patterns",
      pain_points = "Complex navigation, hidden features",
      technical_level = "Advanced"
    ),
    list(
      name = "Manager",
      goals = "Monitor team performance",
      pain_points = "Too much detail, slow loading",
      technical_level = "Intermediate"
    )
  )
}

create_sample_previous_stage <- function() {
  tibble::tibble(
    stage = "Notice",
    problem = "Users struggle to find important metrics",
    theory = "Visual Hierarchies",
    evidence = "User feedback from interviews",
    target_audience = "Marketing team",
    suggestions = "Example suggestions",
    timestamp = Sys.time()
  )
}

# ============================================================================
# CORE FUNCTIONALITY TESTS
# ============================================================================

test_that("bid_interpret works with minimal required parameters", {
  result <- bid_interpret(central_question = "How can we improve the test scenario?")

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_true("central_question" %in% names(result))
  expect_equal(result$central_question[1], "How can we improve the test scenario?")
})

test_that("bid_interpret works with complete data story", {
  result <- bid_interpret(
    central_question = "What is causing data complexity issues?",
    data_story = create_sample_data_story()
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$central_question[1], "What is causing data complexity issues?")
  expect_true(!is.na(result$data_story_hook[1]))
  expect_true(!is.na(result$data_story_context[1]))
})

test_that("bid_interpret works with user personas", {
  result <- bid_interpret(
    central_question = "How to improve user experience?",
    user_personas = create_sample_personas()
  )

  expect_s3_class(result, "bid_stage")
  expect_true("user_personas" %in% names(result))
  expect_true(!is.na(result$user_personas[1]))
})

test_that("bid_interpret works with target audience", {
  result <- bid_interpret(
    central_question = "How to optimize for managers?",
    target_audience = "Executive team"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$target_audience[1], "Executive team")
})

# ============================================================================
# PARAMETER VALIDATION TESTS
# ============================================================================

test_that("bid_interpret validates data_story parameter", {
  expect_error(
    bid_interpret(
      central_question = "Test question",
      data_story = "not a list"
    ),
    regexp = "'data_story' must be a list"
  )

  expect_error(
    bid_interpret(
      central_question = "Test question",
      data_story = 123
    ),
    regexp = "'data_story' must be a list"
  )
})

test_that("bid_interpret validates user_personas parameter", {
  # test with invalid personas structure - should error
  expect_error(
    bid_interpret(
      central_question = "Test question",
      user_personas = "not a list"
    )
  )

  expect_error(
    bid_interpret(
      central_question = "Test question",
      user_personas = list(list(goals = "missing name"))
    )
  )
})

test_that("bid_interpret validates central_question parameter", {
  expect_error(
    bid_interpret(central_question = NULL),
    "central_question.*required|must be provided"
  )

  expect_error(
    bid_interpret(central_question = ""),
    "central_question.*required|must be provided"
  )

  expect_error(
    bid_interpret(central_question = NA),
    "central_question.*required|must be provided"
  )
})

# ============================================================================
# AUTO-SUGGESTION FUNCTIONALITY TESTS
# ============================================================================

test_that("bid_interpret auto-suggests central_question when provided as NULL with previous_stage", {
  previous_stage <- create_sample_previous_stage()

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$central_question[1]))
  expect_true(nchar(result$central_question[1]) > 0)
})

test_that("bid_interpret auto-suggests data_story when NULL", {
  suppressMessages(
    result <- bid_interpret(
      central_question = "Test question",
      data_story = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$data_story_hook[1]))
  expect_true(nchar(result$data_story_hook[1]) > 0)
})

test_that("bid_interpret auto-suggests user_personas when NULL", {
  suppressMessages(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$user_personas[1]))
  expect_true(nchar(result$user_personas[1]) > 0)
})

# ============================================================================
# DATA STORY HANDLING TESTS
# ============================================================================

test_that("bid_interpret handles partial data_story", {
  partial_story <- list(
    hook = "Users are confused",
    context = "Interface is complex"
    # missing tension and resolution
  )

  result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = partial_story
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$data_story_hook[1], "Users are confused")
  expect_equal(result$data_story_context[1], "Interface is complex")
})

test_that("bid_interpret handles empty data_story elements", {
  story_with_empty <- list(
    hook = "Valid hook",
    context = "",
    tension = NA,
    resolution = NULL
  )

  result <- bid_interpret(
    central_question = "Test question",
    data_story = story_with_empty
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$data_story_hook[1], "Valid hook")
  # empty/NA/NULL elements should be handled gracefully
})

# ============================================================================
# WORKFLOW INTEGRATION TESTS
# ============================================================================

test_that("bid_interpret works as first stage in BID workflow", {
  result <- bid_interpret(
    central_question = "How to improve user engagement?",
    data_story = create_sample_data_story(),
    user_personas = create_sample_personas()
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")

  # should work with subsequent stages
  expect_no_error(
    bid_notice(
      previous_stage = result,
      problem = "Test problem",
      evidence = "Test evidence"
    )
  )
})

test_that("bid_interpret works with previous_stage for iterative workflow", {
  previous_stage <- create_sample_previous_stage()

  result <- bid_interpret(
    previous_stage = previous_stage,
    central_question = "Updated central question",
    data_story = create_sample_data_story()
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$central_question[1], "Updated central question")
})

test_that("bid_interpret preserves essential metadata", {
  result <- bid_interpret(
    central_question = "Metadata test question",
    target_audience = "Test audience"
  )

  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))
  expect_equal(result$stage[1], "Interpret")
  expect_s3_class(result$timestamp, "POSIXct")
})

# ============================================================================
# EDGE CASES AND ERROR HANDLING
# ============================================================================

test_that("bid_interpret handles missing optional fields gracefully", {
  result <- bid_interpret(central_question = "Minimal test")

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_equal(result$central_question[1], "Minimal test")
})

test_that("bid_interpret handles unexpected parameters gracefully", {
  expect_warning(
    result <- bid_interpret(
      central_question = "Test question",
      unexpected_param = "should be ignored"
    ),
    "unexpected.*parameter|ignored"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_interpret generates suggestions", {
  result <- bid_interpret(
    central_question = "How to improve dashboard usability?",
    data_story = create_sample_data_story()
  )

  expect_s3_class(result, "bid_stage")
  expect_true("suggestions" %in% names(result))
  expect_true(!is.na(result$suggestions[1]))
  expect_true(nchar(result$suggestions[1]) > 0)
})