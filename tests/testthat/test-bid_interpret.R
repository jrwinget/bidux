# ==============================================================================
# HELPERS
# ==============================================================================

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

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

test_that("bid_interpret works with minimal required parameters", {
  result <- bid_interpret(
    central_question = "How can we improve the test scenario?"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_true("central_question" %in% names(result))
  expect_equal(
    result$central_question[1],
    "How can we improve the test scenario?"
  )
})

test_that("bid_interpret works with complete data story", {
  result <- bid_interpret(
    central_question = "What is causing data complexity issues?",
    data_story = create_sample_data_story()
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(
    result$central_question[1],
    "What is causing data complexity issues?"
  )
  expect_true(!is.na(result$hook[1]))
  expect_true(!is.na(result$context[1]))
})

test_that("bid_interpret works with user personas", {
  result <- bid_interpret(
    central_question = "How to improve user experience?",
    user_personas = create_sample_personas()
  )

  expect_s3_class(result, "bid_stage")
  expect_true("personas" %in% names(result))
  expect_true(!is.na(result$personas[1]))
})


# ==============================================================================
# PARAMETER VALIDATION TESTS
# ==============================================================================

test_that("bid_interpret validates data_story parameter", {
  expect_error(
    bid_interpret(
      central_question = "Test question",
      data_story = "not a list"
    ),
    regexp = "data_story must be a bid_data_story object or list"
  )

  expect_error(
    bid_interpret(
      central_question = "Test question",
      data_story = 123
    ),
    regexp = "data_story must be a bid_data_story object or list"
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

  # Migration provides default name for missing names
  expect_warning(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = list(list(goals = "missing name"))
    ),
    "deprecated list format"
  )
  # Should auto-fill missing name
  expect_true("personas" %in% names(result))
})

test_that("bid_interpret handles NULL/empty central_question by auto-suggestion", {
  suppressMessages(result1 <- bid_interpret(central_question = NULL))
  expect_s3_class(result1, "bid_stage")
  expect_false(is.na(result1$central_question[1]))

  suppressMessages(result2 <- bid_interpret(central_question = ""))
  expect_s3_class(result2, "bid_stage")
  expect_false(is.na(result2$central_question[1]))
})

test_that("bid_interpret handles NA central_question", {
  # Current implementation has an issue with NA handling
  expect_error(
    bid_interpret(central_question = NA),
    "missing value where TRUE/FALSE needed"
  )
})

# ==============================================================================
# AUTO-SUGGESTION FUNCTIONALITY TESTS
# ==============================================================================

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
  expect_false(is.na(result$hook[1]))
  expect_true(nchar(result$hook[1]) > 0)
})

test_that("bid_interpret handles NULL user_personas", {
  suppressMessages(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$personas[1]))
})

# ==============================================================================
# DATA STORY HANDLING TESTS
# ==============================================================================

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
  expect_equal(result$hook[1], "Users are confused")
  expect_equal(result$context[1], "Interface is complex")
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
  expect_equal(result$hook[1], "Valid hook")
  # empty/NA/NULL elements should be handled gracefully
})

# ==============================================================================
# WORKFLOW INTEGRATION TESTS
# ==============================================================================

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
    central_question = "Metadata test question"
  )

  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))
  expect_equal(result$stage[1], "Interpret")
  expect_s3_class(result$timestamp, "POSIXct")
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("bid_interpret handles missing optional fields gracefully", {
  result <- bid_interpret(central_question = "Minimal test")

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_equal(result$central_question[1], "Minimal test")
})

test_that("bid_interpret rejects unexpected parameters", {
  expect_error(
    bid_interpret(
      central_question = "Test question",
      unexpected_param = "should be ignored"
    ),
    "unused argument"
  )
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

# ==============================================================================
# ADDITIONAL EDGE CASE COVERAGE TESTS
# ==============================================================================

test_that("bid_interpret handles user_personas as data.frame", {
  personas_df <- data.frame(
    name = c("Alice", "Bob"),
    goals = c("Quick insights", "Deep analysis"),
    pain_points = c("Complex UI", "Slow loading"),
    technical_level = c("intermediate", "advanced"),
    stringsAsFactors = FALSE
  )

  result <- bid_interpret(
    central_question = "How to serve different user types?",
    user_personas = personas_df
  )

  expect_s3_class(result, "bid_stage")
  expect_true("personas" %in% names(result))
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret handles persona data with missing names", {
  expect_warning(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = list(list(
        goals = "Test goals",
        pain_points = "Test pain points",
        technical_level = "intermediate"
      ))
    ),
    "deprecated list format"
  )

  expect_s3_class(result, "bid_stage")
  expect_true("personas" %in% names(result))
})

test_that("bid_interpret works with new_data_story() constructor", {
  story <- new_data_story(
    hook = "Users struggling with complexity",
    context = "Dashboard has grown over time",
    tension = "Features buried in menus",
    resolution = "Simplify navigation",
    audience = "Product managers"
  )

  result <- bid_interpret(
    central_question = "How to simplify navigation?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$hook[1], "Users struggling with complexity")
  expect_equal(result$context[1], "Dashboard has grown over time")
})

test_that("bid_interpret works with new_user_personas() constructor", {
  personas <- new_user_personas(data.frame(
    name = "Test Persona",
    goals = "Test goals",
    pain_points = "Test pain points",
    technical_level = "beginner",
    stringsAsFactors = FALSE
  ))

  result <- bid_interpret(
    central_question = "Test question",
    user_personas = personas
  )

  expect_s3_class(result, "bid_stage")
  expect_true("personas" %in% names(result))
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret handles various central_question lengths", {
  # very short question
  result_short <- bid_interpret(central_question = "How to improve?")
  expect_s3_class(result_short, "bid_stage")
  expect_match(result_short$suggestions[1], "specificity")

  # very long question
  long_question <- paste(rep("How can we improve the user experience?", 10), collapse = " ")
  result_long <- bid_interpret(central_question = long_question)
  expect_s3_class(result_long, "bid_stage")
  expect_match(result_long$suggestions[1], "simplifying")

  # appropriate length question
  result_good <- bid_interpret(central_question = "How can we improve dashboard navigation for new users?")
  expect_s3_class(result_good, "bid_stage")
  expect_match(result_good$suggestions[1], "appropriately scoped")
})

test_that("bid_interpret handles data_story completeness variations", {
  # complete story (100%)
  complete_story <- list(
    hook = "Hook text",
    context = "Context text",
    tension = "Tension text",
    resolution = "Resolution text"
  )
  expect_warning(
    result_complete <- bid_interpret(
      central_question = "Test?",
      data_story = complete_story
    ),
    "deprecated list format"
  )
  expect_match(result_complete$suggestions[1], "all key elements")

  # 75% complete story
  partial_story <- list(
    hook = "Hook text",
    context = "Context text",
    tension = "Tension text"
  )
  expect_warning(
    result_partial <- bid_interpret(
      central_question = "Test?",
      data_story = partial_story
    ),
    "deprecated list format"
  )
  expect_match(result_partial$suggestions[1], "almost complete")

  # 50% complete story
  half_story <- list(
    hook = "Hook text",
    context = "Context text"
  )
  expect_warning(
    result_half <- bid_interpret(
      central_question = "Test?",
      data_story = half_story
    ),
    "deprecated list format"
  )
  expect_match(result_half$suggestions[1], "taking shape")

  # < 50% complete story
  minimal_story <- list(
    context = "Context only"
  )
  expect_warning(
    result_minimal <- bid_interpret(
      central_question = "Test?",
      data_story = minimal_story
    ),
    "deprecated list format"
  )
  expect_match(result_minimal$suggestions[1], "incomplete")
})

test_that("bid_interpret auto-generates personas from audience info", {
  # test with executive audience
  story_exec <- list(
    hook = "Test hook",
    context = "Test context",
    audience = "Executive leadership team"
  )
  expect_warning(
    result_exec <- bid_interpret(
      central_question = "Test?",
      data_story = story_exec
    ),
    "deprecated list format"
  )
  expect_false(is.na(result_exec$personas[1]))

  # test with analyst audience
  story_analyst <- list(
    hook = "Test hook",
    context = "Test context",
    audience = "Data analysts and scientists"
  )
  expect_warning(
    result_analyst <- bid_interpret(
      central_question = "Test?",
      data_story = story_analyst
    ),
    "deprecated list format"
  )
  expect_false(is.na(result_analyst$personas[1]))

  # test with sales audience
  story_sales <- list(
    hook = "Test hook",
    context = "Test context",
    audience = "Sales representatives"
  )
  expect_warning(
    result_sales <- bid_interpret(
      central_question = "Test?",
      data_story = story_sales
    ),
    "deprecated list format"
  )
  expect_false(is.na(result_sales$personas[1]))
})

test_that("bid_interpret extracts audience from previous_stage", {
  previous <- create_sample_previous_stage()
  previous$target_audience <- "Marketing team"

  result <- bid_interpret(
    previous_stage = previous,
    central_question = "Test question"
  )

  expect_s3_class(result, "bid_stage")
  # should create persona from target_audience if no user_personas provided
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret handles invalid user_personas gracefully", {
  expect_error(
    bid_interpret(
      central_question = "Test?",
      user_personas = "not a list or data frame"
    ),
    "user_personas must be"
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      user_personas = 123
    ),
    "user_personas must be"
  )
})

test_that("bid_interpret quiet parameter works correctly", {
  # with quiet = TRUE, should not show messages
  expect_silent(
    result1 <- bid_interpret(
      central_question = NULL,
      quiet = TRUE
    )
  )

  # with quiet = FALSE, should show messages
  expect_message(
    result2 <- bid_interpret(
      central_question = NULL,
      quiet = FALSE
    ),
    "Stage 1"
  )
})

test_that("bid_interpret works with different previous_stage types", {
  # from Structure stage
  structure_stage <- tibble::tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )

  result_structure <- bid_interpret(
    previous_stage = structure_stage,
    central_question = "Refine understanding?"
  )
  expect_s3_class(result_structure, "bid_stage")

  # from Anticipate stage
  anticipate_stage <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: test",
    timestamp = Sys.time()
  )

  result_anticipate <- bid_interpret(
    previous_stage = anticipate_stage,
    central_question = "Refine understanding?"
  )
  expect_s3_class(result_anticipate, "bid_stage")
})
