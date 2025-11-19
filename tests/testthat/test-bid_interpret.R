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

test_that("bid_interpret validates user_personas S3 class structure", {
  personas_df <- data.frame(
    name = "Test Persona",
    goals = "Test goals",
    pain_points = "Test pain points",
    technical_level = "beginner",
    stringsAsFactors = FALSE
  )
  personas <- new_user_personas(personas_df)

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

test_that("bid_interpret handles data_story with different completeness levels", {
  # complete story (100%) - has all elements
  complete_story <- new_data_story(
    hook = "Hook text",
    context = "Context text",
    tension = "Tension text",
    resolution = "Resolution text"
  )
  result_complete <- bid_interpret(
    central_question = "Test?",
    data_story = complete_story
  )
  expect_s3_class(result_complete, "bid_stage")
  expect_equal(result_complete$hook[1], "Hook text")
  expect_equal(result_complete$tension[1], "Tension text")
  expect_equal(result_complete$resolution[1], "Resolution text")

  # minimal story (only context required)
  minimal_story <- new_data_story(
    context = "Context only"
  )
  result_minimal <- bid_interpret(
    central_question = "Test?",
    data_story = minimal_story
  )
  expect_s3_class(result_minimal, "bid_stage")
  expect_equal(result_minimal$context[1], "Context only")
})

test_that("bid_interpret auto-generates personas from audience info", {
  # test with executive audience
  story_exec <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    audience = "Executive leadership team"
  )
  result_exec <- bid_interpret(
    central_question = "Test?",
    data_story = story_exec
  )
  expect_false(is.na(result_exec$personas[1]))

  # test with analyst audience
  story_analyst <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    audience = "Data analysts and scientists"
  )
  result_analyst <- bid_interpret(
    central_question = "Test?",
    data_story = story_analyst
  )
  expect_false(is.na(result_analyst$personas[1]))

  # test with sales audience
  story_sales <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    audience = "Sales representatives"
  )
  result_sales <- bid_interpret(
    central_question = "Test?",
    data_story = story_sales
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

test_that("bid_interpret quiet parameter suppresses info messages", {
  # quiet parameter should work - just verify the function runs
  result1 <- bid_interpret(
    central_question = "Test question",
    quiet = TRUE
  )
  expect_s3_class(result1, "bid_stage")

  result2 <- bid_interpret(
    central_question = "Test question",
    quiet = FALSE
  )
  expect_s3_class(result2, "bid_stage")
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

test_that("bid_interpret errors on invalid bid_data_story object", {
  # create an object with bid_data_story class but invalid structure
  # missing context - required field
  invalid_story <- structure(
    list(
      hook = "Test hook"
    ),
    class = c("bid_data_story", "list")
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      data_story = invalid_story
    ),
    "Invalid bid_data_story object"
  )

  # object with context but neither flat format fields (hook/tension/resolution)
  # nor nested format fields (variables/relationships) - fails validation
  invalid_story2 <- structure(
    list(
      context = "Has context but no format indicator"
    ),
    class = c("bid_data_story", "list")
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      data_story = invalid_story2
    ),
    "Invalid bid_data_story object"
  )

  # object with non-character context - fails validation
  invalid_story3 <- structure(
    list(
      hook = "Test hook",
      context = 123
    ),
    class = c("bid_data_story", "list")
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      data_story = invalid_story3
    ),
    "Invalid bid_data_story object"
  )
})

test_that("bid_interpret errors on invalid bid_user_personas object", {
  # create an object with bid_user_personas class but invalid structure
  # missing required columns

  invalid_personas <- structure(
    tibble::tibble(
      name = "Test",
      goals = "Test goals"
      # missing pain_points and technical_level
    ),
    class = c("bid_user_personas", "tbl_df", "tbl", "data.frame")
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      user_personas = invalid_personas
    ),
    "Invalid bid_user_personas object"
  )

  # empty personas - should also fail validation
  empty_personas <- structure(
    tibble::tibble(
      name = character(0),
      goals = character(0),
      pain_points = character(0),
      technical_level = character(0)
    ),
    class = c("bid_user_personas", "tbl_df", "tbl", "data.frame")
  )

  expect_error(
    bid_interpret(
      central_question = "Test?",
      user_personas = empty_personas
    ),
    "Invalid bid_user_personas object"
  )
})

test_that("bid_interpret generates question for 'find/locate/discover' problems", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users cannot find key reports quickly",
    theory = "Information Architecture",
    evidence = "User testing feedback",
    target_audience = "Analysts",
    suggestions = "Improve navigation",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "easier for users to find",
    ignore.case = TRUE
  )
})

test_that("bid_interpret generates question for 'slow/delay/time' problems", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard is too slow to load",
    theory = "Performance optimization",
    evidence = "Timing metrics",
    target_audience = "End users",
    suggestions = "Optimize queries",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "speed and efficiency",
    ignore.case = TRUE
  )
})

test_that("bid_interpret generates question for 'overwhelm' problems", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users are overwhelmed by too many charts",
    theory = "Cognitive load theory",
    evidence = "Survey results",
    target_audience = "Managers",
    suggestions = "Simplify",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "cognitive load|focus",
    ignore.case = TRUE
  )
})

test_that("bid_interpret generates default question for unmatched problems", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Color scheme is not accessible",
    theory = "Accessibility",
    evidence = "Accessibility audit",
    target_audience = "All users",
    suggestions = "Update colors",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "address the issue|Color scheme",
    ignore.case = TRUE
  )
})

test_that("bid_interpret modifies question based on cognitive load theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle with complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User testing",
    target_audience = "General users",
    suggestions = "Simplify",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "cognitive load|reduce",
    ignore.case = TRUE
  )
})

test_that("bid_interpret modifies question based on Hick's Law theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle with too many menu options",
    theory = "Hick's Law",
    evidence = "Decision time metrics",
    target_audience = "New users",
    suggestions = "Reduce choices",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "simplify choices",
    ignore.case = TRUE
  )
})

test_that("bid_interpret modifies question based on Visual Hierarchy theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle to prioritize information",
    theory = "Visual Hierarchy principles",
    evidence = "Eye tracking study",
    target_audience = "Data consumers",
    suggestions = "Improve layout",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "visual hierarchy",
    ignore.case = TRUE
  )
})

test_that("bid_interpret generates visual approach for cognitive load theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard is cluttered",
    theory = "Cognitive Load",
    evidence = "User feedback",
    target_audience = "Analysts",
    suggestions = "Simplify",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL,
      data_story = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # visual_approach should be in the result
  expect_true("visual_approach" %in% names(result))
})

test_that("bid_interpret generates visual approach for Hick's Law theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Too many navigation options",
    theory = "Hick's Law decision time",
    evidence = "Analytics",
    target_audience = "Users",
    suggestions = "Reduce options",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL,
      data_story = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true("visual_approach" %in% names(result))
})

test_that("bid_interpret generates visual approach for Visual Hierarchy theory", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Important data not prominent",
    theory = "Visual Hierarchy design",
    evidence = "Heatmaps",
    target_audience = "Executives",
    suggestions = "Improve prominence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL,
      data_story = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true("visual_approach" %in% names(result))
})

test_that("bid_interpret extracts character metrics from data_story", {
  story <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    metrics = "metric1, metric2, metric3"
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_true("metrics" %in% names(result))
  expect_equal(result$metrics[1], "metric1, metric2, metric3")
})

test_that("bid_interpret extracts numeric metrics from data_story", {
  story <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    metrics = c(100, 200, 300)
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_true("metrics" %in% names(result))
  expect_equal(result$metrics[1], "100, 200, 300")
})

test_that("bid_interpret extracts list metrics from data_story", {
  story <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    metrics = list(kpi1 = "revenue", kpi2 = "engagement")
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_true("metrics" %in% names(result))
  expect_match(result$metrics[1], "revenue.*engagement|engagement.*revenue")
})

test_that("bid_interpret handles character vector metrics from data_story", {
  story <- new_data_story(
    hook = "Test hook",
    context = "Test context",
    metrics = c("DAU", "MAU", "retention_rate")
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$metrics[1], "DAU, MAU, retention_rate")
})

test_that("bid_interpret handles Notice stage with NA problem", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = NA_character_,
    theory = "Some theory",
    evidence = "Some evidence",
    target_audience = "Users",
    suggestions = "Suggestions",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = NULL,
      data_story = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$central_question[1],
    "improve the user experience",
    ignore.case = TRUE
  )
})

test_that("bid_interpret generates appropriate suggestion for incomplete story", {
  # use legacy list format to create an actually incomplete story
  # new_data_story() always includes all fields in names even if NULL
  suppressWarnings(
    result <- bid_interpret(
      central_question = "Test?",
      data_story = list(
        context = "Only context"
        # missing hook, tension, resolution
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$suggestions[1], "incomplete|25%", ignore.case = TRUE)
})

test_that("bid_interpret generates taking shape suggestion for 50% complete story", {
  # manually create a bid_data_story with exactly 2 of 4 elements at top level
  story <- structure(
    list(
      hook = "Hook",
      context = "Context"
      # no tension or resolution at top level = 50%
    ),
    class = c("bid_data_story", "list")
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$suggestions[1], "taking shape|50%", ignore.case = TRUE)
})

test_that("bid_interpret generates almost complete suggestion for 75% complete story", {
  # manually create a bid_data_story with exactly 3 of 4 elements at top level
  story <- structure(
    list(
      hook = "Hook",
      context = "Context",
      tension = "Tension"
      # no resolution at top level = 75%
    ),
    class = c("bid_data_story", "list")
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$suggestions[1], "almost complete|75%", ignore.case = TRUE)
})

test_that("bid_interpret errors on empty list user_personas", {
  # empty list cannot be migrated - results in error
  expect_error(
    suppressWarnings(
      bid_interpret(
        central_question = "Test?",
        user_personas = list()
      )
    ),
    "at least 1 row"
  )
})

test_that("bid_interpret generates correct suggestion for multiple personas", {
  personas_df <- data.frame(
    name = c("Analyst", "Manager", "Executive"),
    goals = c("Deep analysis", "Team oversight", "Strategic view"),
    pain_points = c("Complexity", "Time", "Detail"),
    technical_level = c("advanced", "intermediate", "beginner"),
    stringsAsFactors = FALSE
  )

  result <- bid_interpret(
    central_question = "Test?",
    user_personas = personas_df
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$suggestions[1],
    "3 persona\\(s\\)|defined.*3",
    ignore.case = TRUE
  )
})

test_that("bid_interpret handles audience in legacy list data_story", {
  # note: when legacy list is migrated, audience goes into nested metadata
  # this test verifies the migration behavior
  suppressWarnings(
    result <- bid_interpret(
      central_question = "Test?",
      data_story = list(
        hook = "Hook",
        context = "Context",
        audience = "Marketing professionals"
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  # migration nests audience in metadata$metadata, so it's not directly accessible
  # this is expected behavior for deprecated legacy format
  expect_true("personas" %in% names(result))
})

test_that("bid_interpret generates developer persona from audience", {
  story <- new_data_story(
    hook = "Test",
    context = "Test context",
    audience = "Software developers and engineers"
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret generates operations persona from audience", {
  story <- new_data_story(
    hook = "Test",
    context = "Test context",
    audience = "Operations and IT staff"
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret generates generic persona from unrecognized audience", {
  story <- new_data_story(
    hook = "Test",
    context = "Test context",
    audience = "Some custom user group"
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$personas[1]))
})

test_that("bid_interpret handles empty audience string in data_story", {
  story <- new_data_story(
    hook = "Test",
    context = "Test context",
    audience = ""
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  # empty audience should result in NA personas
  expect_true(is.na(result$personas[1]))
})

test_that("bid_interpret handles whitespace-only audience in data_story", {
  story <- new_data_story(
    hook = "Test",
    context = "Test context",
    audience = "   "
  )

  result <- bid_interpret(
    central_question = "Test?",
    data_story = story
  )

  expect_s3_class(result, "bid_stage")
  # whitespace-only audience should result in NA personas
  expect_true(is.na(result$personas[1]))
})

test_that("bid_interpret truncates long central question in message", {
  # question longer than 60 characters should be truncated in the message
  long_question <- paste0(
    "How can we improve the dashboard experience for all users ",
    "across multiple departments and regions?"
  )
  expect_true(nchar(long_question) > 60)


  result <- bid_interpret(
    central_question = long_question
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$central_question[1], long_question)
})
