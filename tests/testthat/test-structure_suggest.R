test_that("safe_lower handles various input types", {
  expect_equal(safe_lower("UPPERCASE"), "uppercase")
  expect_equal(safe_lower("MixedCase"), "mixedcase")
  expect_equal(safe_lower(NULL), "")
  expect_equal(safe_lower(NA), "na") # NA gets converted to "na" string
  expect_equal(safe_lower(""), "")
  expect_equal(safe_lower(c("A", "B")), "a b")
  expect_equal(safe_lower("  spaced  "), "spaced")
})

test_that("safe_stage_data_story_access extracts data story elements", {
  # test with proper data story structure
  previous_stage <- data.frame(
    data_story = I(list(list(
      hook = "Users struggle",
      context = "Complex interface",
      tension = "Confusion",
      resolution = "Simplify"
    )))
  )

  expect_equal(safe_stage_data_story_access(previous_stage, "hook"), "Users struggle")
  expect_equal(safe_stage_data_story_access(previous_stage, "context"), "Complex interface")
  expect_equal(safe_stage_data_story_access(previous_stage, "nonexistent"), "")

  # test with named list
  named_story <- data.frame(
    data_story = I(list(
      hook = "Direct hook",
      context = "Direct context"
    ))
  )

  expect_equal(safe_stage_data_story_access(named_story, "hook"), "Direct hook")

  # test with missing data story
  no_story <- data.frame(stage = "Notice")
  expect_equal(safe_stage_data_story_access(no_story, "hook"), "")

  # test with NULL data story
  null_story <- data.frame(data_story = I(list(NULL)))
  expect_equal(safe_stage_data_story_access(null_story, "hook"), "")
})

test_that("structure_suggestions generates concept groups", {
  # mock previous stage with theory and story elements
  previous_stage <- data.frame(
    stage = "Notice",
    theory = "Cognitive Load Theory",
    problem = "complex interface with focus issues",
    central_question = "How to reduce overload?",
    hook = "Users struggle",
    context = "Dashboard evolution",
    tension = "Too many options",
    resolution = "Simplify step by step"
  )

  suggestions <- structure_suggestions(previous_stage, c("Visual Hierarchy"))

  expect_true(is.list(suggestions))
  expect_gt(length(suggestions), 0)

  # each group should have concept and suggestions
  for (group in suggestions) {
    expect_true("concept" %in% names(group))
    expect_true("suggestions" %in% names(group))
    expect_true(is.character(group$concept))
    expect_true(is.list(group$suggestions))

    # each suggestion should have required fields
    if (length(group$suggestions) > 0) {
      suggestion <- group$suggestions[[1]]
      expect_true("title" %in% names(suggestion))
      expect_true("details" %in% names(suggestion))
      expect_true("components" %in% names(suggestion))
      expect_true("rationale" %in% names(suggestion))
      expect_true("score" %in% names(suggestion))

      # score should be between 0 and 1
      expect_gte(suggestion$score, 0)
      expect_lte(suggestion$score, 1)
    }
  }
})
