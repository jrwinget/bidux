test_that("safe_lower handles various input types", {
  expect_equal(safe_lower("UPPERCASE"), "uppercase")
  expect_equal(safe_lower("MixedCase"), "mixedcase")
  expect_equal(safe_lower(NULL), "")
  expect_equal(safe_lower(NA), "na")  # NA gets converted to "na" string
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

test_that("layout_rationale provides appropriate explanations", {
  # test dual_process rationale
  dual_stage <- data.frame(
    problem = "need summary vs detail views",
    evidence = "users want overview and detail modes"
  )

  rationale <- layout_rationale(dual_stage, "dual_process")
  expect_true(is.character(rationale))
  expect_true(grepl("overview vs detail|dual_process", rationale, ignore.case = TRUE))

  # test breathable rationale with overload pattern
  overload_stage <- data.frame(
    problem = "too many options overwhelm users",
    evidence = "cognitive load issues"
  )

  breathable_rationale <- layout_rationale(overload_stage, "breathable")
  expect_true(is.character(breathable_rationale))
  expect_true(grepl("overload|cognitive load|breathable", breathable_rationale, ignore.case = TRUE))

  # test grid rationale
  grid_stage <- data.frame(
    problem = "need to group related content",
    evidence = "comparison tasks"
  )

  grid_rationale <- layout_rationale(grid_stage, "grid")
  expect_true(is.character(grid_rationale))
  expect_true(grepl("group|grid", grid_rationale, ignore.case = TRUE))

  # test fallback rationale
  generic_stage <- data.frame(problem = "generic issue", evidence = "some evidence")
  fallback_rationale <- layout_rationale(generic_stage, "custom_layout")
  expect_true(is.character(fallback_rationale))
  expect_true(grepl("custom_layout", fallback_rationale))
})

test_that("suggest_layout_from_previous detects layout patterns", {
  # test dual_process detection
  dual_context <- data.frame(
    problem = "users need summary vs detail access",
    evidence = "quick vs thorough analysis needed",
    central_question = "how to support overview and detail modes?"
  )

  layout <- suggest_layout_from_previous(dual_context)
  expect_equal(layout, "dual_process")

  # test breathable detection with cognitive load
  overload_context <- data.frame(
    problem = "interface is cluttered and overwhelming",
    evidence = "users report cognitive overload"
  )

  layout_breathable <- suggest_layout_from_previous(overload_context)
  expect_equal(layout_breathable, "breathable")

  # test grid detection
  grid_context <- data.frame(
    problem = "need to group related metrics for comparison",
    evidence = "visual hierarchy important"
  )

  layout_grid <- suggest_layout_from_previous(grid_context)
  expect_equal(layout_grid, "grid")

  # test cards detection
  card_context <- data.frame(
    problem = "content needs modular blocks",
    evidence = "entity cards work well"
  )

  layout_card <- suggest_layout_from_previous(card_context)
  expect_equal(layout_card, "card")

  # test tabs detection
  tabs_context <- data.frame(
    problem = "content has clear sections",
    evidence = "progressive disclosure needed"
  )

  layout_tabs <- suggest_layout_from_previous(tabs_context)
  expect_equal(layout_tabs, "tabs")

  # test tabs avoidance with telemetry issues
  tabs_with_issues <- data.frame(
    problem = "content has sections but navigation fails",
    evidence = "progressive disclosure needed"
  )

  telemetry_flags <- list(has_navigation_issues = TRUE)
  layout_avoid_tabs <- suggest_layout_from_previous(tabs_with_issues, telemetry_flags)
  expect_equal(layout_avoid_tabs, "grid")  # should fallback to grid

  # test default fallback
  empty_context <- data.frame()
  layout_default <- suggest_layout_from_previous(empty_context)
  expect_equal(layout_default, "breathable")
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

  suggestions <- structure_suggestions(previous_stage, "breathable", c("Visual Hierarchy"))

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
