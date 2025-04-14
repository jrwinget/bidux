library(testthat)
library(tibble)

test_that("bid_structure returns a tibble with stage 'Structure'", {
  local_mock(
    bid_concepts = function(search = NULL) {
      tibble::tibble(
        concept = "Test Concept",
        description = "Dummy description",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )

  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "The dashboard layout is cluttered.",
    theory = "Visual Hierarchies",
    evidence = "User feedback indicates confusion."
  )

  result <- bid_structure(
    previous_stage,
    layout = "dual_process",
    concepts = c("Test Concept")
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage[1], "Structure")
  expect_equal(result$layout[1], "dual_process")
  expect_true("concepts" %in% names(result))
})

test_that("bid_structure warns on invalid layout", {
  local_mock(
    bid_concepts = function(search = NULL) {
      tibble::tibble(
        concept = "Test Concept",
        description = "Dummy description",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )

  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard elements are not arranged clearly.",
    theory = "Cognitive Load Theory",
    evidence = "Mixed user feedback."
  )

  expect_warning(
    bid_structure(
      previous_stage,
      layout = "invalid_layout",
      concepts = c("Test Concept")
    ),
    regexp = "not recognized as a standard layout type"
  )
})

test_that("bid_structure errors when accessibility parameter is not a list", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Layout organization is suboptimal.",
    theory = "Dual-Processing Theory",
    evidence = "Users are confused by spacing."
  )

  expect_error(
    bid_structure(
      previous_stage,
      layout = "dual_process",
      accessibility = "not a list"
    ),
    regexp = "The accessibility parameter must be a list"
  )
})

test_that("bid_structure auto-detects concepts when not provided", {
  local_mock(
    bid_concepts = function(search = NULL) {
      tibble::tibble(
        concept = "Auto Detected Concept",
        description = "Automatically detected based on problem statement.",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )

  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard elements are scattered across the screen.",
    theory = "Principle of Proximity",
    evidence = "User tests show poor grouping."
  )

  result <- bid_structure(previous_stage, layout = "dual_process")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage[1], "Structure")
  expect_true(length(result$concepts[1]) > 0)
})

test_that("bid_structure fuzzy matches concept names", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "How can we improve dashboard usability?",
    hook = "Users struggle with navigation",
    context = "Dashboard has evolved over time",
    tension = "Complex structure confuses users",
    resolution = "Simplify and reorganize",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c(
        "principal of proximity",
        "vizual hierarchy" # misspelling
      )
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_match(result$concepts, "Principle of Proximity", fixed = TRUE)
  expect_match(result$concepts, "Visual Hierarchy", fixed = TRUE)

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("proximity", "hierarchy", "load") # abbreviated
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_match(result$concepts, "Proximity", fixed = TRUE)
  expect_match(result$concepts, "Hierarchy", fixed = TRUE)
  expect_match(result$concepts, "Load", fixed = TRUE)

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c(
        "principle_of_proximity",
        "visual-hierarchy",
        "COGNITIVE_LOAD"
      )
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_match(result$concepts, "Principle of Proximity", fixed = TRUE)
  expect_match(result$concepts, "Visual Hierarchy", fixed = TRUE)
  expect_match(result$concepts, "Cognitive Load", fixed = TRUE)
})

test_that("bid_structure handles NULL concepts with automatic detection", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "How can we make data easier to find?",
    hook = "Users struggle with information overload",
    context = "Dashboard has too many charts and options",
    tension = "Critical metrics are hard to locate",
    resolution = "Establish clear visual hierarchy",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$concepts[1]))
  expect_true(nchar(result$concepts[1]) > 0)
  expect_match(result$concepts, "Visual|Hierarchy|Cognitive|Load", perl = TRUE)

  minimal_previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "How to improve?",
    hook = "Issue found",
    context = "Basic context",
    tension = "Problem exists",
    resolution = "Fix it",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(
      minimal_previous_stage,
      layout = "dual_process",
      concepts = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$concepts[1]))
  expect_true(nchar(result$concepts[1]) > 0)
})

test_that("bid_structure handles NA values in previous_stage", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = NA_character_,
    hook = "Users struggle with navigation",
    context = NA_character_,
    tension = "Complex structure confuses users",
    resolution = NA_character_,
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("Visual Hierarchy", "Cognitive Load Theory")
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$previous_question[1]))
  expect_true(is.na(result$previous_story_hook[1]))
})

test_that("bid_structure handles edge cases in accessibility parameter", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "Test question",
    hook = "Test hook",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("Visual Hierarchy"),
      accessibility = list()
    )
  )

  expect_s3_class(result, "tbl_df")
  # should warn but still work
  expect_false(is.na(result$accessibility[1]))

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("Visual Hierarchy"),
      accessibility = list(
        color_contrast = "WCAG AA compliant",
        keyboard_navigation = NA,
        screen_reader = NA_character_
      )
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$accessibility[1]))
  expect_match(result$accessibility, "color_contrast", fixed = TRUE)

  expect_warning(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("Visual Hierarchy"),
      accessibility = list(
        color_contrast = "Yes", # too short
        keyboard_navigation = "No" # too short
      )
    ),
    "invalid or too brief value"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$accessibility[1]))
})

test_that("bid_structure handles various layout values", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "Test question",
    hook = "Test hook",
    timestamp = Sys.time()
  )

  valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")

  for (layout in valid_layouts) {
    suppressMessages(
      result <- bid_structure(
        previous_stage,
        layout = layout,
        concepts = c("Visual Hierarchy")
      )
    )

    expect_s3_class(result, "tbl_df")
    expect_equal(result$layout[1], layout)
    expect_match(result$suggestions, layout, ignore.case = TRUE)
  }

  suppressMessages(
    result <- bid_structure(
      previous_stage,
      layout = "DUAL_PROCESS", # uppercase
      concepts = c("Visual Hierarchy")
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$layout[1], "dual_process")
})

test_that("bid_structure handles complex accessibility parameter structures", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )

  nested_accessibility <- list(
    vision = list(
      color_contrast = "WCAG AA compliant (4.5:1)",
      text_size = "16px minimum"
    ),
    motor = list(
      keyboard_navigation = "Full keyboard support",
      target_size = "44px minimum touch targets"
    )
  )

  # should warn about structure but not fail
  expect_warning(
    result <- bid_structure(
      previous_stage,
      layout = "dual_process",
      concepts = c("Visual Hierarchy"),
      accessibility = nested_accessibility
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$accessibility))
})
