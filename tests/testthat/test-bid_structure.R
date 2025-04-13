library(testthat)
library(tibble)

test_that("bid_structure returns a tibble with stage 'Structure'", {
  local_mock(
    bid_concepts = function(search = NULL) {
      tibble(
        concept = "Test Concept",
        description = "Dummy description",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )
  
  previous_stage <- tibble(
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
      tibble(
        concept = "Test Concept",
        description = "Dummy description",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )
  
  previous_stage <- tibble(
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
  previous_stage <- tibble(
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
      tibble(
        concept = "Auto Detected Concept",
        description = "Automatically detected based on problem statement.",
        category = "Stage 1",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )
  
  previous_stage <- tibble(
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
