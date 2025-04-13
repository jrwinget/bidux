library(testthat)
library(tibble)

test_that("bid_interpret returns a tibble with stage 'Interpret'", {
  local_mock(
    validate_user_personas = function(user_personas) invisible(NULL),
    bid_concepts = function(search = NULL) {
      tibble(
        concept = "Data Storytelling Framework",
        description = "Dummy description",
        category = "Stage 2",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )
  
  previous_stage <- tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays",
    target_audience = "Data Analysts"
  )
  
  result <- bid_interpret(previous_stage)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage[1], "Interpret")
  expect_true("central_question" %in% names(result))
})

test_that("bid_interpret uses provided central_question", {
  local_mock(
    validate_user_personas = function(user_personas) invisible(NULL),
    bid_concepts = function(search = NULL) {
      tibble(
        concept = "Data Storytelling Framework",
        description = "Dummy description",
        category = "Stage 2",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )
  
  previous_stage <- tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays"
  )
  
  my_question <- "What is causing data complexity issues?"
  result <- bid_interpret(previous_stage, central_question = my_question)
  
  expect_equal(result$central_question[1], my_question)
})

test_that("bid_interpret errors when data_story is not a list", {
  local_mock(
    validate_user_personas = function(user_personas) invisible(NULL)
  )
  
  previous_stage <- tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays"
  )
  
  expect_error(
    bid_interpret(previous_stage, data_story = "not a list"),
    regexp = "The data_story parameter must be a list"
  )
})

test_that("bid_interpret errors when user_personas is invalid", {
  local_mock(
    validate_user_personas = function(user_personas) {
      cli::cli_abort("Invalid user_personas provided")
    }
  )
  
  previous_stage <- tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays"
  )
  
  expect_error(
    bid_interpret(previous_stage, user_personas = "not a list"),
    regexp = "Invalid user_personas provided"
  )
})
