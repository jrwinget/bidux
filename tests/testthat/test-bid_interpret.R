library(testthat)
library(tibble)

test_that("bid_interpret returns a tibble with stage 'Interpret'", {
  local_mocked_bindings(
    validate_required_params = function(...) invisible(TRUE),
    validate_previous_stage = function(...) invisible(TRUE),
    bid_message = function(...) invisible(NULL),
    safe_column_access = function(data, column) {
      if (column %in% names(data)) data[[column]][1] else NA_character_
    },
    bid_stage = function(stage_name, data, metadata) {
      attr(data, "class") <- c("bid_stage", class(data))
      attr(data, "stage_name") <- stage_name
      attr(data, "metadata") <- metadata
      data
    }
  )
  
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays",
    target_audience = "Data Analysts"
  )

  result <- bid_interpret(previous_stage)

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_true("central_question" %in% names(result))
})

test_that("bid_interpret uses provided central_question", {
  local_mocked_bindings(
    validate_user_personas = function(user_personas) invisible(NULL),
    bid_concepts = function(search = NULL) {
      tibble::tibble(
        concept = "Data Storytelling Framework",
        description = "Dummy description",
        category = "Stage 2",
        reference = NA_character_,
        example = NA_character_
      )
    }
  )

  previous_stage <- tibble::tibble(
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
  local_mocked_bindings(
    validate_user_personas = function(user_personas) invisible(NULL)
  )

  previous_stage <- tibble::tibble(
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
  local_mocked_bindings(
    validate_user_personas = function(user_personas) {
      cli::cli_abort("Invalid user_personas provided")
    }
  )

  previous_stage <- tibble::tibble(
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

test_that("bid_interpret auto-suggests central_question when NULL", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle to find important metrics",
    theory = "Visual Hierarchies",
    evidence = "User feedback from interviews",
    target_audience = "Marketing team",
    suggestions = "Example suggestions",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(previous_stage, central_question = NULL)
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$central_question[1]))
  expect_true(nchar(result$central_question[1]) > 0)
  expect_match(
    result$central_question[1],
    "metrics|find|important",
    perl = TRUE
  )
})

test_that("bid_interpret auto-suggests data_story when NULL", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard has too many visualizations",
    theory = "Cognitive Load Theory",
    evidence = "User testing revealed confusion",
    target_audience = "Data analysts",
    suggestions = "Example suggestions",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage,
      central_question = "How can we simplify the dashboard?"
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$hook[1]))
  expect_false(is.na(result$context[1]))
  expect_false(is.na(result$tension[1]))
  expect_false(is.na(result$resolution[1]))
  expect_match(
    paste(result$hook[1], result$context[1], result$tension[1]),
    "visualizations|dashboard|confusion",
    perl = TRUE
  )
})

test_that("bid_interpret handles edge cases in data_story parameter", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      data_story = list(
        hook = "Test hook",
        context = "Test context"
        # intentionally missing tension and resolution
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$hook[1], "Test hook")
  expect_equal(result$context[1], "Test context")
  expect_true(is.na(result$tension[1]))
  expect_true(is.na(result$resolution[1]))

  suppressMessages(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      data_story = list(
        hook = "",
        context = "  ", # just whitespace
        tension = "Test tension",
        resolution = "Test resolution"
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$hook[1]) || result$hook[1] == "")
  expect_true(is.na(result$context[1]) || result$context[1] == "  ")

  suppressMessages(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      data_story = list(
        hook = "Test hook",
        context = "Test context",
        tension = "Test tension",
        resolution = "Test resolution",
        extra_field = "This is not a standard field",
        another_extra = "Another non-standard field"
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$hook[1], "Test hook")
  expect_equal(result$resolution[1], "Test resolution")
})

test_that("bid_interpret handles edge cases in user_personas parameter", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressWarnings(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      user_personas = list(
        list(name = "Minimal Persona")
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$user_personas[1]))

  suppressWarnings(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      user_personas = list()
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$user_personas[1]))

  suppressWarnings(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      user_personas = list(
        list(
          name = "Complete Persona",
          goals = "Test goals",
          pain_points = "Test pain points",
          technical_level = "Advanced"
        ),
        list(
          name = "Partial Persona",
          goals = "Test goals"
        )
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$user_personas[1]))
  expect_match(result$user_personas[1], "Complete Persona")
  expect_match(result$user_personas[1], "Partial Persona")
})

test_that("bid_interpret handles NA values in previous_stage", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = NA_character_, # NA problem
    theory = "Cognitive Load Theory",
    evidence = "Test evidence",
    target_audience = NA_character_, # NA audience
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_interpret(previous_stage)
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$central_question[1]))
  expect_true(is.na(result$previous_problem[1]))
  expect_true(is.na(result$previous_audience[1]))
})

test_that("bid_interpret validates user_personas structure correctly", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )


  expect_error(
    bid_interpret(
      previous_stage,
      central_question = "Test question",
      user_personas = list(
        list(
          # missing required persona 'name' field
          goals = "Test goals",
          pain_points = "Test pain points"
        )
      )
    )
  )

  # multiple personas with missing recommended fields should warn but not error
  suppressWarnings(
    result <- bid_interpret(
      previous_stage,
      central_question = "Test question",
      user_personas = list(
        list(name = "Persona 1"),
        list(name = "Persona 2", technical_level = "Advanced")
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$user_personas, "Persona 1", fixed = TRUE)
  expect_match(result$user_personas, "Persona 2", fixed = TRUE)
})
