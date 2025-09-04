test_that("bid_interpret returns a tibble with stage 'Interpret'", {
  local_mocked_bindings(
    validate_required_params = function(...) invisible(TRUE),
    validate_previous_stage = function(...) invisible(TRUE),
    bid_message = function(...) invisible(NULL),
    safe_column_access = function(df, column_name, default = NA) {
      if (column_name %in% names(df)) df[[column_name]][1] else default
    },
    bid_stage = function(stage_name, data, metadata) {
      attr(data, "class") <- c("bid_stage", class(data))
      attr(data, "stage_name") <- stage_name
      attr(data, "metadata") <- metadata
      data
    }
  )

  # Test without previous_stage (new behavior)
  result <- bid_interpret(central_question = "How can we improve the test scenario?")

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_true("central_question" %in% names(result))
})

test_that("bid_interpret uses provided central_question", {
  local_mocked_bindings(
    validate_user_personas = function(personas) invisible(NULL),
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

  # Test with just central_question (no previous_stage needed)
  my_question <- "What is causing data complexity issues?"
  result <- bid_interpret(central_question = my_question)

  expect_equal(result$central_question[1], my_question)
})

test_that("bid_interpret errors when data_story is not a list", {
  expect_error(
    bid_interpret(data_story = "not a list"),
    regexp = "'data_story' must be a list"
  )
})

test_that("bid_interpret errors when personas is invalid", {
  local_mocked_bindings(
    validate_user_personas = function(personas) {
      cli::cli_abort("Invalid personas provided")
    }
  )

  expect_error(
    bid_interpret(
      central_question = "Test question",
      user_personas = "not a list"
    ),
    regexp = "Invalid personas provided"
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
  suppressMessages(
    result <- bid_interpret(
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
    "Dashboard users|maximum value|user experience|insights",
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

test_that("bid_interpret handles edge cases in personas parameter", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressWarnings(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = list(
        list(name = "Minimal Persona")
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$personas[1]))

  suppressWarnings(
    result <- bid_interpret(
      central_question = "Test question",
      user_personas = list()
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$personas[1]))

  suppressWarnings(
    result <- bid_interpret(
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
  expect_false(is.na(result$personas[1]))
  expect_match(result$personas[1], "Complete Persona")
  expect_match(result$personas[1], "Partial Persona")
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
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = "How can we handle missing data?"
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$central_question[1]))
  expect_true(is.na(result$previous_problem[1]))
  expect_true(is.na(result$previous_audience[1]))
})

test_that("bid_interpret validates personas structure correctly", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )

  expect_error(
    bid_interpret(
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
      central_question = "Test question",
      user_personas = list(
        list(name = "Persona 1"),
        list(name = "Persona 2", technical_level = "Advanced")
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$personas, "Persona 1", fixed = TRUE)
  expect_match(result$personas, "Persona 2", fixed = TRUE)
})

test_that("bid_interpret works with previous_stage for backward compatibility", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "Test results indicate delays",
    target_audience = "Data Analysts",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_interpret(
      previous_stage = previous_stage,
      central_question = "How can we address the data complexity?"
    )
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Interpret")
  expect_equal(result$central_question[1], "How can we address the data complexity?")
  expect_equal(result$previous_problem[1], "Users struggle with complex data")
})

test_that("bid_interpret generates central_question from different problem types", {
  # Test different problem patterns that trigger specific question generation
  problem_cases <- list(
    list(
      problem = "Users struggle with finding important metrics",
      expected_pattern = "simplify.*interface"
    ),
    list(
      problem = "Interface is very slow and takes too much time",
      expected_pattern = "speed.*efficiency"
    ),
    list(
      problem = "Too many options overwhelm users with excessive choices",
      expected_pattern = "cognitive load.*focus.*matters"
    ),
    list(
      problem = "Everything is very difficult and confusing",
      expected_pattern = "simplify.*interface"
    )
  )
  
  for (case in problem_cases) {
    previous_stage <- tibble::tibble(
      stage = "Notice",
      problem = case$problem,
      theory = "General Theory",
      evidence = "User feedback",
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_interpret(previous_stage, central_question = NULL)
    )
    
    expect_match(
      tolower(result$central_question[1]),
      case$expected_pattern,
      info = paste("Problem:", case$problem)
    )
  }
})

test_that("bid_interpret generates central_question considering theory", {
  theories <- list(
    list(
      theory = "Cognitive Load Theory",
      problem = "Users are overwhelmed",
      expected_pattern = "cognitive load"
    ),
    list(
      theory = "Hick's Law",
      problem = "Too many choices confuse users",
      expected_pattern = "simplify choices"
    ),
    list(
      theory = "Visual Hierarchy principles",
      problem = "Information layout is poor",
      expected_pattern = "visual hierarchy"
    )
  )
  
  for (case in theories) {
    previous_stage <- tibble::tibble(
      stage = "Notice",
      problem = case$problem,
      theory = case$theory,
      evidence = "Test evidence",
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_interpret(previous_stage, central_question = NULL)
    )
    
    expect_match(
      tolower(result$central_question[1]),
      case$expected_pattern,
      info = paste("Theory:", case$theory, "Problem:", case$problem)
    )
  }
})

test_that("bid_interpret handles different previous stage types", {
  # Test with Structure stage
  structure_stage <- tibble::tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_interpret(structure_stage, central_question = NULL)
  )
  
  expect_match(
    result$central_question[1],
    "refine.*understanding.*user needs"
  )
  
  # Test with Anticipate stage
  anticipate_stage <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: Test",
    accessibility = "Guidelines defined",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_interpret(anticipate_stage, central_question = NULL)
  )
  
  expect_match(
    result$central_question[1],
    "refine.*understanding.*user needs"
  )
})

test_that("bid_interpret creates proper user personas from audience information", {
  audience_cases <- list(
    list(
      audience = "Data Analyst team",
      expected_name = "Data Analyst Persona",
      expected_technical = "Advanced"
    ),
    list(
      audience = "Executive leadership group",
      expected_name = "Executive Persona",
      expected_technical = "Basic"
    ),
    list(
      audience = "Marketing professionals",
      expected_name = "Marketing Professional Persona",
      expected_technical = "Intermediate"
    ),
    list(
      audience = "Sales representatives",
      expected_name = "Sales Representative Persona",
      expected_technical = "Intermediate"
    ),
    list(
      audience = "End users and customers",
      expected_name = "End User Persona",
      expected_technical = "Intermediate"
    )
  )
  
  for (case in audience_cases) {
    previous_stage <- tibble::tibble(
      stage = "Notice",
      problem = "Test problem",
      theory = "Test theory",
      evidence = "Test evidence",
      target_audience = case$audience,
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_interpret(
        previous_stage,
        central_question = "Test question",
        data_story = list(audience = case$audience)
      )
    )
    
    expect_false(is.na(result$personas[1]))
    if (!is.na(result$personas[1])) {
      personas_json <- jsonlite::fromJSON(result$personas[1])
      if (length(personas_json) > 0 && !is.null(personas_json[[1]]$name)) {
        expect_match(personas_json[[1]]$name, case$expected_name)
        expect_equal(personas_json[[1]]$technical_level, case$expected_technical)
      }
    }
  }
})

test_that("bid_interpret handles metrics formatting edge cases", {
  # Test different metrics data types
  metrics_cases <- list(
    list(
      metrics = c("metric1", "metric2"),
      expected = "metric1, metric2"
    ),
    list(
      metrics = c(1, 2, 3),
      expected = "1, 2, 3"
    ),
    list(
      metrics = list("nested" = "value", "another" = "test"),
      expected = "value, test"
    )
  )
  
  for (case in metrics_cases) {
    data_story <- list(
      hook = "Test hook",
      context = "Test context",
      tension = "Test tension",
      resolution = "Test resolution",
      metrics = case$metrics
    )
    
    suppressMessages(
      result <- bid_interpret(
        central_question = "Test question",
        data_story = data_story
      )
    )
    
    expect_equal(result$metrics[1], case$expected)
  }
})

test_that("bid_interpret validates central_question format properly", {
  # Test question length validation and suggestions
  questions <- list(
    list(
      question = "How?",
      expected_suggestion = "benefit from more specificity"
    ),
    list(
      question = paste(rep("Very long question that exceeds the recommended character limit for central questions in the BID framework", 3), collapse = " "),
      expected_suggestion = "simplifying.*central question.*focus"
    ),
    list(
      question = "How can we improve user experience with better design?",
      expected_suggestion = "appropriately scoped"
    )
  )
  
  for (case in questions) {
    suppressMessages(
      result <- bid_interpret(
        central_question = case$question,
        data_story = list(
          hook = "Test",
          context = "Test",
          tension = "Test", 
          resolution = "Test"
        )
      )
    )
    
    expect_match(
      result$suggestions[1],
      case$expected_suggestion,
      info = paste("Question:", substr(case$question, 1, 50))
    )
  }
})

test_that("bid_interpret handles complex data story generation from previous stage", {
  previous_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard layout causes user confusion",
    theory = "Visual Hierarchy",
    evidence = "User testing shows high error rates",
    target_audience = "Marketing team",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_interpret(
      previous_stage,
      central_question = "How can we improve layout clarity?",
      data_story = NULL
    )
  )
  
  expect_false(is.na(result$hook[1]))
  expect_false(is.na(result$context[1]))
  expect_false(is.na(result$tension[1]))
  expect_false(is.na(result$resolution[1]))
  
  # Check that generated story incorporates previous stage information
  expect_match(result$hook[1], "Dashboard layout causes user confusion")
  expect_match(result$context[1], "User testing shows high error rates")
  expect_match(result$tension[1], "Visual Hierarchy")
  expect_match(result$resolution[1], "Visual Hierarchy")
  expect_equal(result$audience[1], "Marketing team")
})
