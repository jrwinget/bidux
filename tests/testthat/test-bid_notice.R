library(testthat)
library(tibble)

test_that("bid_notice returns a bid_stage object with correct structure", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve dashboard navigation?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Users struggle to navigate cluttered dashboards",
    evidence = "User testing showed increased time to locate key metrics."
  )

  # Check S3 class
  expect_s3_class(result, "bid_stage")
  expect_s3_class(result, "bid_stage")

  # Check required columns
  expected_cols <- c(
    "stage",
    "problem",
    "theory",
    "evidence",
    "suggestions",
    "timestamp"
  )
  expect_equal(sort(names(result)), sort(expected_cols))
  expect_equal(result$stage, "Notice")

  # Check S3 methods work
  expect_equal(get_stage(result), "Notice")
  expect_type(get_metadata(result), "list")
})

test_that("bid_notice respects provided theory and doesn't auto-suggest", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve user experience?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Simple dashboard issue",
    evidence = "Users are confused by the interface layout",
    theory = "Custom Theory"
  )
  expect_equal(result$theory, "Custom Theory")

  # Check metadata reflects manual theory selection
  metadata <- get_metadata(result)
  expect_false(metadata$auto_suggested_theory)
})

test_that("bid_notice auto-suggests theory when not provided", {
  interpret_stage <- bid_interpret(
    central_question = "How can we reduce user overwhelm?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Users are overwhelmed with too many options in the dropdown",
    evidence = "User testing shows confusion"
  )

  # Should suggest a theory
  expect_false(is.na(result$theory))

  # Check metadata reflects auto-suggestion
  metadata <- get_metadata(result)
  expect_true(metadata$auto_suggested_theory)
  expect_type(metadata$theory_confidence, "double")
  expect_true(metadata$theory_confidence > 0 && metadata$theory_confidence <= 1)
})

test_that("bid_notice warns for deprecated target_audience parameter", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve sales team efficiency?"
  )
  
  expect_warning(
    result <- bid_notice(
      previous_stage = interpret_stage,
      problem = "Sales team struggles with complex filter combinations",
      evidence = "Training sessions revealed confusion with multiple selections",
      target_audience = "Sales representatives with varying technical skills"
    ),
    "target_audience.*deprecated"
  )
  # target_audience should not be in result columns
  expect_false("target_audience" %in% names(result))
})

test_that("bid_notice works correctly without target_audience", {
  interpret_stage <- bid_interpret(
    central_question = "How can we clarify chart design?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "The chart is cluttered and confusing",
    evidence = "Feedback indicates users are disoriented."
  )
  # target_audience should not be in columns at all
  expect_false("target_audience" %in% names(result))
})

test_that("bid_notice warns for short problem description", {
  expect_warning(
    {
      interpret_stage <- bid_interpret(central_question = "Test question?")
      bid_notice(
        previous_stage = interpret_stage,
        problem = "Short", 
        evidence = "Sufficient evidence provided."
      )
    },
    "Problem description is very short"
  )
})

test_that("bid_notice warns for short evidence description", {
  expect_warning(
    {
      interpret_stage <- bid_interpret(central_question = "Test question?")
      bid_notice(
        previous_stage = interpret_stage,
        problem = "A sufficiently detailed problem description",
        evidence = "Short"
      )
    },
    "Evidence description is very short"
  )
})

test_that("bid_notice errors with proper validation messages", {
  interpret_stage <- bid_interpret(central_question = "Test question?")
  
  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = 123, 
      evidence = "Valid evidence"
    ),
    "'problem' must be a single character string"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Valid problem", 
      evidence = 456
    ),
    "'evidence' must be a single character string"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Valid problem",
      evidence = "Valid evidence",
      theory = 789
    ),
    "'theory' must be a single character string"
  )

  # target_audience parameter is now deprecated and ignored with warning
  expect_warning(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Valid problem",
      evidence = "Valid evidence",
      target_audience = 101112
    ),
    "target_audience.*deprecated"
  )
})

test_that("bid_notice returns timestamp as a POSIXct object", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve user experience?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "A sufficiently detailed problem description.",
    evidence = "Evidence with enough detail for proper matching of theories."
  )
  expect_s3_class(result$timestamp, "POSIXct")
})

test_that("bid_notice suggests appropriate theory based on problem description", {
  interpret_stage <- bid_interpret(
    central_question = "How can we reduce user overwhelm?"
  )
  
  result1 <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Users are overwhelmed with too many options in the dropdown",
    evidence = "User testing shows confusion"
  )
  expect_match(result1$theory, "Hick's Law", ignore.case = TRUE)

  result2 <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Dashboard layout is cluttered and disorganized",
    evidence = "Users can't find important metrics"
  )
  expect_match(
    result2$theory,
    "Visual Hierarch|Cognitive Load",
    ignore.case = TRUE,
    perl = TRUE
  )
})

test_that("bid_notice handles empty strings and validates properly", {
  interpret_stage <- bid_interpret(central_question = "Test question?")
  
  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "", 
      evidence = "Valid evidence"
    ),
    "'problem' cannot be empty or contain only whitespace"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "   ", 
      evidence = "Valid evidence"
    ),
    "'problem' cannot be empty or contain only whitespace"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Valid problem", 
      evidence = ""
    ),
    "'evidence' cannot be empty or contain only whitespace"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = NULL, 
      evidence = "Valid evidence"
    ),
    "'problem' cannot be NULL"
  )

  expect_error(
    bid_notice(
      previous_stage = interpret_stage,
      problem = "Valid problem", 
      evidence = NULL
    ),
    "'evidence' cannot be NULL"
  )
})

test_that("bid_notice handles edge cases in optional parameters", {
  long_problem <- paste(
    rep("This is a very long problem description. ", 20),
    collapse = ""
  )

  long_evidence <- paste(
    rep("This is detailed evidence. ", 20),
    collapse = ""
  )

  interpret_stage <- bid_interpret(
    central_question = "How can we improve user experience?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = long_problem,
    evidence = long_evidence
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$problem, long_problem)
  expect_equal(result$evidence, long_evidence)

  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "UI is confusing",
    evidence = "Users report difficulties",
    theory = "My Custom Theory Framework"
  )

  expect_equal(result$theory, "My Custom Theory Framework")
})

test_that("bid_notice metadata contains expected information", {
  interpret_stage <- bid_interpret(
    central_question = "How can we simplify complex dashboards?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Complex dashboard with many options",
    evidence = "User feedback indicates confusion and task abandonment"
  )

  metadata <- get_metadata(result)

  # Check required metadata fields
  expect_true("auto_suggested_theory" %in% names(metadata))
  expect_true("theory_confidence" %in% names(metadata))
  expect_true("problem_length" %in% names(metadata))
  expect_true("evidence_length" %in% names(metadata))
  # has_target_audience is no longer in metadata since target_audience was removed
  expect_true("validation_status" %in% names(metadata))
  expect_true("stage_number" %in% names(metadata))
  expect_true("total_stages" %in% names(metadata))
  expect_true("custom_mappings_used" %in% names(metadata))

  # Check values
  expect_equal(metadata$stage_number, 2)
  expect_equal(metadata$total_stages, 5)
  expect_equal(metadata$validation_status, "completed")
  expect_false(metadata$custom_mappings_used)
})

test_that("bid_notice print method works correctly", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve data visualization?"
  )
  
  suppressWarnings(
    result <- bid_notice(
      previous_stage = interpret_stage,
      problem = "Users struggle with complex data visualization",
      evidence = "User testing revealed high task completion times",
      target_audience = "Data analysts"
    )
  )

  # Test that print method executes without error
  expect_output(print(result), "BID Framework")
  expect_output(print(result), "Notice Stage")
  expect_output(print(result), "Problem:")
  expect_output(print(result), "Theory:")
  expect_output(print(result), "Evidence:")
  # Target Audience is no longer displayed since it's been removed
})

test_that("bid_notice summary method works correctly", {
  interpret_stage <- bid_interpret(
    central_question = "How can we reduce interface complexity?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Complex interface overwhelms users",
    evidence = "Analytics show high bounce rates"
  )

  # Test that summary method executes without error
  expect_output(summary(result), "BID Framework:")
  expect_output(summary(result), "Notice Stage Summary")
  expect_output(summary(result), "Metadata:")
})

test_that("bid_notice as_tibble method works correctly", {
  interpret_stage <- bid_interpret(
    central_question = "How can we address interface complexity?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Interface complexity issue",
    evidence = "User research indicates problems"
  )

  # Convert to tibble
  tibble_result <- as_tibble(result)

  # Should be a regular tibble without bid_stage class
  expect_s3_class(tibble_result, "tbl_df")
  expect_false(inherits(tibble_result, "bid_stage"))

  # Should have same data
  expect_equal(names(tibble_result), names(result))
  expect_equal(nrow(tibble_result), nrow(result))
})

test_that("bid_notice theory confidence scoring works", {
  interpret_stage <- bid_interpret(
    central_question = "How can we reduce user overwhelm?"
  )
  
  # Test high-confidence match
  result1 <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Too many dropdown options overwhelm users",
    evidence = "Users abandon tasks when faced with many choices"
  )

  metadata1 <- get_metadata(result1)
  expect_true(metadata1$theory_confidence >= 0.8) # Should be high confidence

  # Test lower confidence scenario
  result2 <- bid_notice(
    previous_stage = interpret_stage,
    problem = "General usability issues",
    evidence = "Some user complaints"
  )

  metadata2 <- get_metadata(result2)
  expect_true(metadata2$theory_confidence > 0) # Should have some confidence
})

test_that("bid_notice generates appropriate suggestions", {
  interpret_stage <- bid_interpret(
    central_question = "How can we improve mobile interface usability?"
  )
  
  result <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Mobile interface is difficult to use",
    evidence = "Touch targets are too small"
  )

  # Should generate relevant suggestions
  expect_true(nchar(result$suggestions) > 0)
  expect_match(
    result$suggestions,
    "target audience|design solutions",
    ignore.case = TRUE
  )

  result2 <- bid_notice(
    previous_stage = interpret_stage,
    problem = "Users struggle with too many choices",
    evidence = "Decision time is very long"
  )

  # Should generate relevant suggestions
  expect_true(nchar(result2$suggestions) > 0)
  expect_match(
    result2$suggestions,
    "target audience|design solutions",
    ignore.case = TRUE
  )
})

test_that("bid_notice requires previous_stage parameter", {
  expect_error(
    bid_notice(
      problem = "Users struggle with navigation",
      evidence = "User testing shows confusion"
    ),
    "previous_stage"
  )
})
