library(testthat)
library(tibble)
library(cli)
library(stringr)

test_that("bid_notice returns a bid_stage object with correct structure", {
  result <- bid_notice(
    problem = "Users struggle to navigate cluttered dashboards",
    evidence = "User testing showed increased time to locate key metrics."
  )
  
  # Check S3 class
  expect_s3_class(result, "bid_stage")
  expect_s3_class(result, "tbl_df")
  
  # Check required columns
  expected_cols <- c(
    "stage", "problem", "theory", "evidence",
    "target_audience", "suggestions", "timestamp"
  )
  expect_equal(sort(names(result)), sort(expected_cols))
  expect_equal(result$stage, "Notice")
  
  # Check S3 methods work
  expect_equal(get_stage(result), "Notice")
  expect_type(get_metadata(result), "list")
})

test_that("bid_notice respects provided theory and doesn't auto-suggest", {
  result <- bid_notice(
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
  result <- bid_notice(
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

test_that("bid_notice records provided target audience correctly", {
  result <- bid_notice(
    problem = "Sales team struggles with complex filter combinations",
    evidence = "Training sessions revealed confusion with multiple selections",
    target_audience = "Sales representatives with varying technical skills"
  )
  expect_equal(
    result$target_audience,
    "Sales representatives with varying technical skills"
  )
  
  # Check metadata
  metadata <- get_metadata(result)
  expect_true(metadata$has_target_audience)
})

test_that("bid_notice returns NA for target audience when not provided", {
  result <- bid_notice(
    problem = "The chart is cluttered and confusing",
    evidence = "Feedback indicates users are disoriented."
  )
  expect_true(is.na(result$target_audience))
  
  # Check metadata
  metadata <- get_metadata(result)
  expect_false(metadata$has_target_audience)
})

test_that("bid_notice warns for short problem description", {
  expect_warning(
    bid_notice(problem = "Short", evidence = "Sufficient evidence provided."),
    "Problem description is very short"
  )
})

test_that("bid_notice warns for short evidence description", {
  expect_warning(
    bid_notice(
      problem = "A sufficiently detailed problem description",
      evidence = "Short"
    ),
    "Evidence description is very short"
  )
})

test_that("bid_notice errors with proper validation messages", {
  expect_error(
    bid_notice(problem = 123, evidence = "Valid evidence"),
    "'problem' must be a single character string"
  )
  
  expect_error(
    bid_notice(problem = "Valid problem", evidence = 456),
    "'evidence' must be a single character string"
  )
  
  expect_error(
    bid_notice(
      problem = "Valid problem",
      evidence = "Valid evidence",
      theory = 789
    ),
    "'theory' must be a single character string or NULL"
  )
  
  expect_error(
    bid_notice(
      problem = "Valid problem",
      evidence = "Valid evidence",
      target_audience = 101112
    ),
    "'target_audience' must be a single character string or NULL"
  )
})

test_that("bid_notice returns timestamp as a POSIXct object", {
  result <- bid_notice(
    problem = "A sufficiently detailed problem description.",
    evidence = "Evidence with enough detail for proper matching of theories."
  )
  expect_s3_class(result$timestamp, "POSIXct")
})

test_that("bid_notice suggests appropriate theory based on problem description", {
  result1 <- bid_notice(
    problem = "Users are overwhelmed with too many options in the dropdown",
    evidence = "User testing shows confusion"
  )
  expect_match(result1$theory, "Hick's Law", ignore.case = TRUE)

  result2 <- bid_notice(
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
  expect_error(
    bid_notice(problem = "", evidence = "Valid evidence"),
    "Problem cannot be empty or whitespace only"
  )

  expect_error(
    bid_notice(problem = "   ", evidence = "Valid evidence"),
    "Problem cannot be empty or whitespace only"
  )

  expect_error(
    bid_notice(problem = "Valid problem", evidence = ""),
    "Evidence cannot be empty or whitespace only"
  )

  expect_error(
    bid_notice(problem = NULL, evidence = "Valid evidence"),
    "Required parameter 'problem' must be provided"
  )

  expect_error(
    bid_notice(problem = "Valid problem", evidence = NULL),
    "Required parameter 'evidence' must be provided"
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

  result <- bid_notice(
    problem = long_problem,
    evidence = long_evidence
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$problem, long_problem)
  expect_equal(result$evidence, long_evidence)

  result <- bid_notice(
    problem = "UI is confusing",
    evidence = "Users report difficulties",
    theory = "My Custom Theory Framework"
  )

  expect_equal(result$theory, "My Custom Theory Framework")
})

test_that("bid_notice metadata contains expected information", {
  result <- bid_notice(
    problem = "Complex dashboard with many options",
    evidence = "User feedback indicates confusion and task abandonment"
  )
  
  metadata <- get_metadata(result)
  
  # Check required metadata fields
  expect_true("auto_suggested_theory" %in% names(metadata))
  expect_true("theory_confidence" %in% names(metadata))
  expect_true("problem_length" %in% names(metadata))
  expect_true("evidence_length" %in% names(metadata))
  expect_true("has_target_audience" %in% names(metadata))
  expect_true("validation_status" %in% names(metadata))
  expect_true("stage_number" %in% names(metadata))
  expect_true("total_stages" %in% names(metadata))
  expect_true("custom_mappings_used" %in% names(metadata))
  
  # Check values
  expect_equal(metadata$stage_number, 1)
  expect_equal(metadata$total_stages, 5)
  expect_equal(metadata$validation_status, "completed")
  expect_false(metadata$custom_mappings_used)
})

test_that("bid_notice print method works correctly", {
  result <- bid_notice(
    problem = "Users struggle with complex data visualization",
    evidence = "User testing revealed high task completion times",
    target_audience = "Data analysts"
  )
  
  # Test that print method executes without error
  expect_output(print(result), "BID Framework")
  expect_output(print(result), "Notice Stage")
  expect_output(print(result), "Problem:")
  expect_output(print(result), "Theory:")
  expect_output(print(result), "Evidence:")
  expect_output(print(result), "Target Audience:")
})

test_that("bid_notice summary method works correctly", {
  result <- bid_notice(
    problem = "Complex interface overwhelms users",
    evidence = "Analytics show high bounce rates"
  )
  
  # Test that summary method executes without error
  expect_output(summary(result), "BID Framework:")
  expect_output(summary(result), "Notice Stage Summary")
  expect_output(summary(result), "Metadata:")
})

test_that("bid_notice as_tibble method works correctly", {
  result <- bid_notice(
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
  # Test high-confidence match
  result1 <- bid_notice(
    problem = "Too many dropdown options overwhelm users",
    evidence = "Users abandon tasks when faced with many choices"
  )
  
  metadata1 <- get_metadata(result1)
  expect_true(metadata1$theory_confidence >= 0.8)  # Should be high confidence
  
  # Test lower confidence scenario
  result2 <- bid_notice(
    problem = "General usability issues",
    evidence = "Some user complaints"
  )
  
  metadata2 <- get_metadata(result2)
  expect_true(metadata2$theory_confidence > 0)  # Should have some confidence
})

test_that("bid_notice generates appropriate suggestions", {
  result <- bid_notice(
    problem = "Mobile interface is difficult to use",
    evidence = "Touch targets are too small"
  )
  
  # Should include mobile-specific suggestions
  expect_match(result$suggestions, "mobile|touch", ignore.case = TRUE)
  
  result2 <- bid_notice(
    problem = "Users struggle with too many choices",
    evidence = "Decision time is very long"
  )
  
  # Should include choice-related suggestions
  expect_match(result2$suggestions, "choice|decision|disclosure", ignore.case = TRUE)
})
