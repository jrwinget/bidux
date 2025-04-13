library(testthat)
library(tibble)
library(cli)
library(stringr)

validate_required_params <- function(...) {}
bid_message <- function(...) {}

bid_concept <- function(theory) {
  known_theories <- c(
    "Cognitive Load Theory", "Visual Hierarchies", "Hick's Law",
    "Information Scent", "Pre-attentive Processing", "Fitts's Law",
    "Miller's Law", "Gestalt Principles", "Principle of Proximity"
  )
  if (theory %in% known_theories) {
    return(tibble(implementation_tips = paste("Tip for", theory)))
  } else {
    return(tibble())
  }
}

test_that("bid_notice returns a tibble with correct columns and stage", {
  result <- bid_notice(
    problem = "Users struggle to navigate cluttered dashboards",
    evidence = "User testing showed increased time to locate key metrics."
  )
  expect_s3_class(result, "tbl_df")
  
  expected_cols <- c(
    "stage", "problem", "theory", "evidence", 
    "target_audience", "suggestions", "timestamp"
  )

  expect_equal(sort(names(result)), sort(expected_cols))
  
  expect_equal(result$stage, "Notice")
})

test_that("bid_notice respects provided theory", {
  result <- bid_notice(
    problem = "Simple dashboard issue",
    evidence = "Users are confused by the interface layout",
    theory = "Custom Theory"
  )
  expect_equal(result$theory, "Custom Theory")
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
})

test_that("bid_notice returns NA for target audience when not provided", {
  result <- bid_notice(
    problem = "The chart is cluttered and confusing",
    evidence = "Feedback indicates users are disoriented."
  )
  expect_true(is.na(result$target_audience))
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

test_that("bid_notice errors if problem is not a character", {
  expect_error(
    bid_notice(problem = 123, evidence = "Valid evidence"),
    "Problem must be a character string"
  )
})

test_that("bid_notice errors if evidence is not a character", {
  expect_error(
    bid_notice(problem = "Valid problem", evidence = 456),
    "Evidence must be a character string"
  )
})

test_that("bid_notice errors if theory is provided and is not a character", {
  expect_error(
    bid_notice(
      problem = "Valid problem",
      evidence = "Valid evidence",
      theory = 789
    ),
    "Theory must be a character string"
  )
})

test_that(
  "bid_notice errors if target_audience is provided and is not a character", {
  expect_error(
    bid_notice(
      problem = "Valid problem",
      evidence = "Valid evidence",
      target_audience = 101112
    ),
    "Target audience must be a character string"
  )
})

test_that("bid_notice returns timestamp as a POSIXct object", {
  result <- bid_notice(
    problem = "A sufficiently detailed problem description.",
    evidence = "Evidence with enough detail for proper matching of theories."
  )
  expect_s3_class(result$timestamp, "POSIXct")
})
