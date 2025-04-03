test_that("bid_notice works with all parameters", {
  result <- bid_notice(
    problem = "Users struggle with too many options",
    theory = "Hick's Law",
    evidence = "Testing shows users take 40+ seconds to make selections"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Notice")
  expect_equal(result$problem, "Users struggle with too many options")
  expect_equal(result$theory, "Hick's Law")
  expect_equal(result$evidence, "Testing shows users take 40+ seconds to make selections")
  expect_true(!is.na(result$suggestions))
  expect_true(!is.na(result$timestamp))
})

test_that("bid_notice auto-suggests theory when not provided", {
  result <- bid_notice(
    problem = "Dashboard is cluttered with too many visualizations",
    evidence = "User feedback indicates confusion about where to look first"
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Notice")
  expect_true(!is.na(result$theory))
  expect_message(
    bid_notice(
      problem = "Dashboard is cluttered with too many visualizations",
      evidence = "User feedback indicates confusion about where to look first"
    ),
    "suggested theory"
  )
})

test_that("bid_notice fails with missing parameters", {
  expect_error(bid_notice(problem = "Test"), "must be provided")
  expect_error(bid_notice(evidence = "Test"), "must be provided")
})

test_that("bid_notice matches appropriate theories to problems", {
  # Test Hick's Law matching
  hicks_result <- bid_notice(
    problem = "Too many dropdown options",
    evidence = "Users struggle to choose from list"
  )
  expect_match(hicks_result$theory, "Hick", ignore.case = TRUE)
  
  # Test Cognitive Load matching
  cog_load_result <- bid_notice(
    problem = "Users feel overwhelmed by complex interface", 
    evidence = "Reported mental effort is high"
  )
  expect_match(cog_load_result$theory, "Cognitive", ignore.case = TRUE)
  
  # Test Visual Hierarchies matching
  visual_result <- bid_notice(
    problem = "Important data isn't getting noticed",
    evidence = "Key metrics are overlooked in the layout"
  )
  expect_match(visual_result$theory, "Visual|Hierarch", ignore.case = TRUE)
})
