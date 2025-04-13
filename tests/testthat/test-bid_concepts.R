library(testthat)
library(tibble)

test_that("bid_concepts returns a tibble with expected columns", {
  result <- bid_concepts()
  
  expect_s3_class(result, "tbl_df")
  expected_cols <- c(
    "concept", "description", "category",
    "reference", "example", "implementation_tips"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("bid_concepts returns the full list when no search is provided", {
  result <- bid_concepts()
  expect_true(nrow(result) >= 41)
})

test_that("bid_concepts search argument filters results correctly", {
  result <- bid_concepts("cognitive")
  expect_true(all(grepl("cognitive", tolower(result$concept))))
})

test_that("bid_concepts search is case-insensitive", {
  result_upper <- bid_concepts("HICK")
  result_lower <- bid_concepts("hick")
  expect_equal(nrow(result_upper), nrow(result_lower))
})

test_that("bid_concepts returns an empty tibble if search term does not match any concept", {
  result <- bid_concepts("nonexistentconcept")
  expect_equal(nrow(result), 0)
})
