test_that("bid_concepts returns complete concept data", {
  concepts <- bid_concepts()
  
  # Check structure
  expect_s3_class(concepts, "tbl_df")
  expect_true(all(c("concept", "description", "category", "reference", "example", "implementation_tips") %in% names(concepts)))
  
  # Check content
  expect_gt(nrow(concepts), 10)
  expect_true("Cognitive Load Theory" %in% concepts$concept)
  expect_true("Hick's Law" %in% concepts$concept)
})

test_that("bid_concepts search works case-insensitively", {
  cognitive_concepts <- bid_concepts("cognitive")
  
  expect_gt(nrow(cognitive_concepts), 0)
  expect_true(any(grepl("cognitive", tolower(cognitive_concepts$concept))))
  expect_true(any(grepl("cognitive", tolower(cognitive_concepts$description))))
})

test_that("bid_concept returns specific concept data", {
  hicks_law <- bid_concept("Hick's Law")
  
  expect_s3_class(hicks_law, "tbl_df")
  expect_equal(nrow(hicks_law), 1)
  expect_equal(hicks_law$concept, "Hick's Law")
  expect_true(!is.na(hicks_law$description))
})

test_that("bid_concept performs case-insensitive lookup", {
  hicks_law_lower <- bid_concept("hick's law")
  
  expect_s3_class(hicks_law_lower, "tbl_df")
  expect_equal(nrow(hicks_law_lower), 1)
  expect_equal(hicks_law_lower$concept, "Hick's Law")
})

test_that("bid_concept handles partial matches", {
  process_fluency <- bid_concept("processing")
  
  expect_s3_class(process_fluency, "tbl_df")
  expect_true(grepl("Processing", process_fluency$concept))
})

test_that("bid_concept suggests alternatives for non-existent concepts", {
  # Invalid concept name
  expect_message(result <- bid_concept("nonexistent concept"), "Did you mean")
  expect_null(result)
  
  # Check suggestions attribute
  suggestions <- attr(result, "suggestions")
  expect_s3_class(suggestions, "tbl_df")
  expect_gt(nrow(suggestions), 0)
})
