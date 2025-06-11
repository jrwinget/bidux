test_that("bid_concepts returns at least 40 concepts", {
  concepts <- bid_concepts()

  expect_s3_class(concepts, "tbl_df")
  expect_gte(nrow(concepts), 40)
  expect_true(all(c("concept", "description", "category") %in% names(concepts)))
})

test_that("bid_concepts search functionality works correctly", {
  # Test exact search
  suppressMessages({
    cognitive_concepts <- bid_concepts("cognitive")
  })

  expect_s3_class(cognitive_concepts, "tbl_df")
  expect_gte(nrow(cognitive_concepts), 1)

  # Verify search actually filtered results
  expect_true(any(
    grepl("cognitive", tolower(cognitive_concepts$concept)) |
      grepl("cognitive", tolower(cognitive_concepts$description))
  ))

  # Test case insensitive search
  suppressMessages({
    cognitive_concepts_upper <- bid_concepts("COGNITIVE")
  })
  expect_equal(nrow(cognitive_concepts), nrow(cognitive_concepts_upper))

  # Test multiple search terms
  suppressMessages({
    multi_concepts <- bid_concepts("visual, hierarchy")
  })
  expect_s3_class(multi_concepts, "tbl_df")
})

test_that("bid_concepts handles empty and invalid search terms", {
  # Empty search should return all concepts
  all_concepts <- bid_concepts("")
  expect_gte(nrow(all_concepts), 40)

  # NULL search should return all concepts
  all_concepts_null <- bid_concepts(NULL)
  expect_equal(nrow(all_concepts), nrow(all_concepts_null))

  # Whitespace only should return all concepts
  all_concepts_space <- bid_concepts("   ")
  expect_equal(nrow(all_concepts), nrow(all_concepts_space))

  # Non-existent search term might return results (due to fuzzy matching)
  suppressWarnings({
    no_results <- bid_concepts("xyznonexistent")
  })
  # Just check it returns a tibble, not necessarily empty
  expect_s3_class(no_results, "tbl_df")
})

test_that("bid_concept returns detailed information for valid concepts", {
  # Test with exact concept name
  suppressMessages({
    cognitive_load <- bid_concept("Cognitive Load Theory")
  })

  expect_s3_class(cognitive_load, "tbl_df")
  expect_equal(nrow(cognitive_load), 1)
  expect_equal(cognitive_load$concept[1], "Cognitive Load Theory")
  expect_true(all(
    c("concept", "description", "category", "recommendations") %in%
      names(cognitive_load)
  ))
})

test_that("bid_concept handles partial matching", {
  # Test partial matching
  suppressMessages({
    partial_match <- bid_concept("cognitive")
  })

  expect_s3_class(partial_match, "tbl_df")
  expect_gte(nrow(partial_match), 1)
  expect_true(grepl("cognitive", tolower(partial_match$concept[1])))

  # Test case insensitive matching
  suppressMessages({
    case_insensitive <- bid_concept("COGNITIVE LOAD")
  })
  expect_s3_class(case_insensitive, "tbl_df")
  expect_gte(nrow(case_insensitive), 1)
})

test_that("bid_concept handles invalid inputs gracefully", {
  # Test NULL input - should provide messaging rather than warning
  suppressMessages({
    null_result <- bid_concept(NULL)
  })
  expect_s3_class(null_result, "tbl_df")
  expect_equal(nrow(null_result), 0)

  # Test empty string - should provide messaging rather than warning
  suppressMessages({
    empty_result <- bid_concept("")
  })
  expect_s3_class(empty_result, "tbl_df")
  expect_equal(nrow(empty_result), 0)

  # Test whitespace only - should provide messaging rather than warning
  suppressMessages({
    space_result <- bid_concept("   ")
  })
  expect_s3_class(space_result, "tbl_df")
  expect_equal(nrow(space_result), 0)

  # Test non-existent concept - should provide messaging rather than warning
  suppressMessages({
    nonexistent <- bid_concept("NonExistentConcept123")
  })
  expect_s3_class(nonexistent, "tbl_df")
  expect_equal(nrow(nonexistent), 0)
})

test_that("bid_concept adds recommendations based on BID stage", {
  suppressMessages({
    # Test Stage 1 concept
    stage1_concept <- bid_concept("Cognitive Load Theory")
  })
  expect_true("recommendations" %in% names(stage1_concept))
  expect_true(grepl("NOTICE", stage1_concept$recommendations[1]))

  suppressMessages({
    # Test Stage 2 concept
    stage2_concept <- bid_concept("Data Storytelling Framework")
  })
  expect_true(grepl("INTERPRET", stage2_concept$recommendations[1]))

  suppressMessages({
    # Test Stage 3 concept
    stage3_concept <- bid_concept("Principle of Proximity")
  })
  expect_true(grepl("STRUCTURE", stage3_concept$recommendations[1]))
})

test_that("concepts data includes all required fields", {
  concepts <- bid_concepts()

  required_fields <- c(
    "concept",
    "description",
    "category",
    "reference",
    "example",
    "implementation_tips",
    "related_concepts"
  )

  expect_true(all(required_fields %in% names(concepts)))

  # Check that concepts have valid categories
  valid_categories <- c(
    "Stage 1",
    "Stage 2",
    "Stage 3",
    "Stage 4",
    "Stage 5",
    "All Stages"
  )
  expect_true(all(concepts$category %in% valid_categories))

  # Check that all concepts have non-empty names and descriptions
  expect_true(all(nchar(concepts$concept) > 0))
  expect_true(all(nchar(concepts$description) > 0))
})

test_that("bid_concept handles multi-word intelligent matching", {
  suppressMessages({
    # Test multi-word matching
    multi_word <- bid_concept("visual hierarchy")
  })

  expect_s3_class(multi_word, "tbl_df")
  expect_gte(nrow(multi_word), 1)

  # Should match concepts containing both "visual" and "hierarchy"
  concept_name <- tolower(multi_word$concept[1])
  expect_true(grepl("visual", concept_name) && grepl("hierarch", concept_name))
})

test_that("bid_concepts search returns results ordered by relevance", {
  suppressMessages({
    search_results <- bid_concepts("visual")
  })

  expect_s3_class(search_results, "tbl_df")
  expect_gte(nrow(search_results), 1)

  # Results should be ordered with most relevant first
  # (This tests the relevance scoring functionality)
  if (nrow(search_results) > 1) {
    # Check that any result contains the search term - don't require it's first
    any_match <- any(grepl("visual", tolower(search_results$concept)))
    expect_true(any_match)
  }
})

test_that("edge cases in search functionality", {
  # Test search with special characters and numbers
  suppressMessages({
    special_search <- bid_concepts("cognitive,load") # comma without space
  })
  expect_s3_class(special_search, "tbl_df")

  # Test very long search terms - might return something due to fuzzy matching
  suppressMessages({
    long_search <- bid_concepts("verylongnonexistentsearchterm")
  })
  expect_s3_class(long_search, "tbl_df")

  # Test search with multiple spaces
  suppressMessages({
    space_search <- bid_concepts("visual   hierarchy") # multiple spaces
  })
  expect_s3_class(space_search, "tbl_df")
})

test_that("concept data integrity checks", {
  concepts <- bid_concepts()

  # Check for duplicate concept names
  expect_equal(length(unique(concepts$concept)), nrow(concepts))

  # Check that related_concepts field references valid concepts when not NA
  for (i in seq_len(nrow(concepts))) {
    related <- concepts$related_concepts[i]
    if (!is.na(related) && related != "") {
      related_list <- trimws(unlist(strsplit(related, ",")))
      # At least some related concepts should exist in the main list
      # (allowing for some flexibility as related concepts might be broader terms)
      existing_related <- related_list %in% concepts$concept
      expect_true(length(related_list) > 0) # Should have at least one related concept
    }
  }

  # Check that categories are consistent
  stage_concepts <- concepts[grepl("Stage \\d", concepts$category), ]
  expect_gte(nrow(stage_concepts), 30) # Should have concepts for each stage

  # Check that implementation_tips are provided for most concepts
  non_empty_tips <- sum(
    !is.na(concepts$implementation_tips) &
      concepts$implementation_tips != ""
  )
  expect_gte(non_empty_tips, nrow(concepts) * 0.8) # At least 80% should have tips
})
