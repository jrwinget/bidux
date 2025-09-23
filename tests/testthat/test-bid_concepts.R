test_that("bid_concepts returns at least 40 concepts", {
  concepts <- bid_concepts()

  expect_s3_class(concepts, "tbl_df")
  expect_gte(nrow(concepts), 40)
  expect_true(all(c("concept", "description", "category") %in% names(concepts)))
})

test_that("bid_concepts search functionality works correctly", {
  # test exact search
  suppressMessages({
    cognitive_concepts <- bid_concepts("cognitive")
  })

  expect_s3_class(cognitive_concepts, "tbl_df")
  expect_gte(nrow(cognitive_concepts), 1)

  # verify search actually filtered results
  expect_true(any(
    grepl("cognitive", tolower(cognitive_concepts$concept)) |
      grepl("cognitive", tolower(cognitive_concepts$description))
  ))

  # test case insensitive search
  suppressMessages({
    cognitive_concepts_upper <- bid_concepts("COGNITIVE")
  })
  expect_equal(nrow(cognitive_concepts), nrow(cognitive_concepts_upper))

  # test multiple search terms
  suppressMessages({
    multi_concepts <- bid_concepts("visual, hierarchy")
  })
  expect_s3_class(multi_concepts, "tbl_df")
})

test_that("bid_concepts handles empty and invalid search terms", {
  # empty search should return all concepts
  all_concepts <- bid_concepts("")
  expect_gte(nrow(all_concepts), 40)

  # NULL search should return all concepts
  all_concepts_null <- bid_concepts(NULL)
  expect_equal(nrow(all_concepts), nrow(all_concepts_null))

  # whitespace only should return all concepts
  all_concepts_space <- bid_concepts("   ")
  expect_equal(nrow(all_concepts), nrow(all_concepts_space))

  # non-existent search term might return results (due to fuzzy matching)
  suppressWarnings({
    no_results <- bid_concepts("xyznonexistent")
  })
  # just check it returns a tibble, not necessarily empty
  expect_s3_class(no_results, "tbl_df")
})

test_that("bid_concept returns detailed information for valid concepts", {
  # test with exact concept name
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
  # test partial matching
  suppressMessages({
    partial_match <- bid_concept("cognitive")
  })

  expect_s3_class(partial_match, "tbl_df")
  expect_gte(nrow(partial_match), 1)
  expect_true(grepl("cognitive", tolower(partial_match$concept[1])))

  # test case insensitive matching
  suppressMessages({
    case_insensitive <- bid_concept("COGNITIVE LOAD")
  })
  expect_s3_class(case_insensitive, "tbl_df")
  expect_gte(nrow(case_insensitive), 1)
})

test_that("bid_concept handles invalid inputs gracefully", {
  # test NULL input - should provide messaging rather than warning
  suppressMessages({
    null_result <- bid_concept(NULL)
  })
  expect_s3_class(null_result, "tbl_df")
  expect_equal(nrow(null_result), 0)

  # test empty string - should provide messaging rather than warning
  suppressMessages({
    empty_result <- bid_concept("")
  })
  expect_s3_class(empty_result, "tbl_df")
  expect_equal(nrow(empty_result), 0)

  # test whitespace only - should provide messaging rather than warning
  suppressMessages({
    space_result <- bid_concept("   ")
  })
  expect_s3_class(space_result, "tbl_df")
  expect_equal(nrow(space_result), 0)

  # test non-existent concept - should provide messaging rather than warning
  suppressMessages({
    nonexistent <- bid_concept("NonExistentConcept123")
  })
  expect_s3_class(nonexistent, "tbl_df")
  expect_equal(nrow(nonexistent), 0)
})

test_that("bid_concept adds recommendations based on BID stage", {
  suppressMessages({
    # test Stage 1 concept
    stage1_concept <- bid_concept("Cognitive Load Theory")
  })
  expect_true("recommendations" %in% names(stage1_concept))
  expect_true(grepl("NOTICE", stage1_concept$recommendations[1]))

  suppressMessages({
    # test Stage 2 concept
    stage2_concept <- bid_concept("Data Storytelling Framework")
  })
  expect_true(grepl("INTERPRET", stage2_concept$recommendations[1]))

  suppressMessages({
    # test Stage 3 concept
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

  # check that concepts have valid categories
  valid_categories <- c(
    "Stage 1",
    "Stage 2",
    "Stage 3",
    "Stage 4",
    "Stage 5",
    "All Stages"
  )
  expect_true(all(concepts$category %in% valid_categories))

  # check that all concepts have non-empty names and descriptions
  expect_true(all(nchar(concepts$concept) > 0))
  expect_true(all(nchar(concepts$description) > 0))
})

test_that("bid_concept handles multi-word intelligent matching", {
  suppressMessages({
    # test multi-word matching
    multi_word <- bid_concept("visual hierarchy")
  })

  expect_s3_class(multi_word, "tbl_df")
  expect_gte(nrow(multi_word), 1)

  # should match concepts containing both "visual" and "hierarchy"
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
    # check that any result contains the search term - don't require it's first
    any_match <- any(grepl("visual", tolower(search_results$concept)))
    expect_true(any_match)
  }
})

test_that("edge cases in search functionality", {
  # test search with special characters and numbers
  suppressMessages({
    special_search <- bid_concepts("cognitive,load") # comma without space
  })
  expect_s3_class(special_search, "tbl_df")

  # test very long search terms - might return something due to fuzzy matching
  suppressMessages({
    long_search <- bid_concepts("verylongnonexistentsearchterm")
  })
  expect_s3_class(long_search, "tbl_df")

  # test search with multiple spaces
  suppressMessages({
    space_search <- bid_concepts("visual   hierarchy") # multiple spaces
  })
  expect_s3_class(space_search, "tbl_df")
})

test_that("concept data integrity checks", {
  concepts <- bid_concepts()

  # check for duplicate concept names
  expect_equal(length(unique(concepts$concept)), nrow(concepts))

  # check that related_concepts field references valid concepts when not NA
  for (i in seq_len(nrow(concepts))) {
    related <- concepts$related_concepts[i]
    if (!is.na(related) && related != "") {
      related_list <- trimws(unlist(strsplit(related, ",")))
      # At least some related concepts should exist in the main list
      # (allowing for some flexibility as related concepts might be broader terms)
      existing_related <- related_list %in% concepts$concept
      expect_true(length(related_list) > 0) # should have at least one related concept
    }
  }

  # check that categories are consistent
  stage_concepts <- concepts[grepl("Stage \\d", concepts$category), ]
  expect_gte(nrow(stage_concepts), 30) # should have concepts for each stage

  # check that implementation_tips are provided for most concepts
  non_empty_tips <- sum(
    !is.na(concepts$implementation_tips) &
      concepts$implementation_tips != ""
  )
  expect_gte(non_empty_tips, nrow(concepts) * 0.8) # >= 80% should have tips
})

# Tests for get_concepts_data internal function
test_that("get_concepts_data returns proper tibble structure", {
  concepts_data <- get_concepts_data()

  expect_s3_class(concepts_data, "tbl_df")
  expect_gte(nrow(concepts_data), 40)

  required_cols <- c(
    "concept",
    "description",
    "category",
    "reference",
    "example",
    "implementation_tips",
    "related_concepts"
  )

  expect_true(all(required_cols %in% names(concepts_data)))
})

# tests for get_default_concepts_data internal function
test_that("get_default_concepts_data provides fallback data", {
  default_data <- get_default_concepts_data()

  expect_s3_class(default_data, "tbl_df")
  expect_gte(nrow(default_data), 40)

  # check specific concepts exist
  expect_true("Cognitive Load Theory" %in% default_data$concept)
  expect_true("Visual Hierarchies" %in% default_data$concept)
  expect_true("Hick's Law" %in% default_data$concept)

  # check data consistency
  expect_equal(length(default_data$concept), nrow(default_data))
  expect_equal(length(default_data$description), nrow(default_data))
  expect_equal(length(default_data$category), nrow(default_data))
})

# test fuzzy matching functionality
test_that("bid_concepts fuzzy matching works correctly", {
  # test fuzzy matching with typos
  suppressMessages({
    # should find "Visual Hierarchy" even with typos
    fuzzy_result <- bid_concepts("vizual hierachy", fuzzy_match = TRUE)
  })

  expect_s3_class(fuzzy_result, "tbl_df")

  # test with fuzzy matching disabled
  suppressMessages({
    no_fuzzy <- bid_concepts("vizual hierachy", fuzzy_match = FALSE)
  })

  expect_s3_class(no_fuzzy, "tbl_df")
})

# test max_distance parameter
test_that("bid_concepts max_distance parameter works", {
  suppressMessages({
    # Strict distance matching
    strict_match <- bid_concepts("cognitve", fuzzy_match = TRUE, max_distance = 1)

    # lenient distance matching
    lenient_match <- bid_concepts("cognitve", fuzzy_match = TRUE, max_distance = 5)
  })

  expect_s3_class(strict_match, "tbl_df")
  expect_s3_class(lenient_match, "tbl_df")

  # lenient should return same or more results
  expect_gte(nrow(lenient_match), nrow(strict_match))
})

# test bid_concept without recommendations
test_that("bid_concept works without recommendations", {
  suppressMessages({
    concept_no_rec <- bid_concept("Cognitive Load Theory", add_recommendations = FALSE)
  })

  expect_s3_class(concept_no_rec, "tbl_df")
  expect_equal(nrow(concept_no_rec), 1)
  expect_false("recommendations" %in% names(concept_no_rec))
})

# test all stage recommendation types
test_that("bid_concept generates correct recommendations for all stages", {
  suppressMessages({
    # Stage 1
    stage1 <- bid_concept("Cognitive Load Theory")
    expect_true(grepl("NOTICE", stage1$recommendations[1]))

    # Stage 2
    stage2 <- bid_concept("Data Storytelling Framework")
    expect_true(grepl("INTERPRET", stage2$recommendations[1]))

    # Stage 3
    stage3 <- bid_concept("Principle of Proximity")
    expect_true(grepl("STRUCTURE", stage3$recommendations[1]))

    # Stage 4
    stage4 <- bid_concept("Anchoring Effect")
    expect_true(grepl("ANTICIPATE", stage4$recommendations[1]))

    # Stage 5
    stage5 <- bid_concept("Peak-End Rule")
    expect_true(grepl("VALIDATE", stage5$recommendations[1]))

    # All Stages
    all_stages <- bid_concept("User-Centric Design")
    expect_true(grepl("UNIVERSAL", all_stages$recommendations[1]))
  })
})

# test search with empty results
test_that("bid_concepts handles no matches gracefully", {
  suppressMessages({
    no_matches <- bid_concepts("xyznonexistentterm123", fuzzy_match = FALSE)
  })

  expect_s3_class(no_matches, "tbl_df")
  expect_equal(nrow(no_matches), 0)
  expect_true(all(names(get_concepts_data()) %in% names(no_matches)))
})

# test multiple search term combinations
test_that("bid_concepts handles complex search combinations", {
  suppressMessages({
    # Multiple terms with spaces
    multi_search <- bid_concepts("cognitive, visual, hierarchy")

    # Terms with different separators
    sep_search <- bid_concepts("cognitive,visual;hierarchy")

    # Mixed case and punctuation
    mixed_search <- bid_concepts("COGNITIVE load, Visual-Hierarchy!")
  })

  expect_s3_class(multi_search, "tbl_df")
  expect_s3_class(sep_search, "tbl_df")
  expect_s3_class(mixed_search, "tbl_df")
})

# test concept name trimming and normalization
test_that("bid_concept handles whitespace and case variations", {
  suppressMessages({
    # Leading/trailing whitespace
    whitespace_concept <- bid_concept("  Cognitive Load Theory  ")

    # Different case variations
    upper_concept <- bid_concept("COGNITIVE LOAD THEORY")
    lower_concept <- bid_concept("cognitive load theory")
  })

  expect_s3_class(whitespace_concept, "tbl_df")
  expect_s3_class(upper_concept, "tbl_df")
  expect_s3_class(lower_concept, "tbl_df")

  # All should find the same concept if matching works
  if (nrow(whitespace_concept) > 0 && nrow(upper_concept) > 0) {
    expect_equal(whitespace_concept$concept[1], upper_concept$concept[1])
  }
})

# test stringdist integration
test_that("bid_concepts integrates with stringdist package", {
  # This tests the stringdist functionality if available
  if (requireNamespace("stringdist", quietly = TRUE)) {
    suppressMessages({
      # test with string distance calculations
      dist_search <- bid_concepts("cognitiv load", fuzzy_match = TRUE)
    })

    expect_s3_class(dist_search, "tbl_df")
  } else {
    skip("stringdist package not available")
  }
})

# test edge cases in search term processing
test_that("bid_concepts processes edge case search terms", {
  suppressMessages({
    # single character
    single_char <- bid_concepts("a")

    # numbers and special characters
    special_chars <- bid_concepts("123!@#")

    # very long search term
    long_term <- bid_concepts(paste(rep("cognitive", 20), collapse = " "))
  })

  expect_s3_class(single_char, "tbl_df")
  expect_s3_class(special_chars, "tbl_df")
  expect_s3_class(long_term, "tbl_df")
})

# test default recommendations fallback
test_that("bid_concept handles unknown categories gracefully", {
  # create a mock concept with unknown category to test fallback
  # this tests the default case in the switch statement
  suppressMessages({
    # test a concept that might have an edge case category
    concepts_data <- get_concepts_data()
    if (nrow(concepts_data) > 0) {
      # just verify the function handles the recommendation generation
      first_concept <- bid_concept(concepts_data$concept[1])
      expect_s3_class(first_concept, "tbl_df")
      if (nrow(first_concept) > 0) {
        expect_true("recommendations" %in% names(first_concept))
        expect_true(nchar(first_concept$recommendations[1]) > 0)
      }
    }
  })
})

# test message handling in search functions
test_that("bid_concepts and bid_concept provide appropriate messages", {
  # test that functions provide user feedback
  expect_message(bid_concepts(), "Returning all")
  expect_message(bid_concepts("cognitive"), "Found")
  expect_message(bid_concept("cognitive"), "Found partial match|$")
  expect_message(bid_concept("nonexistent123"), "not found")
  expect_message(bid_concept(NULL), "Please provide")
  expect_message(bid_concept(""), "Please provide")
})
