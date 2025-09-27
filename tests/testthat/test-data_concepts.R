test_that("bid_concepts returns all concepts when no search term", {
  result <- bid_concepts()

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true(all(c("concept", "description", "category") %in% names(result)))
  expect_true(all(nchar(result$concept) > 0))
  expect_true(all(nchar(result$description) > 0))
})

test_that("bid_concepts filters by search term", {
  # Search for a specific concept
  result <- bid_concepts("proximity")

  expect_true(is.data.frame(result))
  expect_true(nrow(result) >= 1)
  expect_true(any(grepl("proximity", result$concept, ignore.case = TRUE)))

  # Search for non-existent concept
  result_empty <- bid_concepts("nonexistent_concept_xyz")
  expect_true(is.data.frame(result_empty))
  expect_equal(nrow(result_empty), 0)
})

test_that("bid_concepts fuzzy matching works", {
  # Test with fuzzy matching enabled (default)
  result_fuzzy <- bid_concepts("proximty", fuzzy_match = TRUE)  # intentional typo
  expect_true(is.data.frame(result_fuzzy))

  # Test with fuzzy matching disabled
  result_exact <- bid_concepts("proximty", fuzzy_match = FALSE)  # intentional typo
  expect_true(is.data.frame(result_exact))
  expect_equal(nrow(result_exact), 0)  # Should find nothing with exact match
})

test_that("bid_concepts max_distance parameter works", {
  # Test with different max_distance values
  result_strict <- bid_concepts("proximty", max_distance = 1)
  result_lenient <- bid_concepts("proximty", max_distance = 3)

  expect_true(is.data.frame(result_strict))
  expect_true(is.data.frame(result_lenient))
  expect_true(nrow(result_lenient) >= nrow(result_strict))
})

test_that("bid_concept returns specific concept details", {
  # Get all concepts first to find a valid one
  all_concepts <- bid_concepts()
  if (nrow(all_concepts) > 0) {
    test_concept <- all_concepts$concept[1]

    result <- bid_concept(test_concept)

    expect_true(is.list(result))
    expect_true("concept" %in% names(result))
    expect_true("description" %in% names(result))
    expect_equal(result$concept, test_concept)
    expect_true(nchar(result$description) > 0)
  }
})

test_that("bid_concept handles non-existent concepts", {
  result <- bid_concept("nonexistent_concept_xyz")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  expect_true(all(c("concept", "description", "category") %in% names(result)))
})

test_that("bid_concept recommendations parameter works", {
  all_concepts <- bid_concepts()
  if (nrow(all_concepts) > 0) {
    test_concept <- all_concepts$concept[1]

    # With recommendations
    result_with_rec <- bid_concept(test_concept, add_recommendations = TRUE)
    expect_true("recommendations" %in% names(result_with_rec) ||
                length(result_with_rec) > 2)

    # Without recommendations
    result_without_rec <- bid_concept(test_concept, add_recommendations = FALSE)
    expect_true(is.list(result_without_rec))
    expect_true(length(result_without_rec) >= 2)
  }
})

test_that("get_concepts_data returns valid structure", {
  concepts_data <- get_concepts_data()

  expect_true(is.data.frame(concepts_data))
  expect_true(nrow(concepts_data) > 0)
  expect_true(all(c("concept", "description", "category") %in% names(concepts_data)))

  # Test that all required fields are non-empty
  expect_true(all(nchar(trimws(concepts_data$concept)) > 0))
  expect_true(all(nchar(trimws(concepts_data$description)) > 0))
  expect_true(all(nchar(trimws(concepts_data$category)) > 0))
})

test_that("get_default_concepts_data returns fallback data", {
  default_data <- get_default_concepts_data()

  expect_true(is.data.frame(default_data))
  expect_true(nrow(default_data) > 0)
  expect_true(all(c("concept", "description", "category") %in% names(default_data)))

  # Should include some known basic concepts
  expect_true(any(grepl("visual", default_data$concept, ignore.case = TRUE)))
})

test_that("validate_concepts_data_structure works correctly", {
  # valid structure with all required columns
  valid_data <- data.frame(
    concept = c("test_concept1", "test_concept2"),
    description = c("Test description 1", "Test description 2"),
    category = c("test", "test"),
    reference = c("ref1", "ref2"),
    example = c("ex1", "ex2"),
    implementation_tips = c("tip1", "tip2"),
    related_concepts = c("rel1", "rel2"),
    stringsAsFactors = FALSE
  )

  expect_invisible(validate_concepts_data_structure(valid_data))

  # missing required columns
  invalid_data <- data.frame(
    concept = c("test1", "test2"),
    description = c("desc1", "desc2")
    # missing other required columns
  )

  expect_error(
    validate_concepts_data_structure(invalid_data),
    "missing required columns"
  )

  # empty data frame
  empty_data <- data.frame(
    concept = character(0),
    description = character(0),
    category = character(0),
    reference = character(0),
    example = character(0),
    implementation_tips = character(0),
    related_concepts = character(0)
  )

  expect_error(
    validate_concepts_data_structure(empty_data),
    "must have at least 1 row"
  )

  # non-data.frame input
  expect_error(
    validate_concepts_data_structure(list(concept = "test")),
    "must be a data.frame"
  )
})

test_that("get_fallback_concepts_data provides minimal concepts", {
  fallback_data <- get_fallback_concepts_data()

  expect_true(is.data.frame(fallback_data))
  expect_true(nrow(fallback_data) > 0)
  expect_true(all(c("concept", "description", "category") %in% names(fallback_data)))

  # should be valid according to structure validation
  expect_invisible(validate_concepts_data_structure(fallback_data))

  # Should include basic UI concepts
  concepts <- tolower(fallback_data$concept)
  expect_true(any(grepl("visual", concepts)))
})

test_that("concepts data loading handles errors gracefully", {
  # This tests the error handling in get_concepts_data when loading fails
  # We can't easily mock the file loading, but we can test the fallback behavior

  # the function should always return valid data, even if file loading fails
  result <- get_concepts_data()
  expect_true(is.data.frame(result))
  expect_invisible(validate_concepts_data_structure(result))
})

test_that("bid_concepts handles edge cases", {
  # Empty search string
  result_empty <- bid_concepts("")
  expect_true(is.data.frame(result_empty))

  # Search with special characters
  result_special <- bid_concepts("test!@#$%")
  expect_true(is.data.frame(result_special))

  # Very long search string
  long_search <- paste(rep("a", 1000), collapse = "")
  result_long <- bid_concepts(long_search)
  expect_true(is.data.frame(result_long))

  # NULL search (should work like no search term)
  result_null <- bid_concepts(NULL)
  expect_true(is.data.frame(result_null))
  expect_true(nrow(result_null) > 0)
})

test_that("bid_concept handles edge cases", {
  # Empty concept name
  result_empty <- bid_concept("")
  expect_true(is.list(result_empty))
  expect_true("concept" %in% names(result_empty))

  # Very long concept name
  long_name <- paste(rep("a", 1000), collapse = "")
  result_long <- bid_concept(long_name)
  expect_true(is.list(result_long))

  # Concept name with special characters
  result_special <- bid_concept("test!@#$%")
  expect_true(is.list(result_special))
})

test_that("concepts integration works correctly", {
  # Test that bid_concepts and bid_concept work together
  all_concepts <- bid_concepts()

  if (nrow(all_concepts) > 0) {
    # Pick a random concept
    test_concept <- all_concepts$concept[1]

    # Get details for that concept
    concept_details <- bid_concept(test_concept)

    expect_equal(concept_details$concept, test_concept)
    expect_true(nchar(concept_details$description) > 0)

    # The description should match what's in the concepts data
    matching_row <- all_concepts[all_concepts$concept == test_concept, ]
    if (nrow(matching_row) > 0) {
      expect_equal(concept_details$description, matching_row$description[1])
    }
  }
})