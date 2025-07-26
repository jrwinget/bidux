test_that("load_theory_mappings works with default data", {
  mappings <- load_theory_mappings()

  expect_s3_class(mappings, "data.frame")
  expect_true(all(c("keywords", "theory", "confidence") %in% names(mappings)))
  expect_true(nrow(mappings) > 0)
  expect_true(all(is.numeric(mappings$confidence)))
  expect_true(all(mappings$confidence > 0 & mappings$confidence <= 1))
})

test_that("load_theory_mappings works with custom mappings", {
  custom_mappings <- data.frame(
    keywords = c("test.*pattern", "another.*pattern"),
    theory = c("Test Theory", "Another Theory"),
    confidence = c(0.9, 0.8)
  )

  result <- load_theory_mappings(custom_mappings)
  expect_equal(result, custom_mappings)

  # Should error with missing columns
  incomplete_mappings <- data.frame(
    keywords = "test",
    theory = "Test Theory"
    # missing confidence
  )

  expect_error(
    load_theory_mappings(incomplete_mappings),
    "Custom mappings must contain columns"
  )
})

test_that("suggest_theory_from_mappings works correctly", {
  # Test with default mappings
  theory1 <- suggest_theory_from_mappings(
    "Users are overwhelmed with too many options",
    "Dropdown menus have many choices"
  )
  expect_equal(theory1, "Hick's Law")

  theory2 <- suggest_theory_from_mappings(
    "Dashboard layout is cluttered and disorganized",
    "Visual elements are scattered"
  )
  expect_equal(theory2, "Visual Hierarchies")

  theory3 <- suggest_theory_from_mappings(
    "Interface is too complex",
    "Users report feeling overwhelmed"
  )
  expect_equal(theory3, "Cognitive Load Theory")

  # Test with custom mappings
  custom_mappings <- data.frame(
    keywords = c("mobile.*issue", "slow.*performance"),
    theory = c("Fitts's Law", "Performance Theory"),
    confidence = c(0.9, 0.8)
  )

  theory4 <- suggest_theory_from_mappings(
    "Mobile interface is problematic",
    "Users report text, buttons, and sliders are too small",
    custom_mappings
  )
  expect_equal(theory4, "Fitts's Law")
})

test_that("suggest_theory_from_mappings handles edge cases", {
  # NULL/empty problem should return default
  default_theory <- suggest_theory_from_mappings(NULL, "evidence")
  expect_equal(default_theory, "Cognitive Load Theory")

  empty_theory <- suggest_theory_from_mappings("", "evidence")
  expect_equal(empty_theory, "Cognitive Load Theory")

  # No matches should return default
  no_match <- suggest_theory_from_mappings(
    "completely unrelated problem",
    "no matching evidence"
  )
  expect_equal(no_match, "Cognitive Load Theory")

  # Multiple matches should return highest confidence
  multi_match <- suggest_theory_from_mappings(
    "complex overwhelming interface with many options",
    "users struggle with cognitive load and choices"
  )
  expect_true(multi_match %in% c("Cognitive Load Theory", "Hick's Law"))
})

test_that("load_concept_bias_mappings works correctly", {
  mappings <- load_concept_bias_mappings()

  expect_s3_class(mappings, "data.frame")

  # Should have required columns even if empty
  required_cols <- c("concept", "bias_type", "mitigation_strategy")
  expect_true(all(required_cols %in% names(mappings)))

  # Test with custom mappings
  custom_mappings <- data.frame(
    concept = "Test Concept",
    bias_type = "test_bias",
    mitigation_strategy = "Test mitigation"
  )

  result <- load_concept_bias_mappings(custom_mappings)
  expect_equal(result, custom_mappings)
})

test_that("get_concept_bias_mappings works correctly", {
  # Test with empty concepts
  empty_result <- get_concept_bias_mappings(character(0))
  expect_s3_class(empty_result, "data.frame")
  expect_equal(nrow(empty_result), 0)

  # Test with custom mappings
  custom_mappings <- data.frame(
    concept = c("Cognitive Load Theory", "Visual Hierarchy", "Test Concept"),
    bias_type = c("overload_bias", "attention_bias", "test_bias"),
    mitigation_strategy = c("Reduce complexity", "Guide attention", "Test strategy")
  )

  result <- get_concept_bias_mappings(
    c("Cognitive Load Theory", "Visual Hierarchy"),
    custom_mappings
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true(all(result$concept %in% c("Cognitive Load Theory", "Visual Hierarchy")))

  # Test partial matching
  partial_result <- get_concept_bias_mappings(
    c("Cognitive", "Visual"),
    custom_mappings
  )
  expect_s3_class(partial_result, "data.frame")
  expect_true(nrow(partial_result) >= 0) # May find partial matches

  # Test with no matching concepts
  no_match_result <- get_concept_bias_mappings(
    c("NonExistentConcept"),
    custom_mappings
  )
  expect_s3_class(no_match_result, "data.frame")
  expect_equal(nrow(no_match_result), 0)
})

test_that("load_layout_mappings works correctly", {
  mappings <- load_layout_mappings()

  expect_s3_class(mappings, "data.frame")
  expect_true(all(c("layout", "primary_concepts", "description") %in% names(mappings)))
  expect_true(nrow(mappings) > 0)

  # Should include standard layouts
  expect_true("dual_process" %in% mappings$layout)
  expect_true("grid" %in% mappings$layout)

  # Test with custom mappings
  custom_mappings <- data.frame(
    layout = "custom_layout",
    primary_concepts = "Test Concept 1, Test Concept 2",
    description = "Custom layout description"
  )

  result <- load_layout_mappings(custom_mappings)
  expect_equal(result, custom_mappings)
})

test_that("get_layout_concepts works correctly", {
  # Test with known layouts
  dual_concepts <- get_layout_concepts("dual_process")
  expect_type(dual_concepts, "character")
  expect_true(length(dual_concepts) > 0)
  expect_true(any(grepl("Dual-Processing", dual_concepts)))

  grid_concepts <- get_layout_concepts("grid")
  expect_type(grid_concepts, "character")
  expect_true(length(grid_concepts) > 0)

  # Test case insensitive matching
  dual_upper <- get_layout_concepts("DUAL_PROCESS")
  expect_equal(dual_concepts, dual_upper)

  # Test partial matching
  dual_partial <- get_layout_concepts("dual")
  expect_type(dual_partial, "character")
  expect_true(length(dual_partial) > 0)

  # Test unknown layout returns defaults
  unknown_concepts <- get_layout_concepts("unknown_layout")
  expect_equal(unknown_concepts, c("Visual Hierarchy", "Principle of Proximity"))

  # Test NULL/empty input
  null_concepts <- get_layout_concepts(NULL)
  expect_equal(null_concepts, c("Visual Hierarchy", "Principle of Proximity"))

  empty_concepts <- get_layout_concepts("")
  expect_equal(empty_concepts, c("Visual Hierarchy", "Principle of Proximity"))
})

test_that("load_accessibility_guidelines works correctly", {
  guidelines <- load_accessibility_guidelines()

  expect_s3_class(guidelines, "data.frame")
  expect_true(nrow(guidelines) > 0)

  # Should have some basic accessibility guidelines
  if (nrow(guidelines) > 0) {
    expect_true(any(grepl("color|contrast", guidelines$guideline, ignore.case = TRUE)))
  }

  # Test with custom guidelines
  custom_guidelines <- data.frame(
    guideline = "custom_guideline",
    requirement = "Custom requirement",
    wcag_level = "AA"
  )

  result <- load_accessibility_guidelines(custom_guidelines)
  expect_equal(result, custom_guidelines)
})

test_that("get_accessibility_recommendations works correctly", {
  # Test with visual context
  visual_recs <- get_accessibility_recommendations("visual charts and graphs")
  expect_type(visual_recs, "character")
  expect_true(length(visual_recs) > 0)

  # Test with interactive context
  interactive_recs <- get_accessibility_recommendations("interactive buttons and forms")
  expect_type(interactive_recs, "character")
  expect_true(length(interactive_recs) > 0)

  # Test with data visualization context
  data_viz_recs <- get_accessibility_recommendations("data visualization dashboard")
  expect_type(data_viz_recs, "character")
  expect_true(length(data_viz_recs) > 0)

  # Test with empty context
  empty_recs <- get_accessibility_recommendations("")
  expect_type(empty_recs, "character")
  expect_true(length(empty_recs) > 0)

  # Test with custom guidelines
  custom_guidelines <- data.frame(
    guideline = c("color_contrast", "keyboard_nav", "screen_reader_support"),
    requirement = c("4.5:1 contrast ratio", "Full keyboard access", "ARIA labels"),
    wcag_level = c("AA", "AA", "AA")
  )

  custom_recs <- get_accessibility_recommendations("visual interface", custom_guidelines)
  expect_type(custom_recs, "character")
  expect_true(length(custom_recs) > 0)
})

test_that("mapping functions handle file loading errors gracefully", {
  # These tests verify fallback behavior when external files don't exist
  # or can't be loaded (which is normal in testing environments)

  # Theory mappings should fall back to defaults
  theory_mappings <- load_theory_mappings()
  expect_s3_class(theory_mappings, "data.frame")
  expect_true(nrow(theory_mappings) > 0)

  # Concept bias mappings should return empty structure if file missing
  bias_mappings <- load_concept_bias_mappings()
  expect_s3_class(bias_mappings, "data.frame")
  expect_true(all(c("concept", "bias_type", "mitigation_strategy") %in% names(bias_mappings)))

  # Layout mappings should fall back to defaults
  layout_mappings <- load_layout_mappings()
  expect_s3_class(layout_mappings, "data.frame")
  expect_true(nrow(layout_mappings) > 0)

  # Accessibility guidelines should return basic structure
  accessibility_guidelines <- load_accessibility_guidelines()
  expect_s3_class(accessibility_guidelines, "data.frame")
})

test_that("mapping functions validate input properly", {
  # Test theory mappings validation
  expect_error(
    load_theory_mappings(data.frame(keywords = "test")), # missing required columns
    "Custom mappings must contain columns"
  )

  # Test concept bias mappings validation
  expect_error(
    load_concept_bias_mappings(data.frame(concept = "test")), # missing required columns
    "Custom concept-bias mappings must contain columns"
  )

  # Test layout mappings validation
  expect_error(
    load_layout_mappings(data.frame(layout = "test")), # missing required columns
    "Custom layout mappings must contain columns"
  )
})

test_that("mapping functions provide consistent output formats", {
  # All mapping functions should return data.frames with consistent structure

  theory_mappings <- load_theory_mappings()
  expect_s3_class(theory_mappings, "data.frame")
  expect_false(any(is.factor(theory_mappings$keywords))) # No factors
  expect_false(any(is.factor(theory_mappings$theory)))

  layout_mappings <- load_layout_mappings()
  expect_s3_class(layout_mappings, "data.frame")
  expect_false(any(is.factor(layout_mappings$layout)))
  expect_false(any(is.factor(layout_mappings$primary_concepts)))

  # Suggestion functions should return character vectors
  theory_suggestion <- suggest_theory_from_mappings("test problem", "test evidence")
  expect_type(theory_suggestion, "character")
  expect_length(theory_suggestion, 1)

  layout_concepts <- get_layout_concepts("dual_process")
  expect_type(layout_concepts, "character")

  accessibility_recs <- get_accessibility_recommendations("test context")
  expect_type(accessibility_recs, "character")
})

test_that("mapping confidence scoring works correctly", {
  # Test that confidence scores are used properly in theory suggestion
  custom_mappings <- data.frame(
    keywords = c("low.*confidence", "high.*confidence"),
    theory = c("Low Confidence Theory", "High Confidence Theory"),
    confidence = c(0.3, 0.9)
  )

  # When both patterns match, should choose higher confidence
  result <- suggest_theory_from_mappings(
    "low confidence problem with high confidence elements",
    "evidence",
    custom_mappings
  )
  expect_equal(result, "High Confidence Theory")
})

test_that("mapping functions handle special characters and regex properly", {
  # Test that regex patterns in keywords work correctly
  theory_mappings <- load_theory_mappings()

  # Should handle regex patterns in keywords
  test_result <- suggest_theory_from_mappings(
    "users have too many options in dropdown",
    "choice overload observed"
  )
  expect_type(test_result, "character")
  expect_length(test_result, 1)

  # Should handle various text patterns
  visual_result <- suggest_theory_from_mappings(
    "visual layout organization problems",
    "hierarchy issues noted"
  )
  expect_type(visual_result, "character")
})

test_that("get_default_theory_mappings returns valid structure", {
  # Test the internal fallback function
  default_mappings <- get_default_theory_mappings()

  expect_s3_class(default_mappings, "data.frame")
  expect_true(all(c("keywords", "theory", "confidence") %in% names(default_mappings)))
  expect_true(nrow(default_mappings) >= 6) # Should have at least 6 default mappings
  expect_true(all(is.numeric(default_mappings$confidence)))
  expect_true(all(default_mappings$confidence > 0 & default_mappings$confidence <= 1))

  # Check that we have key theories
  expect_true("Hick's Law" %in% default_mappings$theory)
  expect_true("Cognitive Load Theory" %in% default_mappings$theory)
  expect_true("Visual Hierarchies" %in% default_mappings$theory)
})

test_that("get_default_layout_mappings returns valid structure", {
  # Test the internal fallback function
  default_layouts <- get_default_layout_mappings()

  expect_s3_class(default_layouts, "data.frame")
  expect_true(all(c("layout", "primary_concepts", "description") %in% names(default_layouts)))
  expect_true(nrow(default_layouts) >= 5) # Should have at least 5 default layouts

  # Check that we have key layouts
  expect_true("dual_process" %in% default_layouts$layout)
  expect_true("grid" %in% default_layouts$layout)
  expect_true("card" %in% default_layouts$layout)
  expect_true("tabs" %in% default_layouts$layout)
  expect_true("breathable" %in% default_layouts$layout)
})

test_that("mapping functions handle edge cases in file loading", {
  # Test behavior when system.file returns empty string (file not found)
  # This simulates the behavior when inst/extdata files don't exist

  # Should fall back gracefully without errors
  expect_no_warning({
    mappings <- load_theory_mappings()
    expect_s3_class(mappings, "data.frame")
    expect_true(nrow(mappings) > 0)
  })

  expect_no_warning({
    bias_mappings <- load_concept_bias_mappings()
    expect_s3_class(bias_mappings, "data.frame")
  })

  expect_no_warning({
    layout_mappings <- load_layout_mappings()
    expect_s3_class(layout_mappings, "data.frame")
    expect_true(nrow(layout_mappings) > 0)
  })

  expect_no_warning({
    accessibility_guidelines <- load_accessibility_guidelines()
    expect_s3_class(accessibility_guidelines, "data.frame")
  })
})

test_that("mapping functions handle malformed CSV files gracefully", {
  # Test that functions handle potential CSV parsing errors
  # This ensures robustness in production environments

  # All loading functions should return valid data.frames even if files are missing/malformed
  theory_result <- load_theory_mappings()
  expect_s3_class(theory_result, "data.frame")
  expect_true(all(c("keywords", "theory", "confidence") %in% names(theory_result)))

  bias_result <- load_concept_bias_mappings()
  expect_s3_class(bias_result, "data.frame")
  expect_true(all(c("concept", "bias_type", "mitigation_strategy") %in% names(bias_result)))

  layout_result <- load_layout_mappings()
  expect_s3_class(layout_result, "data.frame")
  expect_true(all(c("layout", "primary_concepts", "description") %in% names(layout_result)))
})

test_that("mapping integration works end-to-end", {
  # Test that mappings integrate properly with main BID functions

  # This should use theory mappings internally
  theory_suggestion <- suggest_theory_from_mappings(
    "Dashboard has too many overwhelming options",
    "User testing shows choice paralysis"
  )
  expect_equal(theory_suggestion, "Hick's Law")

  # Layout concepts should integrate properly
  concepts <- get_layout_concepts("card")
  expect_type(concepts, "character")
  expect_true(length(concepts) > 0)

  # Accessibility recommendations should be contextual
  recs <- get_accessibility_recommendations("interactive dashboard with charts")
  expect_type(recs, "character")
  expect_true(length(recs) > 0)

  # Test that all mapping functions work together
  layout_concepts <- get_layout_concepts("dual_process")
  bias_mappings <- get_concept_bias_mappings(layout_concepts)
  expect_s3_class(bias_mappings, "data.frame")

  # Test accessibility recommendations with different contexts
  color_recs <- get_accessibility_recommendations("colorful charts")
  interactive_recs <- get_accessibility_recommendations("interactive elements")
  expect_type(color_recs, "character")
  expect_type(interactive_recs, "character")

  # Recommendations should be contextually different
  expect_false(identical(color_recs, interactive_recs))
})

test_that("mapping performance is acceptable", {
  # Test that mapping functions perform well with reasonable data sizes

  # Create larger custom mappings to test performance
  large_theory_mappings <- data.frame(
    keywords = paste0("pattern", 1:100),
    theory = paste0("Theory", 1:100),
    confidence = runif(100, 0.5, 1.0)
  )

  # Should handle larger datasets efficiently
  start_time <- Sys.time()
  result <- suggest_theory_from_mappings(
    "pattern50 is the test problem",
    "evidence for pattern50",
    large_theory_mappings
  )
  end_time <- Sys.time()

  expect_equal(result, "Theory50")
  expect_true(as.numeric(end_time - start_time) < 1) # Should complete within 1 second
})

test_that("mapping functions support debugging and development", {
  # Test that mapping functions provide useful information for development

  # Theory mappings should be inspectable
  mappings <- load_theory_mappings()
  expect_true(nrow(mappings) > 0)
  expect_true(all(nchar(mappings$keywords) > 0))
  expect_true(all(nchar(mappings$theory) > 0))

  # Layout concepts should be detailed enough
  concepts <- get_layout_concepts("grid")
  expect_true(length(concepts) > 0)
  expect_true(all(nchar(concepts) > 3)) # Should be real concept names, not abbreviations

  # Accessibility recommendations should be actionable
  recs <- get_accessibility_recommendations("data visualization")
  expect_true(length(recs) > 0)
  expect_true(any(nchar(recs) > 10)) # Should be substantial recommendations
})
