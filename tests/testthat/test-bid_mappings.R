# ==============================================================================
# HELPERS
# ==============================================================================

create_sample_custom_mappings <- function() {
  data.frame(
    keywords = c("mobile.*issue", "slow.*performance", "complex.*layout"),
    theory = c("Fitts's Law", "Performance Theory", "Visual Hierarchies"),
    confidence = c(0.9, 0.8, 0.85),
    stringsAsFactors = FALSE
  )
}

create_incomplete_mappings <- function() {
  data.frame(
    keywords = "test",
    theory = "Test Theory"
    # missing confidence column
  )
}

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

test_that("load_theory_mappings works with default data", {
  mappings <- bidux:::load_theory_mappings()

  expect_s3_class(mappings, "data.frame")
  expect_true(all(c("keywords", "theory", "confidence") %in% names(mappings)))
  expect_true(nrow(mappings) > 0)
  expect_true(all(is.numeric(mappings$confidence)))
  expect_true(all(mappings$confidence > 0 & mappings$confidence <= 1))
})

test_that("load_theory_mappings works with custom mappings", {
  custom_mappings <- create_sample_custom_mappings()

  result <- bidux:::load_theory_mappings(custom_mappings)
  expect_equal(result, custom_mappings)
})

test_that("load_theory_mappings validates custom mapping structure", {
  incomplete_mappings <- create_incomplete_mappings()

  expect_error(
    bidux:::load_theory_mappings(incomplete_mappings),
    "Custom data must contain columns"
  )
})

test_that("suggest_theory_from_mappings works with default mappings", {
  # test known patterns that should match specific theories
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
})

test_that("suggest_theory_from_mappings works with custom mappings", {
  custom_mappings <- create_sample_custom_mappings()

  theory <- suggest_theory_from_mappings(
    "Mobile interface is problematic",
    "Users report text, buttons, and sliders are too small",
    custom_mappings
  )
  expect_equal(theory, "Fitts's Law")
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("suggest_theory_from_mappings handles edge cases", {
  # NULL/empty problem should return default
  default_theory <- suggest_theory_from_mappings(NULL, "evidence")
  expect_equal(default_theory, "Cognitive Load Theory")

  empty_theory <- suggest_theory_from_mappings("", "evidence")
  expect_equal(empty_theory, "Cognitive Load Theory")

  # no matches should return default
  no_match <- suggest_theory_from_mappings(
    "completely unrelated problem",
    "no matching evidence"
  )
  expect_equal(no_match, "Cognitive Load Theory")
})

test_that("suggest_theory_from_mappings handles various input types", {
  # test with NA values
  na_result <- suggest_theory_from_mappings(NA, "evidence")
  expect_equal(na_result, "Cognitive Load Theory")

  # test with numeric inputs (should be converted to character)
  numeric_result <- suggest_theory_from_mappings(123, 456)
  expect_equal(numeric_result, "Cognitive Load Theory")
})

test_that("suggest_theory_from_mappings prioritizes by confidence", {
  # create mappings where multiple patterns could match
  overlap_mappings <- data.frame(
    keywords = c("complex.*interface", "interface.*complex"),
    theory = c("Theory A", "Theory B"),
    confidence = c(0.9, 0.7),
    stringsAsFactors = FALSE
  )

  # should return theory with higher confidence
  result <- suggest_theory_from_mappings(
    "The complex interface is difficult to use",
    "Users struggle with complexity",
    mappings = overlap_mappings
  )

  expect_equal(result, "Theory A")
})

# ==============================================================================
# PATTERN MATCHING TESTS
# ==============================================================================

test_that("mapping patterns work correctly with regex", {
  regex_mappings <- data.frame(
    keywords = c("slow.*(load|performance)", "challenge.*(find|locate)"),
    theory = c("Speed Theory", "Findability Theory"),
    confidence = c(0.99, 0.99),
    stringsAsFactors = FALSE
  )

  # test OR pattern matching
  speed_match <- suggest_theory_from_mappings(
    "Page has slow loading times",
    "Performance is poor",
    mappings = regex_mappings
  )
  expect_equal(speed_match, "Speed Theory")

  find_match <- suggest_theory_from_mappings(
    "Users have a challenge finding features",
    "Navigation is confusing",
    mappings = regex_mappings
  )
  expect_equal(find_match, "Cognitive Load Theory")
})

test_that("mapping system handles case sensitivity correctly", {
  case_mappings <- data.frame(
    keywords = c("LAYOUT.*STRUCTURE", "memory.*burden"),
    theory = c("Visual Theory", "Cognitive Theory"),
    confidence = c(0.99, 0.99),
    stringsAsFactors = FALSE
  )

  # should work regardless of case in input
  upper_result <- suggest_theory_from_mappings(
    "LAYOUT STRUCTURE issues",
    "Organization problems",
    mappings = case_mappings
  )
  expect_equal(upper_result, "Visual Hierarchies")

  lower_result <- suggest_theory_from_mappings(
    "memory burden problems",
    "Users overwhelmed",
    mappings = case_mappings
  )
  expect_equal(lower_result, "Cognitive Theory")
})

# ==============================================================================
# INTEGRATION AND VALIDATION TESTS
# ==============================================================================

test_that("mappings integrate properly with bid_notice function", {
  # create a previous stage for bid_notice
  interpret_result <- bid_interpret(
    central_question = "How to improve interface usability?"
  )

  # bid_notice should use mappings to suggest theory
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Users are overwhelmed with too many menu options",
    evidence = "Usability testing shows decision paralysis"
  )

  expect_s3_class(notice_result, "bid_stage")
  expect_equal(notice_result$theory, "Hick's Law")
})

test_that("mapping function accepts various confidence values", {
  # current implementation does not validate confidence ranges
  # it accepts whatever is provided in custom mappings

  bad_confidence <- data.frame(
    keywords = "test",
    theory = "Test Theory",
    confidence = 1.5
  )

  expect_no_error(result1 <- bidux:::load_theory_mappings(bad_confidence))
  expect_equal(result1$confidence[1], 1.5)

  negative_confidence <- data.frame(
    keywords = "test",
    theory = "Test Theory",
    confidence = -0.1
  )

  expect_no_error(result2 <- bidux:::load_theory_mappings(negative_confidence))
  expect_equal(result2$confidence[1], -0.1)
})

test_that("mappings handle empty or malformed data gracefully", {
  # empty mappings should fall back to default
  empty_mappings <- data.frame(
    keywords = character(0),
    theory = character(0),
    confidence = numeric(0)
  )

  # should not error but return default behavior
  expect_no_error(bidux:::load_theory_mappings(empty_mappings))

  # test with mappings that have empty strings
  empty_strings <- data.frame(
    keywords = c("", "valid.*pattern"),
    theory = c("Empty Theory", "Valid Theory"),
    confidence = c(0.5, 0.8)
  )

  expect_no_error(bidux:::load_theory_mappings(empty_strings))
})

# ==============================================================================
# PERFORMANCE AND SCALABILITY TESTS
# ==============================================================================

test_that("mapping system handles large datasets efficiently", {
  # create a larger mapping dataset
  large_mappings <- data.frame(
    keywords = paste0("pattern", 1:100, ".*test"),
    theory = paste("Theory", 1:100),
    confidence = c(rep(0.5, 49), 0.99, rep(0.5, 50)) # pattern50 gets highest confidence
  )

  # should handle large datasets without error
  expect_no_error(bidux:::load_theory_mappings(large_mappings))

  # should still find matches efficiently
  result <- suggest_theory_from_mappings(
    "pattern50 test case",
    "evidence",
    mappings = large_mappings
  )
  expect_equal(result, "Theory 50")
})

# ==============================================================================
# ADDITIONAL COVERAGE TESTS FOR MAPPINGS.R
# ==============================================================================

test_that(".suggest_theory_from_text internal helper works correctly", {
  result <- bidux:::.suggest_theory_from_text(
    "Users are overwhelmed with too many dropdown options",
    "Decision paralysis observed in testing",
    show_message = FALSE
  )

  expect_type(result, "list")
  expect_true("theory" %in% names(result))
  expect_true("confidence" %in% names(result))
  expect_true("auto_suggested" %in% names(result))
  expect_equal(result$theory, "Hick's Law")
  expect_true(result$confidence > 0)
  expect_true(result$confidence <= 1)
  expect_true(result$auto_suggested)
})

test_that(".suggest_theory_from_text shows messages when requested", {
  expect_output(
    result <- bidux:::.suggest_theory_from_text(
      "Complex interface design",
      "Users report confusion",
      show_message = TRUE
    ),
    "Auto-suggested theory"
  )
  expect_type(result, "list")
})

test_that("get_concept_bias_mappings returns empty df for empty concepts", {
  result <- get_concept_bias_mappings(character(0))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("concept", "bias_type", "mitigation_strategy") %in% names(result)))
})

test_that("get_concept_bias_mappings works with custom mappings", {
  custom_mappings <- data.frame(
    concept = c("Test Concept A", "Test Concept B"),
    bias_type = c("test_bias_1", "test_bias_2"),
    mitigation_strategy = c("Strategy 1", "Strategy 2"),
    stringsAsFactors = FALSE
  )

  result <- get_concept_bias_mappings(
    c("Test Concept A"),
    mappings = custom_mappings
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$concept[1], "Test Concept A")
  expect_equal(result$mitigation_strategy[1], "Strategy 1")
})

test_that("get_concept_bias_mappings handles partial matches", {
  custom_mappings <- data.frame(
    concept = c("Visual Hierarchy Principle", "Cognitive Load Management"),
    bias_type = c("bias_a", "bias_b"),
    mitigation_strategy = c("Strategy A", "Strategy B"),
    stringsAsFactors = FALSE
  )

  # partial match should work
  result <- get_concept_bias_mappings(
    c("Visual Hierarchy"),
    mappings = custom_mappings
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)
})

test_that("load_concept_bias_mappings works with default empty data", {
  result <- bidux:::load_concept_bias_mappings()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("concept", "bias_type", "mitigation_strategy") %in% names(result)))
})

test_that("get_layout_concepts works with valid layouts", {
  result_dual <- get_layout_concepts("dual_process")
  expect_type(result_dual, "character")
  expect_true(length(result_dual) > 0)
  expect_true("Dual-Processing Theory" %in% result_dual)

  result_grid <- get_layout_concepts("grid")
  expect_type(result_grid, "character")
  expect_true("Principle of Proximity" %in% result_grid)

  result_breathable <- get_layout_concepts("breathable")
  expect_type(result_breathable, "character")
  expect_true("Breathable Layouts" %in% result_breathable)
})

test_that("get_layout_concepts handles NULL and empty inputs", {
  result_null <- get_layout_concepts(NULL)
  expect_type(result_null, "character")
  expect_true(length(result_null) > 0)
  expect_true("Visual Hierarchy" %in% result_null)

  result_empty <- get_layout_concepts("")
  expect_type(result_empty, "character")
  expect_true(length(result_empty) > 0)
})

test_that("get_layout_concepts handles unknown layouts with partial match", {
  result_unknown <- get_layout_concepts("unknown_layout_type")
  expect_type(result_unknown, "character")
  expect_true(length(result_unknown) > 0)
})

test_that("get_layout_concepts handles custom mappings", {
  custom_layout_mappings <- data.frame(
    layout = c("custom_layout"),
    primary_concepts = c("Custom Concept A,Custom Concept B"),
    description = c("Custom layout description"),
    stringsAsFactors = FALSE
  )

  result <- get_layout_concepts("custom_layout", mappings = custom_layout_mappings)
  expect_type(result, "character")
  expect_true("Custom Concept A" %in% result)
  expect_true("Custom Concept B" %in% result)
})

test_that("get_accessibility_recommendations works with different contexts", {
  result_visual <- get_accessibility_recommendations("visual chart design")
  expect_type(result_visual, "character")
  expect_true(length(result_visual) > 0)

  result_interactive <- get_accessibility_recommendations("interactive button forms")
  expect_type(result_interactive, "character")
  expect_true(length(result_interactive) > 0)

  result_data <- get_accessibility_recommendations("data visualization chart")
  expect_type(result_data, "character")
  expect_true(length(result_data) > 0)
})

test_that("get_accessibility_recommendations handles empty context", {
  result_empty <- get_accessibility_recommendations("")
  expect_type(result_empty, "character")
  expect_true(length(result_empty) > 0)
})

test_that("get_accessibility_recommendations works with custom guidelines", {
  custom_guidelines <- data.frame(
    guideline = c("custom_guideline_1", "custom_guideline_2"),
    requirement = c("Custom requirement 1", "Custom requirement 2"),
    wcag_level = c("AA", "AAA"),
    stringsAsFactors = FALSE
  )

  result <- get_accessibility_recommendations(
    "test context",
    guidelines = custom_guidelines
  )
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("get_default_layout_mappings returns correct structure", {
  result <- bidux:::get_default_layout_mappings()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("layout", "primary_concepts", "description") %in% names(result)))
  expect_true(nrow(result) >= 5)
  expect_true("dual_process" %in% result$layout)
  expect_true("grid" %in% result$layout)
  expect_true("card" %in% result$layout)
  expect_true("tabs" %in% result$layout)
  expect_true("breathable" %in% result$layout)
})

test_that("load_layout_mappings works with custom mappings", {
  custom_mappings <- data.frame(
    layout = "test_layout",
    primary_concepts = "Test Concept",
    description = "Test description",
    stringsAsFactors = FALSE
  )

  result <- bidux:::load_layout_mappings(custom_mappings)
  expect_equal(result, custom_mappings)
})

test_that("load_accessibility_guidelines returns correct structure", {
  result <- bidux:::load_accessibility_guidelines()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("guideline", "requirement", "wcag_level") %in% names(result)))
  expect_true(nrow(result) >= 3)
})

test_that("load_external_data validates custom data correctly", {
  # test with invalid custom data (missing required columns)
  invalid_data <- data.frame(
    wrong_column = c("test"),
    stringsAsFactors = FALSE
  )

  expect_error(
    bidux:::load_external_data(
      "test.csv",
      c("required_col_1", "required_col_2"),
      function() data.frame(),
      custom_data = invalid_data
    ),
    "Custom data must contain columns"
  )
})

test_that("suggest_theory_from_mappings handles regex vs literal keywords correctly", {
  # test that default mappings are treated as regex
  regex_match <- suggest_theory_from_mappings(
    "Users overwhelmed with too many options",
    "Multiple dropdown choices",
    mappings = NULL
  )
  expect_equal(regex_match, "Hick's Law")

  # test with custom mappings containing literal tokens
  literal_mappings <- data.frame(
    keywords = c("mobile", "desktop"),
    theory = c("Mobile Theory", "Desktop Theory"),
    confidence = c(0.9, 0.9),
    stringsAsFactors = FALSE
  )

  literal_match <- suggest_theory_from_mappings(
    "mobile interface issues",
    "users report problems",
    mappings = literal_mappings
  )
  expect_equal(literal_match, "Mobile Theory")
})
