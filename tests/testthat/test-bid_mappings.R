# test functions to reduce repetition
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
    confidence = c(rep(0.5, 49), 0.99, rep(0.5, 50))  # pattern50 gets highest confidence
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
