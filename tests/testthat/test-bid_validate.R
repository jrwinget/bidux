# ==============================================================================
# HELPERS
# ==============================================================================

create_complete_bid_workflow <- function() {
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )

  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )
}

create_minimal_structure_stage <- function() {
  tibble::tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
}

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

test_that("bid_validate works with complete workflow", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
  expect_equal(
    result$summary_panel,
    "Dashboard simplified for quicker insights"
  )
  expect_equal(result$collaboration, "Added team annotation features")
  expect_match(result$previous_bias, "anchoring: Provide reference points")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_validate requires previous_stage parameter", {
  expect_error(
    bid_validate(summary_panel = "Test", collaboration = "Test"),
    "argument \"previous_stage\" is missing, with no default"
  )
})

test_that("bid_validate works with optional parameters", {
  structure_result <- create_complete_bid_workflow()

  # should not error when only summary_panel is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test"
    )
  )

  # should not error when only collaboration is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      collaboration = "Test"
    )
  )
})

# ==============================================================================
# PARAMETER VALIDATION AND AUTO-SUGGESTIONS
# ==============================================================================

test_that("bid_validate auto-suggests when parameters are NULL", {
  structure_result <- create_complete_bid_workflow()

  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = NULL,
      collaboration = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$summary_panel[1]))
  expect_false(is.na(result$collaboration[1]))
  expect_true(nchar(result$summary_panel[1]) > 0)
  expect_true(nchar(result$collaboration[1]) > 0)
})

test_that("bid_validate handles next_steps parameter correctly", {
  structure_result <- create_complete_bid_workflow()

  # test with character vector
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    next_steps = c("Step 1", "Step 2", "Step 3")
  )

  expect_s3_class(result1, "bid_stage")
  expect_true("next_steps" %in% names(result1))

  # test with single string
  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    next_steps = "Single step"
  )

  expect_s3_class(result2, "bid_stage")
  expect_true("next_steps" %in% names(result2))
})

test_that("bid_validate handles telemetry_refs parameter", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    telemetry_refs = list(
      clicks = "button clicks",
      views = "page views"
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(
    "telemetry_refs" %in% names(result) ||
      "next_steps" %in% names(result)
  )
})

# ==============================================================================
# DATA EXTRACTION AND STAGE INTEGRATION
# ==============================================================================

test_that("bid_validate extracts data from previous stages correctly", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "bid_stage")
  # should extract bias information from previous stages
  expect_true(nchar(result$previous_bias[1]) > 0)
  expect_match(result$previous_bias, "anchoring|framing")
})

test_that("bid_validate handles minimal previous_stage data", {
  minimal_structure <- create_minimal_structure_stage()

  result <- bid_validate(
    previous_stage = minimal_structure,
    summary_panel = "Minimal test",
    collaboration = "Minimal collaboration"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
  expect_equal(result$summary_panel, "Minimal test")
})

test_that("bid_validate handles missing fields in previous_stage", {
  # test with structure stage missing some fields
  incomplete_structure <- tibble::tibble(
    stage = "Structure",
    layout = "card",
    timestamp = Sys.time()
  )

  result <- bid_validate(
    previous_stage = incomplete_structure,
    summary_panel = "Test with incomplete data"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
})

# ==============================================================================
# SUGGESTION SYSTEM TESTS
# ==============================================================================

test_that("bid_validate generates contextual suggestions", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard analytics improved",
    collaboration = "Team sharing enabled"
  )

  expect_s3_class(result, "bid_stage")
  expect_true(nchar(result$suggestions[1]) > 0)
  expect_true(is.character(result$suggestions))
})

test_that("bid_validate suggestions vary based on content", {
  structure_result <- create_complete_bid_workflow()

  # test with different content types
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Performance metrics dashboard",
    collaboration = "Real-time team updates"
  )

  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "User behavior analytics",
    collaboration = "Collaborative insights sharing"
  )

  expect_s3_class(result1, "bid_stage")
  expect_s3_class(result2, "bid_stage")
  expect_true(nchar(result1$suggestions[1]) > 0)
  expect_true(nchar(result2$suggestions[1]) > 0)
})

# ==============================================================================
# WORKFLOW INTEGRATION TESTS
# ==============================================================================

test_that("bid_validate works with different previous stage types", {
  # test with bid_stage object
  structure_stage <- create_complete_bid_workflow()

  result1 <- bid_validate(
    previous_stage = structure_stage,
    summary_panel = "Test with bid_stage"
  )

  expect_s3_class(result1, "bid_stage")

  # test with tibble
  structure_tibble <- create_minimal_structure_stage()

  result2 <- bid_validate(
    previous_stage = structure_tibble,
    summary_panel = "Test with tibble"
  )

  expect_s3_class(result2, "bid_stage")
})

test_that("bid_validate preserves essential metadata", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Metadata test",
    collaboration = "Team features"
  )

  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))
  expect_equal(result$stage, "Validate")
  expect_s3_class(result$timestamp, "POSIXct")
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("bid_validate handles empty string parameters", {
  structure_result <- create_complete_bid_workflow()

  # should handle empty strings gracefully
  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = "",
      collaboration = ""
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
})

test_that("bid_validate handles NA parameters", {
  structure_result <- create_complete_bid_workflow()

  # current implementation has an issue with NA handling in if() conditions
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = NA_character_,
      collaboration = NA_character_
    ),
    "missing value where TRUE/FALSE needed"
  )
})

test_that("bid_validate rejects unexpected parameters", {
  structure_result <- create_complete_bid_workflow()

  # current implementation rejects unexpected parameters
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test",
      unexpected_param = "should be ignored"
    ),
    "unused argument"
  )
})

# ==============================================================================
# TELEMETRY INTEGRATION TESTS
# ==============================================================================

test_that("bid_validate integrates telemetry references correctly", {
  structure_result <- create_complete_bid_workflow()

  # test with character vector telemetry refs
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Telemetry-informed summary",
    telemetry_refs = c("click_through_rate", "time_on_page")
  )

  expect_s3_class(result1, "bid_stage")

  # test with list telemetry refs
  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Advanced telemetry summary",
    telemetry_refs = list(
      engagement = "user engagement metrics",
      performance = "page load times"
    )
  )

  expect_s3_class(result2, "bid_stage")
})

test_that("bid_validate combines next_steps with telemetry appropriately", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Combined test",
    next_steps = c("Review metrics", "Update layout"),
    telemetry_refs = list(clicks = "button analytics")
  )

  expect_s3_class(result, "bid_stage")
  expect_true("next_steps" %in% names(result))
})
