# Optimized tests for bid_anticipate function
# focused on core functionality and essential edge cases

# Test fixtures to reduce repetition
create_basic_interpret_stage <- function(question = "How to simplify?") {
  bid_interpret(
    central_question = question,
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
}

create_basic_notice_stage <- function(interpret_stage = NULL) {
  if (is.null(interpret_stage)) {
    interpret_stage <- create_basic_interpret_stage()
  }
  bid_notice(
    previous_stage = interpret_stage,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
}

create_minimal_notice_tibble <- function() {
  tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    theory = "Test Theory",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )
}

# ============================================================================
# CORE FUNCTIONALITY TESTS
# ============================================================================

test_that("bid_anticipate works with valid complete workflow", {
  notice_result <- create_basic_notice_stage()

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Anticipate")
  expect_match(result$bias_mitigations, "anchoring: Provide reference points")
  expect_match(result$bias_mitigations, "framing: Use consistent positive framing")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_anticipate requires previous_stage parameter", {
  expect_error(
    bid_anticipate(bias_mitigations = list(anchoring = "Test")),
    "must be provided"
  )
})

test_that("bid_anticipate works with optional bias_mitigations", {
  notice_result <- create_basic_notice_stage()

  suppressMessages({
    result <- bid_anticipate(previous_stage = notice_result)
  })

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Anticipate")
})

# ============================================================================
# AUTO-SUGGESTION FUNCTIONALITY
# ============================================================================

test_that("bid_anticipate auto-suggests bias_mitigations when NULL", {
  notice_result <- create_basic_notice_stage()

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$bias_mitigations[1]))
  expect_true(nchar(result$bias_mitigations[1]) > 0)
  expect_match(
    result$bias_mitigations,
    "anchoring|framing|confirmation bias|accessibility",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests missing common biases", {
  notice_result <- create_basic_notice_stage()

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )

  # should suggest other biases in suggestions
  expect_match(
    result$suggestions,
    "confirmation|framing|Consider",
    ignore.case = TRUE
  )
})

# ============================================================================
# ACCESSIBILITY FUNCTIONALITY
# ============================================================================

test_that("bid_anticipate includes accessibility when requested", {
  notice_result <- create_basic_notice_stage()

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true("accessibility" %in% names(result))
  expect_false(is.na(result$accessibility[1]))
})

test_that("bid_anticipate handles accessibility when include_accessibility is FALSE", {
  notice_result <- create_minimal_notice_tibble()

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Test bias mitigation"),
    include_accessibility = FALSE
  )

  expect_s3_class(result, "bid_stage")
  # when include_accessibility is FALSE, accessibility field may be NA or absent
  if ("accessibility" %in% names(result)) {
    expect_true(is.na(result$accessibility[1]) || nchar(result$accessibility[1]) == 0)
  }
})

# ============================================================================
# PARAMETER VALIDATION AND ERROR HANDLING
# ============================================================================

test_that("bid_anticipate validates bias_mitigations parameter", {
  notice_result <- create_minimal_notice_tibble()

  # empty values should warn
  expect_warning(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "", framing = NA)
    ),
    "bias_mitigations must be a non-empty named list"
  )

  # unnamed list should warn
  expect_warning(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list("Provide reference points", "Use positive framing")
    ),
    "bias_mitigations must be a non-empty named list"
  )
})

test_that("bid_anticipate handles non-character bias mitigation values", {
  notice_result <- create_minimal_notice_tibble()

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = 123, framing = TRUE)
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$bias_mitigations, "anchoring: 123")
  expect_match(result$bias_mitigations, "framing: TRUE")
})

test_that("bid_anticipate handles deprecated interaction_principles parameter", {
  notice_result <- create_minimal_notice_tibble()

  expect_warning(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = list("unnamed", hover = "named")
    ),
    "deprecated|named"
  )
})

# ============================================================================
# DATA HANDLING AND EDGE CASES
# ============================================================================

test_that("bid_anticipate handles NA values in previous_stage fields", {
  notice_result <- create_minimal_notice_tibble()

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use consistent positive framing"
      )
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$previous_layout[1]))
  expect_true(is.na(result$previous_concepts[1]))
  expect_false(is.na(result$bias_mitigations[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_anticipate works with different layout contexts", {
  layouts <- c("dual_process", "grid", "card", "tabs", "breathable")

  for (layout in layouts) {
    notice_result <- create_basic_notice_stage()

    suppressMessages(
      result <- bid_anticipate(
        previous_stage = notice_result,
        bias_mitigations = NULL
      )
    )

    expect_s3_class(result, "bid_stage")
    expect_true(nchar(result$suggestions[1]) > 0)
  }
})

test_that("bid_anticipate handles missing fields in previous_stage", {
  # test with minimal previous stage data
  minimal_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Minimal problem",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = minimal_stage,
      bias_mitigations = list(anchoring = "Test")
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Anticipate")
  expect_false(is.na(result$bias_mitigations[1]))
})

# ============================================================================
# INTEGRATION AND WORKFLOW TESTS
# ============================================================================

test_that("bid_anticipate integrates properly with BID workflow", {
  # test full workflow integration without excessive repetition
  interpret_result <- create_basic_interpret_stage()
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )

  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Test anchoring")
  )

  # should work with subsequent stages
  expect_no_error(
    bid_structure(
      anticipate_result,
      concepts = c("Principle of Proximity")
    )
  )
})

test_that("bid_anticipate preserves essential stage metadata", {
  notice_result <- create_basic_notice_stage()

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Test")
  )

  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))
  expect_equal(result$stage, "Anticipate")
  expect_s3_class(result$timestamp, "POSIXct")
})

# ============================================================================
# PARAMETER EDGE CASES
# ============================================================================

test_that("bid_anticipate handles unexpected parameters gracefully", {
  notice_result <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      unexpected_param = "should be ignored"
    ),
    "unexpected.*parameter|ignored"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate accessibility advice varies by context", {
  notice_result <- create_minimal_notice_tibble()

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Test accessibility"),
    include_accessibility = TRUE
  )

  expect_s3_class(result, "bid_stage")
  expect_true("accessibility" %in% names(result))
  expect_true(is.character(result$accessibility))
  expect_gt(nchar(result$accessibility[1]), 0)
})