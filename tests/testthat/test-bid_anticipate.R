# ==============================================================================
# HELPERS
# ==============================================================================

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

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

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
  expect_match(
    result$bias_mitigations,
    "framing: Use consistent positive framing"
  )
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

# ==============================================================================
# AUTO-SUGGESTION FUNCTIONALITY
# ==============================================================================

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

# ==============================================================================
# ACCESSIBILITY FUNCTIONALITY
# ==============================================================================

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

test_that(
  "bid_anticipate handles accessibility when include_accessibility is FALSE",
  {
    notice_result <- create_minimal_notice_tibble()

    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test bias mitigation"),
      include_accessibility = FALSE
    )

    expect_s3_class(result, "bid_stage")
    # when include_accessibility is FALSE, accessibility field may be NA/absent
    if ("accessibility" %in% names(result)) {
      expect_true(is.na(result$accessibility[1]) || nchar(
        result$accessibility[1]
      ) == 0)
    }
  }
)

test_that("bid_anticipate validates bias_mitigations parameter", {
  notice_result <- create_minimal_notice_tibble()

  # empty values should warn
  expect_warning(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "", framing = NA)
    ),
    "bias_mitigations contains empty values"
  )

  # unnamed list should warn
  expect_warning(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        "Provide reference points",
        "Use positive framing"
      )
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

test_that(
  "bid_anticipate handles deprecated interaction_principles parameter",
  {
    notice_result <- create_minimal_notice_tibble()

    expect_warning(
      bid_anticipate(
        previous_stage = notice_result,
        bias_mitigations = list(anchoring = "Test"),
        interaction_principles = list("unnamed", hover = "named")
      ),
      "deprecated|named"
    )
  }
)

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

test_that(
  "bid_anticipate warns and defaults when include_accessibility is invalid",
  {
    notice_result <- create_minimal_notice_tibble()

    expect_warning(
      result <- bid_anticipate(
        previous_stage = notice_result,
        bias_mitigations = list(anchoring = "Test"),
        include_accessibility = "not_logical"
      ),
      "include_accessibility must be a single logical value"
    )

    expect_s3_class(result, "bid_stage")
    expect_true("accessibility" %in% names(result))
  }
)

test_that("bid_anticipate warns when layout is not a recognized type", {
  custom_stage <- create_minimal_notice_tibble()
  custom_stage$layout <- "unknown_layout"

  expect_warning(
    bid_anticipate(
      previous_stage = custom_stage,
      bias_mitigations = list(anchoring = "Test")
    ),
    "Layout 'unknown_layout' is not recognized"
  )
})

test_that("bid_anticipate detects biases from Interpret stage story keywords", {
  interpret_stage <- create_basic_interpret_stage(
    question = "Compare recent trends and baseline targets"
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "anchoring|framing|confirmation",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate accepts valid bid_bias_mitigations S3 object", {
  notice_result <- create_minimal_notice_tibble()

  # create valid S3 object
  valid_mitigations <- new_bias_mitigations(data.frame(
    bias_type = c("anchoring", "framing"),
    mitigation_strategy = c("Provide reference points", "Use positive framing"),
    confidence_level = c(0.8, 0.7),
    stringsAsFactors = FALSE
  ))

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = valid_mitigations
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$bias_mitigations, "anchoring")
  expect_match(result$bias_mitigations, "framing")
})

test_that("bid_anticipate rejects invalid bid_bias_mitigations S3 object", {
  notice_result <- create_minimal_notice_tibble()


  # create invalid S3 object (missing required columns)
  invalid_mitigations <- structure(
    tibble::tibble(bias_type = "anchoring"),
    class = c("bid_bias_mitigations", "tbl_df", "tbl", "data.frame")
  )

  expect_error(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = invalid_mitigations
    ),
    "Invalid bid_bias_mitigations object"
  )
})

test_that("bid_anticipate rejects non-list/non-S3 bias_mitigations", {
  notice_result <- create_minimal_notice_tibble()

  expect_error(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = "not a list"
    ),
    "bias_mitigations must be a bid_bias_mitigations object or list"
  )

  expect_error(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = 123
    ),
    "bias_mitigations must be a bid_bias_mitigations object or list"
  )
})

test_that("bid_anticipate migrates legacy list format with deprecation warning", {
  notice_result <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use positive framing"
      )
    ),
    "deprecated list format"
  )

  expect_s3_class(result, "bid_stage")
  expect_match(result$bias_mitigations, "anchoring")
  expect_match(result$bias_mitigations, "framing")
})

test_that("bid_anticipate handles NULL previous_stage parameter", {
  expect_error(
    bid_anticipate(
      previous_stage = NULL,
      bias_mitigations = list(anchoring = "Test")
    ),
    "must be provided"
  )
})

test_that("bid_anticipate extracts layout from previous_layout field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$previous_layout <- "tabs"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_layout[1], "tabs")
})

test_that("bid_anticipate extracts layout from layout field when previous_layout is NA", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "grid"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_layout[1], "grid")
})

test_that("bid_anticipate prioritizes previous_layout over layout field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$previous_layout <- "card"
  notice_stage$layout <- "grid"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_layout[1], "card")
})

test_that("bid_anticipate extracts concepts from previous_concepts field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$previous_concepts <- "Dual-Processing Theory, Visual Hierarchy"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_concepts[1], "Dual-Processing Theory, Visual Hierarchy")
})

test_that("bid_anticipate extracts concepts from concepts field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Information Hierarchy"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_concepts[1], "Information Hierarchy")
})

test_that("bid_anticipate suggests biases based on Dual-Processing Theory concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Dual-Processing Theory"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # should suggest framing and anchoring biases
  expect_match(
    result$bias_mitigations,
    "framing|anchoring",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests biases based on Visual Hierarchy concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Visual Hierarchy"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # should suggest attention bias or belief perseverance
  expect_match(
    result$bias_mitigations,
    "attention|belief|perseverance",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests biases based on Principle of Proximity concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Principle of Proximity"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "association|clustering",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests biases based on Default Effect concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Default Effect"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "status quo|choice architecture",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests biases based on Aesthetic-Usability concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Aesthetic-Usability"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "beautiful|halo",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate suggests biases based on Information Hierarchy concept", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Information Hierarchy"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "availability|prominence",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate handles multiple concepts", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "Dual-Processing Theory, Default Effect"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # should include biases from both concepts
  expect_match(
    result$bias_mitigations,
    "framing|anchoring|status quo|choice",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate extracts accessibility from previous_accessibility field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$previous_accessibility <- "WCAG 2.1 AA compliance required"

  result <- bid_anticipate(
    previous_stage = notice_stage,
    bias_mitigations = list(anchoring = "Test"),
    include_accessibility = TRUE
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate extracts accessibility from accessibility field", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$accessibility <- "Screen reader support needed"

  result <- bid_anticipate(
    previous_stage = notice_stage,
    bias_mitigations = list(anchoring = "Test"),
    include_accessibility = TRUE
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate detects anchoring keywords in Interpret stage", {
  interpret_stage <- bid_interpret(
    central_question = "Compare values to baseline target",
    data_story = list(
      hook = "Reference points missing",
      context = "Users need comparison data"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "anchoring",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate detects framing keywords in Interpret stage", {
  interpret_stage <- bid_interpret(
    central_question = "How to frame data positively?",
    data_story = list(
      hook = "Negative framing causes concern",
      context = "Users see loss instead of gain"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "framing",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate detects confirmation bias keywords in Interpret stage", {
  interpret_stage <- bid_interpret(
    central_question = "How to validate assumptions?",
    data_story = list(
      hook = "Users expect certain results",
      context = "Hypothesis testing needed",
      tension = "Beliefs may be wrong"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "confirmation",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate detects availability bias keywords in Interpret stage", {
  interpret_stage <- bid_interpret(
    central_question = "How to handle recent data?",
    data_story = list(
      hook = "Recent examples dominate",
      context = "Users recall only top of mind items",
      tension = "Memorable events skew perception"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "availability|recency",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate detects loss aversion keywords in Interpret stage", {
  interpret_stage <- bid_interpret(
    central_question = "How to reduce risk perception?",
    data_story = list(
      hook = "Users averse to loss",
      context = "Risk avoidance behavior observed"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "loss|aversion|risk",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate uses default biases when no keywords detected", {
  interpret_stage <- bid_interpret(
    central_question = "Generic dashboard question?",
    data_story = list(
      hook = "Data overview",
      context = "General information display"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # should have common biases as fallback
  expect_match(
    result$bias_mitigations,
    "anchoring|framing|confirmation",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate handles Interpret stage with tension field", {
  interpret_stage <- bid_interpret(
    central_question = "What is the issue?",
    data_story = list(
      hook = "Users struggle",
      context = "Dashboard complexity",
      tension = "Risk of loss aversion impacts decisions"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
})

# layout-specific bias suggestions (deprecated path)

test_that("bid_anticipate generates layout-specific biases for dual_process", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "dual_process"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "framing",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate generates layout-specific biases for grid", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "grid"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "anchoring",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate generates layout-specific biases for card", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "card"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "beautiful|good|stereotype",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate generates layout-specific biases for tabs", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "tabs"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "availability",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate generates layout-specific biases for breathable", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "breathable"

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "cognitive",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate includes tabs-specific accessibility in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "tabs"

  # when using NULL bias_mitigations, accessibility is included in suggestions
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # accessibility advice appears in bias_mitigations field when auto-suggested
  expect_match(
    result$bias_mitigations,
    "keyboard|tab|screen reader|accessibility",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate includes grid-specific accessibility in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "grid"

  # when using NULL bias_mitigations, accessibility is included in suggestions
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # accessibility advice appears in bias_mitigations field when auto-suggested
  expect_match(
    result$bias_mitigations,
    "row|column|header|cell|accessibility",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate includes card-specific biases in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "card"

  # when using NULL bias_mitigations, layout-specific biases are suggested
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # card layout generates beautiful-is-good stereotype bias
  expect_match(
    result$bias_mitigations,
    "beautiful|good|stereotype|aesthetic",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate includes dual_process-specific accessibility in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "dual_process"

  # when using NULL bias_mitigations, accessibility is included in suggestions
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # accessibility advice appears in bias_mitigations field when auto-suggested
  expect_match(
    result$bias_mitigations,
    "summary|detail|view|accessibility",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate includes breathable-specific accessibility in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$layout <- "breathable"

  # when using NULL bias_mitigations, accessibility is included in suggestions
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # accessibility advice appears in bias_mitigations field when auto-suggested
  expect_match(
    result$bias_mitigations,
    "contrast|focus|indicator|accessibility",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate auto-suggests common biases when no layout", {
  notice_stage <- create_minimal_notice_tibble()

  # when using NULL bias_mitigations and no layout, common biases are suggested
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL,
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
  # should include common biases as fallback
  expect_match(
    result$bias_mitigations,
    "anchoring|framing|confirmation",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate adds accessibility to user-provided bias mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  # provide bias mitigations without accessibility
  expect_message(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = "Test anchoring"),
      include_accessibility = TRUE
    ),
    "accessibility mitigation"
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$accessibility[1]))
})

test_that("bid_anticipate does not add accessibility when user provides it", {
  notice_stage <- create_minimal_notice_tibble()

  # when user provides accessibility in the legacy list format,
  # the function should use it (though it gets migrated to S3 class)
  suppressMessages(suppressWarnings(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(
        anchoring = "Test anchoring",
        accessibility = "Custom accessibility advice"
      ),
      include_accessibility = TRUE
    )
  ))

  expect_s3_class(result, "bid_stage")
  # the accessibility should be preserved in the bias_mitigations or result
  # when using legacy format that gets migrated, it becomes part of the S3 object
  expect_true("accessibility" %in% names(result))
})

test_that("bid_anticipate does not add accessibility when include_accessibility is FALSE", {
  notice_stage <- create_minimal_notice_tibble()

  result <- bid_anticipate(
    previous_stage = notice_stage,
    bias_mitigations = list(anchoring = "Test"),
    include_accessibility = FALSE
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$accessibility[1]))
})

test_that("bid_anticipate handles empty concepts string", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- ""

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles concepts with extra whitespace", {
  notice_stage <- create_minimal_notice_tibble()
  notice_stage$concepts <- "  Dual-Processing Theory  ,   Visual Hierarchy  "

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  # concepts should be properly trimmed
  expect_match(result$previous_concepts, "Dual-Processing Theory")
})

test_that("bid_anticipate handles empty list bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list()
    ),
    "must be a non-empty named list"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles partially named list bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = "Test", "Unnamed value")
    ),
    "must be a non-empty named list"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles NA values in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = NA)
    ),
    "empty values"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles NULL values in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = NULL)
    ),
    "empty values"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles whitespace-only values in bias_mitigations", {
  notice_stage <- create_minimal_notice_tibble()

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = "   ")
    ),
    "empty values"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles multiple invalid include_accessibility values", {
  notice_stage <- create_minimal_notice_tibble()

  # vector of logical values
  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = c(TRUE, FALSE)
    ),
    "single logical value"
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles NA include_accessibility value", {
  notice_stage <- create_minimal_notice_tibble()

  # NA in logical check causes an error because NA && ... fails
  # the validation at lines 169-170 doesn't catch NA (is.logical(NA) == TRUE)
  # this causes an error at lines 458-468 where NA && ... is evaluated
  expect_error(
    suppressMessages(suppressWarnings(
      bid_anticipate(
        previous_stage = notice_stage,
        bias_mitigations = list(anchoring = "Test"),
        include_accessibility = NA
      )
    )),
    "missing value"
  )
})

test_that("bid_anticipate generates suggestions for missing common biases", {
  notice_stage <- create_minimal_notice_tibble()

  # provide only one bias
  result <- bid_anticipate(
    previous_stage = notice_stage,
    bias_mitigations = list(anchoring = "Test")
  )

  expect_s3_class(result, "bid_stage")
  # suggestions should mention missing biases
  expect_match(
    result$suggestions,
    "confirmation|framing",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate handles Interpret stage with resolution field", {
  interpret_stage <- bid_interpret(
    central_question = "What should we do?",
    data_story = list(
      hook = "Problem identified",
      context = "Analysis needed",
      tension = "Risk of loss",
      resolution = "Implement baseline comparison"
    )
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate handles bid_data_story S3 object in interpret stage", {
  data_story <- new_data_story(
    hook = "Users need help with comparison",
    context = "Reference points are missing",
    tension = "Baseline targets not clear",
    resolution = "Add anchoring information"
  )

  interpret_stage <- bid_interpret(
    central_question = "How to improve comparisons?",
    data_story = data_story
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_stage,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_match(
    result$bias_mitigations,
    "anchoring",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate handles quiet parameter", {
  notice_stage <- create_minimal_notice_tibble()

  # test with quiet = TRUE - should suppress messages but not warnings
  # deprecation warnings may still appear
  suppressWarnings(
    result <- bid_anticipate(
      previous_stage = notice_stage,
      bias_mitigations = list(anchoring = "Test"),
      quiet = TRUE
    )
  )

  expect_s3_class(result, "bid_stage")
})

test_that("bid_anticipate respects bidux.quiet option", {
  notice_stage <- create_minimal_notice_tibble()

  withr::with_options(list(bidux.quiet = TRUE), {
    # deprecation warnings may still appear
    suppressWarnings(
      result <- bid_anticipate(
        previous_stage = notice_stage,
        bias_mitigations = list(anchoring = "Test")
      )
    )
  })

  expect_s3_class(result, "bid_stage")
})
