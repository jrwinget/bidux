library(testthat)
library(tibble)

test_that("bid_validate works with valid inputs", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Validate")
  expect_equal(result$summary_panel, "Dashboard simplified for quicker insights")
  expect_equal(result$collaboration, "Added team annotation features")
  expect_match(result$previous_bias, "anchoring: Provide reference points")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_validate fails with missing parameters", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  expect_error(bid_validate(previous_stage = anticipate_result, summary_panel = "Test"), "must be provided")
  expect_error(bid_validate(previous_stage = anticipate_result, collaboration = "Test"), "must be provided")
  expect_error(bid_validate(summary_panel = "Test", collaboration = "Test"), "must be provided")
})

test_that("bid_validate provides length-appropriate suggestions", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  short_result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Dashboard improved",
    collaboration = "Added team features"
  )
  expect_match(short_result$suggestions, "expanding", ignore.case = TRUE)

  long_result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = paste(
      rep(
        "Dashboard improved substantially with many features and enhanced visualizations. ",
        5
      ),
      collapse = ""
    ),
    collaboration = "Added team features"
  )
  expect_match(long_result$suggestions, "too long", ignore.case = TRUE)
})

test_that("bid_validate auto-suggests summary_panel when NULL", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = NULL,
      collaboration = "Added team annotation features"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$summary_panel[1]))
  expect_true(nchar(result$summary_panel[1]) > 0)

  expect_match(
    result$summary_panel,
    "dual[_-]process|Proximity|Default",
    ignore.case = TRUE,
    perl = TRUE
  )
})

test_that("bid_validate auto-suggests collaboration when NULL", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    ),
    interaction_principles = list(
      hover = "Show details on hover",
      feedback = "Visual feedback for selected items"
    )
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$collaboration[1]))
  expect_true(nchar(result$collaboration[1]) > 0)

  expect_match(
    result$collaboration,
    "annotation|comment|share|export|save|team",
    ignore.case = TRUE,
    perl = TRUE
  )
})

test_that("bid_validate auto-suggests next_steps when NULL", {
  anticipate_result <- bid_anticipate(
    bid_structure(
      bid_interpret(
        bid_notice(
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),
        central_question = "How to simplify?",
        data_story = list(
          hook = "Users are confused",
          context = "Dashboard has evolved over time"
        )
      ),
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Default Effect")
    ),
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))
  expect_true(nchar(result$next_steps[1]) > 0)

  expect_gt(stringr::str_count(result$next_steps[1], ";"), 0)
})

test_that("bid_validate handles NA values in previous_stage fields", {
  anticipate_result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = NA_character_,
    interaction_principles = NA_character_,
    previous_layout = NA_character_,
    previous_concepts = NA_character_,
    previous_accessibility = NA_character_,
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$previous_bias[1]))
  expect_true(is.na(result$previous_interaction[1]))
  expect_true(is.na(result$previous_layout[1]))
  expect_true(is.na(result$previous_concepts[1]))
  expect_true(is.na(result$previous_accessibility[1]))

  expect_false(is.na(result$summary_panel[1]))
  expect_false(is.na(result$collaboration[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_validate handles edge cases in next_steps parameter", {
  anticipate_result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "test: value",
    timestamp = Sys.time()
  )

  expect_warning(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = c("OK", "Good", "Review dashboard", "Implement changes")
    ),
    "Some next steps are very short"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  long_step <- paste(
    rep(
      "This is a very long next step description that goes into excessive detail. ",
      5
    ),
    collapse = ""
  )

  expect_warning(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = c("Step 1", long_step, "Step 3")
    ),
    "Some next steps are very long"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  expect_warning(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = c("", "  ", "")
    ),
    "next_steps provided but contained only empty strings"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))
  expect_true(nchar(result$next_steps[1]) > 0)
})

test_that("bid_validate handles edge cases in summary_panel and collaboration", {
  anticipate_result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "test: value",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Too short",
      collaboration = "Test collaboration"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, "Too short")
  expect_match(
    result$suggestions,
    "expand|short",
    ignore.case = TRUE,
    perl = TRUE
  )

  long_summary <- paste(
    rep(
      "This is a very detailed summary that contains excessive information about the dashboard. ",
      10
    ),
    collapse = ""
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = long_summary,
      collaboration = "Test collaboration"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, long_summary)
  expect_match(
    result$suggestions,
    "too long|focus",
    ignore.case = TRUE,
    perl = TRUE
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Basic features only"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$collaboration, "Basic features only")
  expect_match(
    result$suggestions,
    "annotation|comment|collaboration",
    ignore.case = TRUE,
    perl = TRUE
  )
})

test_that("bid_validate properly parses interaction_principles JSON", {
  anticipate_result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: Test",
    interaction_principles = "{\"hover\":\"Show on hover\",\"selection\":\"Highlight selected\"}",
    timestamp = Sys.time()
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$previous_interaction))
  expect_match(result$suggestions, "collaboration", ignore.case = TRUE)
})
