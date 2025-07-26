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
  expect_equal(
    result$summary_panel,
    "Dashboard simplified for quicker insights"
  )
  expect_equal(result$collaboration, "Added team annotation features")
  expect_match(result$previous_bias, "anchoring: Provide reference points")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_validate fails with missing previous_stage", {
  expect_error(
    bid_validate(summary_panel = "Test", collaboration = "Test"),
    "argument \"previous_stage\" is missing, with no default"
  )
})

test_that("bid_validate allows optional parameters", {
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

  # Should not error when only summary_panel is provided
  expect_no_error(
    bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test"
    )
  )

  # Should not error when only collaboration is provided
  expect_no_error(
    bid_validate(
      previous_stage = anticipate_result,
      collaboration = "Test"
    )
  )
})

test_that("bid_validate provides contextual suggestions", {
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
    summary_panel = "Dashboard improved",
    collaboration = "Added team features"
  )

  # Check that suggestions are contextual (not specific patterns)
  expect_true(nchar(result$suggestions) > 0)
  expect_type(result$suggestions, "character")
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
  # Auto-suggested summary should be meaningful
  expect_true(nchar(result$summary_panel[1]) > 10)
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
  # Auto-suggested collaboration should be meaningful
  expect_true(nchar(result$collaboration[1]) > 10)
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
  # Should contain multiple steps (semicolon-separated)
  expect_gt(stringr::str_count(result$next_steps[1], ";"), 0)
})

test_that("bid_validate handles NA values in previous_stage fields", {
  anticipate_result <- tibble(
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

test_that("bid_validate handles next_steps edge cases", {
  anticipate_result <- tibble(
    stage = "Anticipate",
    bias_mitigations = "test: value",
    timestamp = Sys.time()
  )

  # Test with short steps - should work without warning
  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration",
    next_steps = c("OK", "Good", "Review dashboard", "Implement changes")
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  # Test with long steps - should work without warning
  long_step <- paste(
    rep(
      "This is a very long next step description that goes into excessive detail. ",
      5
    ),
    collapse = ""
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration",
    next_steps = c("Step 1", long_step, "Step 3")
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  # Test with empty steps - should auto-suggest
  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = c("", "  ", "")
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))
  expect_true(nchar(result$next_steps[1]) > 0)
})

test_that("bid_validate handles summary_panel and collaboration variations", {
  anticipate_result <- tibble(
    stage = "Anticipate",
    bias_mitigations = "test: value",
    timestamp = Sys.time()
  )

  # Test with short summary - should still work
  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Too short",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, "Too short")
  expect_true(nchar(result$suggestions) > 0)

  # Test with long summary - should still work
  long_summary <- paste(
    rep(
      "This is a very detailed summary that contains excessive information about the dashboard. ",
      10
    ),
    collapse = ""
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = long_summary,
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, long_summary)
  expect_true(nchar(result$suggestions) > 0)

  # Test with basic collaboration
  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Basic features only"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$collaboration, "Basic features only")
  expect_true(nchar(result$suggestions) > 0)
})

test_that("bid_validate properly handles interaction_principles JSON", {
  anticipate_result <- tibble(
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
  expect_type(result$suggestions, "character")
})
