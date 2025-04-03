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
  
  # Test short summary
  short_result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Dashboard improved",
    collaboration = "Added team features"
  )
  expect_match(short_result$suggestions, "expanding", ignore.case = TRUE)
  
  # Test very long summary
  long_result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = paste(rep("Dashboard improved substantially with many features and enhanced visualizations. ", 5), collapse = ""),
    collaboration = "Added team features"
  )
  expect_match(long_result$suggestions, "too long", ignore.case = TRUE)
})