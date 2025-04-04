test_that("bid_anticipate works with valid inputs", {
  structure_result <- bid_structure(
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
  )
  
  result <- bid_anticipate(
    previous_stage = structure_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Anticipate")
  expect_match(result$bias_mitigations, "anchoring: Provide reference points")
  expect_match(result$bias_mitigations, "framing: Use consistent positive framing")
  expect_equal(result$previous_layout, "dual_process")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_anticipate fails with missing parameters", {
  structure_result <- bid_structure(
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
  )
  
  expect_error(bid_anticipate(previous_stage = structure_result), "must be provided")
  expect_error(bid_anticipate(bias_mitigations = list(anchoring = "Test")), "must be provided")
})

test_that("bid_anticipate suggests missing common biases", {
  structure_result <- bid_structure(
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
  )
  
  # Test with a single bias mitigation
  result <- bid_anticipate(
    previous_stage = structure_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )
  
  # Should suggest other common biases
  expect_match(result$suggestions, "common biases", ignore.case = TRUE)
})