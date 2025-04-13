library(testthat)
library(tibble)

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
  
  result <- bid_anticipate(
    previous_stage = structure_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )
  
  # should suggest other common biases
  expect_match(result$suggestions, "common biases", ignore.case = TRUE)
})

test_that("bid_anticipate auto-suggests bias_mitigations when NULL", {
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
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = NULL
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$bias_mitigations[1]))
  expect_true(nchar(result$bias_mitigations[1]) > 0)
  
  # should suggest at least anchoring, framing or confirmation bias
  expect_match(
    result$bias_mitigations, 
    "anchoring|framing|confirm", 
    ignore.case = TRUE, 
    perl = TRUE
  )
})

test_that("bid_anticipate auto-suggests interaction_principles when NULL", {
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
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = NULL
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$interaction_principles[1]))
  
  expect_match(
    result$interaction_principles, 
    "hover|feedback|selection|progressive", 
    ignore.case = TRUE, 
    perl = TRUE
  )
})

test_that("bid_anticipate handles different layout types appropriately", {
  layouts <- c("dual_process", "grid", "card", "tabs", "breathable")
  
  for (layout in layouts) {
    structure_result <- tibble::tibble(
      stage = "Structure",
      layout = layout,
      concepts = "Visual Hierarchy",
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_anticipate(
        previous_stage = structure_result,
        bias_mitigations = NULL
      )
    )
    
    expect_s3_class(result, "tbl_df")
    expect_equal(result$previous_layout, layout)
    
    expect_match(
      result$suggestions,
      layout,
      ignore.case = TRUE
    )
  }
})

test_that("bid_anticipate handles NA values in previous_stage fields", {
  structure_result <- tibble::tibble(
    stage = "Structure",
    layout = NA_character_,
    concepts = NA_character_,
    accessibility = NA_character_,
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use consistent positive framing"
      )
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$previous_layout[1]))
  expect_true(is.na(result$previous_concepts[1]))
  expect_true(is.na(result$previous_accessibility[1]))
  
  # should still generate valid suggestions
  expect_false(is.na(result$bias_mitigations[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_anticipate handles edge cases in bias_mitigations parameter", {
  structure_result <- tibble::tibble(
    stage = "Structure",
    layout = "dual_process",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  expect_warning(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        anchoring = "",
        framing = NA
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )
  
  expect_warning(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        "Provide reference points",
        "Use consistent positive framing"
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        anchoring = 123,
        framing = TRUE
      )
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_match(result$bias_mitigations, "anchoring: 123")
  expect_match(result$bias_mitigations, "framing: TRUE")
})

test_that("bid_anticipate handles edge cases in interaction_principles param", {
  structure_result <- tibble::tibble(
    stage = "Structure",
    layout = "dual_process",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  expect_warning(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = list(
        "" = "This has empty name",
        hover = "This has valid name"
      )
    ),
    "interaction_principles must be a non-empty named list"
  )
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = list(
        hover = 123,
        feedback = TRUE
      )
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_match(result$interaction_principles, "hover")
  expect_match(result$interaction_principles, "123")
  expect_match(result$interaction_principles, "feedback")
  expect_match(result$interaction_principles, "TRUE")
})
