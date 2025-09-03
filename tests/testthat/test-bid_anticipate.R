test_that("bid_anticipate works with valid inputs", {
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
  
  structure_result <- bid_structure(
    notice_result,

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
  expect_match(
    result$bias_mitigations,
    "framing: Use consistent positive framing"
  )
  expect_equal(result$previous_layout, "breathable")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_anticipate fails with missing parameters", {
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
  
  structure_result <- bid_structure(
    notice_result,

    concepts = c("Principle of Proximity", "Default Effect")
  )

  # This should NOT throw an error since bias_mitigations is optional
  suppressMessages({
    result <- bid_anticipate(previous_stage = structure_result)
  })
  expect_s3_class(result, "tbl_df")

  # This SHOULD throw an error since previous_stage is required
  expect_error(
    bid_anticipate(bias_mitigations = list(anchoring = "Test")),
    "must be provided"
  )
})

test_that("bid_anticipate suggests missing common biases", {
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
  
  structure_result <- bid_structure(
    notice_result,

    concepts = c("Principle of Proximity", "Default Effect")
  )

  result <- bid_anticipate(
    previous_stage = structure_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )

  # should suggest other biases in suggestions (checking for any of confirmation, framing, or common)
  expect_match(
    result$suggestions,
    "confirmation|framing|Consider",
    ignore.case = TRUE
  )
})

test_that("bid_anticipate auto-suggests bias_mitigations when NULL", {
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
  
  structure_result <- bid_structure(
    notice_result,

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

  # should suggest common bias mitigations (updated for 0.2.0 auto-suggestions)
  expect_match(
    result$bias_mitigations,
    "attention bias|belief perseverance|cognitive load|association bias|choice architecture",
    ignore.case = TRUE,
    perl = TRUE
  )
})

test_that("bid_anticipate auto-suggests interaction_principles when NULL", {
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
  
  structure_result <- bid_structure(
    notice_result,

    concepts = c("Principle of Proximity", "Default Effect")
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result, "tbl_df")
  # interaction_principles is no longer in the result
  expect_false("interaction_principles" %in% names(result))

  # accessibility should be included and not NA
  expect_true("accessibility" %in% names(result))
  expect_false(is.na(result$accessibility[1]))
})

test_that("bid_anticipate handles different layout types appropriately", {
  layouts <- c("dual_process", "grid", "card", "tabs", "breathable")

  for (layout in layouts) {
    structure_result <- tibble(
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

    # Check that the result includes some relevant content instead of specific layout names
    expect_true(nchar(result$suggestions[1]) > 0)
  }
})

test_that("bid_anticipate handles NA values in previous_stage fields", {
  structure_result <- tibble(
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
  # accessibility is now included by default, so it should not be NA
  expect_false(is.na(result$accessibility[1]))

  # should still generate valid suggestions
  expect_false(is.na(result$bias_mitigations[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_anticipate handles edge cases in bias_mitigations parameter", {
  structure_result <- tibble(
    stage = "Structure",

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
  structure_result <- tibble(
    stage = "Structure",

    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = list(
        "This has empty name",
        hover = "This has valid name"
      )
    ),
    "interaction_principles.*deprecated"
  )

  # interaction_principles is now deprecated, should not be in columns
  expect_false("interaction_principles" %in% names(result))

  # Test that accessibility is included by default
  result2 <- bid_anticipate(
    previous_stage = structure_result,
    bias_mitigations = list(anchoring = "Test")
  )

  expect_s3_class(result2, "tbl_df")
  expect_true("accessibility" %in% names(result2))
  expect_false(is.na(result2$accessibility))
})

test_that("bid_anticipate warns for deprecated interaction_principles and includes accessibility", {
  structure_result <- tibble(
    stage = "Structure",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )

  json_interaction <- list(
    hover = "Show details on hover",
    feedback = "Highlight selection"
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      interaction_principles = json_interaction
    ),
    "interaction_principles.*deprecated"
  )

  expect_s3_class(result, "tbl_df")
  # interaction_principles should no longer be in columns
  expect_false("interaction_principles" %in% names(result))
  # accessibility should be included instead
  expect_true("accessibility" %in% names(result))
})
