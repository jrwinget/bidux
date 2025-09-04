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

  expect_s3_class(result, "bid_stage")
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
  expect_s3_class(result, "bid_stage")

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

  expect_s3_class(result, "bid_stage")
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

  expect_s3_class(result, "bid_stage")
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

    expect_s3_class(result, "bid_stage")
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

  expect_s3_class(result, "bid_stage")
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

  expect_s3_class(result, "bid_stage")
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

  expect_s3_class(result, "bid_stage")
  # interaction_principles should no longer be in columns
  expect_false("interaction_principles" %in% names(result))
  # accessibility should be included instead
  expect_true("accessibility" %in% names(result))
})

test_that("bid_anticipate handles include_accessibility parameter properly", {
  structure_result <- tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  # Test with include_accessibility = FALSE
  suppressMessages(
    result_no_access <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = FALSE
    )
  )
  
  expect_s3_class(result_no_access, "bid_stage")
  expect_true(is.na(result_no_access$accessibility[1]))
  
  # Test with include_accessibility = TRUE (default)
  suppressMessages(
    result_with_access <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )
  
  expect_s3_class(result_with_access, "bid_stage")
  expect_false(is.na(result_with_access$accessibility[1]))
  
  # Test with invalid include_accessibility parameter
  suppressWarnings(
    result_invalid <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = "invalid"
    )
  )
  
  expect_s3_class(result_invalid, "bid_stage")
  # Should default to TRUE when invalid
  expect_false(is.na(result_invalid$accessibility[1]))
})

test_that("bid_anticipate generates bias mitigations based on concepts", {
  concepts <- list(
    list(concepts = "Dual-Processing Theory", expected_biases = c("framing", "anchoring")),
    list(concepts = "Visual Hierarchy", expected_biases = c("attention bias", "belief perseverance")),
    list(concepts = "Principle of Proximity", expected_biases = c("association bias", "clustering illusion")),
    list(concepts = "Default Effect", expected_biases = c("status quo bias", "choice architecture")),
    list(concepts = "Aesthetic-Usability", expected_biases = c("beautiful-is-good stereotype", "halo effect")),
    list(concepts = "Information Hierarchy", expected_biases = c("availability bias", "prominence effect"))
  )
  
  for (case in concepts) {
    structure_result <- tibble(
      stage = "Structure",
      layout = "grid",
      concepts = case$concepts,
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_anticipate(
        previous_stage = structure_result,
        bias_mitigations = NULL
      )
    )
    
    expect_s3_class(result, "bid_stage")
    
    # Check that at least one of the expected biases is present
    bias_text <- tolower(result$bias_mitigations[1])
    found_bias <- any(sapply(case$expected_biases, function(bias) {
      grepl(gsub(" ", ".*", tolower(bias)), bias_text)
    }))
    
    expect_true(
      found_bias,
      info = paste("Concept:", case$concepts, "Expected biases:", paste(case$expected_biases, collapse = ", "))
    )
  }
})

test_that("bid_anticipate generates layout-specific bias mitigations", {
  layouts <- list(
    list(layout = "dual_process", expected_pattern = "high-level.*detailed"),
    list(layout = "grid", expected_pattern = "reference.*points|grid|multiple.*reference"),
    list(layout = "card", expected_pattern = "beautiful-is-good|aesthetic"),
    list(layout = "tabs", expected_pattern = "availability|default.*tab"),
    list(layout = "breathable", expected_pattern = "cognitive load|whitespace")
  )
  
  for (case in layouts) {
    structure_result <- tibble(
      stage = "Structure",
      layout = case$layout,
      concepts = "General Concept",
      timestamp = Sys.time()
    )
    
    suppressMessages(
      result <- bid_anticipate(
        previous_stage = structure_result,
        bias_mitigations = NULL
      )
    )
    
    expect_s3_class(result, "bid_stage")
    expect_equal(result$previous_layout[1], case$layout)
    
    # Check that layout-specific bias mitigation is present
    bias_text <- tolower(result$bias_mitigations[1])
    expect_match(
      bias_text,
      case$expected_pattern,
      info = paste("Layout:", case$layout)
    )
  }
})

test_that("bid_anticipate handles custom layout warnings", {
  custom_layouts <- c("custom_layout", "non_standard", "undefined_layout")
  
  for (layout in custom_layouts) {
    structure_result <- tibble(
      stage = "Structure",
      layout = layout,
      concepts = "Visual Hierarchy",
      timestamp = Sys.time()
    )
    
    expect_warning(
      result <- bid_anticipate(
        previous_stage = structure_result,
        bias_mitigations = list(anchoring = "Test")
      ),
      paste0("Layout '", layout, "'.*not recognized")
    )
    
    expect_s3_class(result, "bid_stage")
    expect_equal(result$previous_layout[1], layout)
  }
})

test_that("bid_anticipate extracts previous stage information correctly", {
  # Create a proper bid_stage object using bid_structure
  interpret_result <- bid_interpret(
    central_question = "How to improve layout?",
    data_story = list(
      hook = "Users confused by layout",
      context = "Test context",
      tension = "Test tension",
      resolution = "Test resolution"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Layout issues",
    theory = "Cognitive Load Theory",
    evidence = "Test evidence"
  )
  
  structure_result <- bid_structure(
    previous_stage = notice_result,
    concepts = c("Visual Hierarchy", "Proximity")
  )
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Test reference points")
    )
  )
  
  expect_s3_class(result, "bid_stage") 
  expect_false(is.na(result$previous_layout[1]))
  expect_false(is.na(result$previous_concepts[1]))
  
  # These fields come from the normalized previous stage extraction
  expect_true(is.character(result$previous_central_question[1]) || is.na(result$previous_central_question[1]))
  expect_true(is.character(result$previous_hook[1]) || is.na(result$previous_hook[1]))
  expect_true(is.character(result$previous_problem[1]) || is.na(result$previous_problem[1]))
  expect_true(is.character(result$previous_theory[1]) || is.na(result$previous_theory[1]))
  expect_true(is.character(result$previous_audience[1]) || is.na(result$previous_audience[1]))
  expect_true(is.character(result$previous_personas[1]) || is.na(result$previous_personas[1]))
})

test_that("bid_anticipate creates proper bid_stage object with metadata", {
  structure_result <- tibble(
    stage = "Structure",
    layout = "card",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use positive messaging"
      ),
      include_accessibility = TRUE
    )
  )
  
  # Test bid_stage object properties
  expect_s3_class(result, "bid_stage")
  expect_true("bid_stage" %in% class(result))
  
  # Test metadata
  metadata <- attr(result, "metadata")
  expect_type(metadata, "list")
  expect_true("stage_number" %in% names(metadata))
  expect_equal(metadata$stage_number, 4)
  expect_true("bias_count" %in% names(metadata))
  expect_true(metadata$bias_count >= 2)
  expect_true("include_accessibility" %in% names(metadata))
  expect_true(metadata$include_accessibility)
  expect_false(metadata$auto_generated_biases)
})

test_that("bid_anticipate handles bias_mitigations validation edge cases", {
  structure_result <- tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  
  # Test with numeric values in bias_mitigations  
  suppressMessages(
    result1 <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        anchoring = 123,
        framing = 456.78
      )
    )
  )
  
  # Test with mixed valid and invalid entries
  expect_warning(
    result2 <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(
        "valid_name" = "Valid description",
        "" # empty name
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )
  
  # Test with completely empty list
  suppressMessages(
    result3 <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list()
    )
  )
  
  expect_s3_class(result3, "bid_stage")
  expect_false(is.na(result3$bias_mitigations[1]))
})

test_that("bid_anticipate suggestions include missing bias recommendations", {
  structure_result <- tibble(
    stage = "Structure",
    layout = "dual_process",
    concepts = "Visual Hierarchy, Proximity",
    timestamp = Sys.time()
  )
  
  # Provide only one bias mitigation to trigger suggestions for missing ones
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = structure_result,
      bias_mitigations = list(anchoring = "Use reference points")
    )
  )
  
  expect_s3_class(result, "bid_stage")
  
  # Should suggest other common biases
  suggestions_text <- tolower(result$suggestions[1])
  common_biases <- c("confirmation", "framing", "availability", "status quo")
  
  found_suggestion <- any(sapply(common_biases, function(bias) {
    grepl(bias, suggestions_text)
  }))
  
  expect_true(found_suggestion, info = paste("Suggestions:", result$suggestions[1]))
})

test_that("bid_anticipate handles accessibility extraction from previous stages", {
  # Test with previous_accessibility field
  structure_result1 <- tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    previous_accessibility = "WCAG guidelines applied",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result1 <- bid_anticipate(
      previous_stage = structure_result1,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )
  
  expect_false(is.na(result1$accessibility[1]))
  
  # Test with accessibility field (direct)
  structure_result2 <- tibble(
    stage = "Structure", 
    layout = "grid",
    concepts = "Visual Hierarchy",
    accessibility = "Color contrast verified",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result2 <- bid_anticipate(
      previous_stage = structure_result2,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )
  
  expect_false(is.na(result2$accessibility[1]))
})
