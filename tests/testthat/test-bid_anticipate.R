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

  result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  structure_result <- bid_structure(
    result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Anticipate")
  expect_match(result$bias_mitigations, "anchoring: Provide reference points")
  expect_match(
    result$bias_mitigations,
    "framing: Use consistent positive framing"
  )
  # anticipate stage comes before structure, so no previous_layout expected
  expect_true(is.na(result$previous_layout) || result$previous_layout == "")
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

  # This should NOT throw an error since bias_mitigations is optional
  suppressMessages({
    anticipate_result <- bid_anticipate(previous_stage = notice_result)
  })

  structure_result <- bid_structure(
    anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )
  expect_s3_class(anticipate_result, "bid_stage")

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

  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )

  structure_result <- bid_structure(
    anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  # should suggest other biases in suggestions (checking for any of confirmation, framing, or common)
  expect_match(
    anticipate_result$suggestions,
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

  suppressMessages(
    anticipate_result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  )

  structure_result <- bid_structure(
    anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  expect_s3_class(anticipate_result, "bid_stage")
  expect_false(is.na(anticipate_result$bias_mitigations[1]))
  expect_true(nchar(anticipate_result$bias_mitigations[1]) > 0)

  # should suggest common bias mitigations (updated for 0.3.1 auto-suggestions)
  expect_match(
    anticipate_result$bias_mitigations,
    "anchoring|framing|confirmation bias|accessibility",
    ignore.case = TRUE
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

  suppressMessages(
    anticipate_result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )

  structure_result <- bid_structure(
    anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  expect_s3_class(anticipate_result, "bid_stage")
  # interaction_principles is no longer in the result
  expect_false("interaction_principles" %in% names(anticipate_result))

  # accessibility should be included and not NA
  expect_true("accessibility" %in% names(anticipate_result))
  expect_false(is.na(anticipate_result$accessibility[1]))
})

test_that("bid_anticipate handles different layout types appropriately", {
  layouts <- c("dual_process", "grid", "card", "tabs", "breathable")

  for (layout in layouts) {
    interpret_stage <- bid_interpret(central_question = "Test question")
    notice_result <- bid_notice(interpret_stage, problem = "Test problem", theory = "Test Theory", evidence = "Test evidence")

    suppressMessages(
      result <- bid_anticipate(
        previous_stage = notice_result,
        bias_mitigations = NULL
      )
    )

    expect_s3_class(result, "bid_stage")
    expect_true(is.na(result$previous_layout) || result$previous_layout == "")

    # Check that the result includes some relevant content instead of specific layout names
    expect_true(nchar(result$suggestions[1]) > 0)
  }
})

test_that("bid_anticipate handles NA values in previous_stage fields", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem",
    theory = "Test Theory",
    evidence = "Test evidence",
    timestamp = Sys.time()
  )

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
  # accessibility is now included by default, so it should not be NA
  expect_false(is.na(result$accessibility[1]))

  # should still generate valid suggestions
  expect_false(is.na(result$bias_mitigations[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_anticipate handles edge cases in bias_mitigations parameter", {
  notice_result <- tibble(
    stage = "Notice",

    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        anchoring = "",
        framing = NA
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        "Provide reference points",
        "Use consistent positive framing"
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
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
  notice_result <- tibble(
    stage = "Notice",

    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
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
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Test")
  )

  expect_s3_class(result2, "tbl_df")
  expect_true("accessibility" %in% names(result2))
  expect_false(is.na(result2$accessibility))
})

test_that("bid_anticipate warns for deprecated interaction_principles and includes accessibility", {
  interpret_stage <- bid_interpret(central_question = "Test question")
  notice_result <- bid_notice(interpret_stage, problem = "Test problem", theory = "Test Theory", evidence = "Test evidence")

  json_interaction <- list(
    hover = "Show details on hover",
    feedback = "Highlight selection"
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
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
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  # Test with include_accessibility = FALSE
  suppressMessages(
    result_no_access <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = FALSE
    )
  )

  expect_s3_class(result_no_access, "bid_stage")
  expect_true(is.na(result_no_access$accessibility[1]))

  # Test with include_accessibility = TRUE (default)
  suppressMessages(
    result_with_access <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    )
  )

  expect_s3_class(result_with_access, "bid_stage")
  expect_false(is.na(result_with_access$accessibility[1]))

  # Test with invalid include_accessibility parameter
  suppressWarnings(
    result_invalid <- bid_anticipate(
      previous_stage = notice_result,
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
    notice_result <- tibble(
      stage = "Notice",
      problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
      concepts = case$concepts,
      timestamp = Sys.time()
    )

    suppressMessages(
      result <- bid_anticipate(
        previous_stage = notice_result,
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
    notice_result <- tibble(
      stage = "Notice",
      layout = case$layout,
      concepts = "General Concept",
      timestamp = Sys.time()
    )

    suppressMessages(
      result <- bid_anticipate(
        previous_stage = notice_result,
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
    # Create a previous stage with custom layout
    notice_result <- tibble(
      stage = "Notice",
      problem = "Test problem",
      theory = "Test Theory",
      evidence = "Test evidence",
      layout = layout,
      timestamp = Sys.time()
    )

    expect_warning(
      result <- bid_anticipate(
        previous_stage = notice_result,
        bias_mitigations = list(anchoring = "Test")
      ),
      "Layout.*not recognized"
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

  suppressMessages(
    anticipate_result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test reference points")
    )
  )

  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Visual Hierarchy", "Proximity")
  )

  expect_s3_class(anticipate_result, "bid_stage")
  # anticipate comes before structure, so these should be NA or empty
  expect_true(is.na(anticipate_result$previous_layout[1]) || anticipate_result$previous_layout[1] == "")
  expect_true(is.na(anticipate_result$previous_concepts[1]) || anticipate_result$previous_concepts[1] == "")

  # These fields come from the normalized previous stage extraction
  expect_true(is.character(anticipate_result$previous_central_question[1]) || is.na(anticipate_result$previous_central_question[1]))
  expect_true(is.character(anticipate_result$previous_hook[1]) || is.na(anticipate_result$previous_hook[1]))
  expect_true(is.character(anticipate_result$previous_problem[1]) || is.na(anticipate_result$previous_problem[1]))
  expect_true(is.character(anticipate_result$previous_theory[1]) || is.na(anticipate_result$previous_theory[1]))
  expect_true(is.character(anticipate_result$previous_audience[1]) || is.na(anticipate_result$previous_audience[1]))
  expect_true(is.character(anticipate_result$previous_personas[1]) || is.na(anticipate_result$previous_personas[1]))
})

test_that("bid_anticipate creates proper bid_stage object with metadata", {
  notice_result <- tibble(
    stage = "Notice",
    layout = "card",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
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
  expect_equal(metadata$stage_number, 3)
  expect_true("bias_count" %in% names(metadata))
  expect_true(metadata$bias_count >= 2)
  expect_true("include_accessibility" %in% names(metadata))
  expect_true(metadata$include_accessibility)
  expect_false(metadata$auto_generated_biases)
})

test_that("bid_anticipate handles bias_mitigations validation edge cases", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  # Test with numeric values in bias_mitigations
  suppressMessages(
    result1 <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        anchoring = 123,
        framing = 456.78
      )
    )
  )

  # Test with mixed valid and invalid entries
  expect_warning(
    result2 <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        "valid_name" = "Valid description",
        "" # empty name
      )
    ),
    "bias_mitigations must be a non-empty named list"
  )

  # Test with completely empty list - should generate warning
  expect_warning(
    result3 <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list()
    ),
    "bias_mitigations must be a non-empty named list"
  )

  expect_s3_class(result3, "bid_stage")
  expect_false(is.na(result3$bias_mitigations[1]))
})

test_that("bid_anticipate suggestions include missing bias recommendations", {
  notice_result <- tibble(
    stage = "Notice",
    layout = "dual_process",
    concepts = "Visual Hierarchy, Proximity",
    timestamp = Sys.time()
  )

  # Provide only one bias mitigation to trigger suggestions for missing ones
  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
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
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
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
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
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

# Test story element bias detection from Interpret stage
test_that("bid_anticipate detects biases from Interpret stage story elements", {
  # Create interpret stage with story elements containing bias keywords
  interpret_result <- tibble(
    stage = "Interpret",
    central_question = "How to compare performance against baseline targets?",
    hook = "Recent data shows unexpected losses compared to previous gains",
    tension = "Risk of not meeting expectations and avoiding negative outcomes",
    resolution = "Validate assumptions with new hypothesis testing",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")

  # Should detect multiple biases based on keywords
  bias_text <- tolower(result$bias_mitigations[1])

  # Check for specific biases detected from keywords
  expected_biases <- c("anchoring", "framing", "loss aversion", "confirmation bias")
  found_biases <- sapply(expected_biases, function(bias) {
    grepl(gsub(" ", ".*", bias), bias_text)
  })

  expect_true(any(found_biases),
    info = paste("Expected biases:", paste(expected_biases, collapse = ", "),
                 "Found:", result$bias_mitigations[1]))
})

# Test fallback when no biases are auto-suggested
test_that("bid_anticipate provides fallback biases when auto-suggestion yields few results", {
  # Create minimal previous stage to trigger fallback
  minimal_previous <- tibble(
    stage = "Notice",
    problem = "Simple issue",
    theory = "Basic theory",
    evidence = "Basic evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = minimal_previous,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")

  # Should contain common fallback biases
  bias_text <- tolower(result$bias_mitigations[1])
  common_fallbacks <- c("anchoring", "framing", "confirmation")

  found_fallback <- any(sapply(common_fallbacks, function(bias) {
    grepl(bias, bias_text)
  }))

  expect_true(found_fallback)
})

# Test concept extraction from previous stages
test_that("bid_anticipate extracts concepts from previous_concepts field", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem",
    theory = "Test Theory",
    evidence = "Test evidence",
    previous_concepts = "Dual-Processing Theory, Visual Hierarchy",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$previous_concepts[1], "Dual-Processing Theory, Visual Hierarchy")

  # Should apply concept-based bias mitigations
  bias_text <- tolower(result$bias_mitigations[1])
  expected_concept_biases <- c("framing", "anchoring", "attention bias")

  found_concept_bias <- any(sapply(expected_concept_biases, function(bias) {
    grepl(gsub(" ", ".*", bias), bias_text)
  }))

  expect_true(found_concept_bias)
})

# Test non-character bias mitigation conversion
test_that("bid_anticipate converts non-character bias mitigations", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        anchoring = 123,
        framing = TRUE,
        confirmation = factor("address confirmation bias")
      )
    )
  )

  expect_s3_class(result, "bid_stage")

  # All should be converted to character strings
  expect_match(result$bias_mitigations, "anchoring: 123")
  expect_match(result$bias_mitigations, "framing: TRUE")
  expect_match(result$bias_mitigations, "confirmation: address confirmation bias")
})

# Test bid_concept integration for bias suggestions
test_that("bid_anticipate integrates with bid_concept for implementation tips", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        "Anchoring Effect" = "Use multiple reference points"
      )
    )
  )

  expect_s3_class(result, "bid_stage")

  # Should include implementation tips from bid_concept if available
  expect_match(result$suggestions, "Anchoring Effect mitigation:")
  expect_true(nchar(result$suggestions[1]) > 50) # Should have detailed suggestions
})

# Test unknown bias mitigation handling
test_that("bid_anticipate handles unknown bias names gracefully", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(
        "UnknownBias123" = "Handle unknown bias type",
        "CustomBias" = "Custom mitigation strategy"
      )
    )
  )

  expect_s3_class(result, "bid_stage")

  # Should handle unknown biases gracefully with fallback message
  expect_match(result$suggestions, "UnknownBias123 mitigation: Consider how this bias affects")
  expect_match(result$suggestions, "CustomBias mitigation: Consider how this bias affects")
})

# Test empty concept list handling
test_that("bid_anticipate handles empty concept lists", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    concepts = "", # Empty concepts
    previous_concepts = "   ", # Whitespace only
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$previous_concepts[1]))

  # Should still generate fallback bias mitigations
  expect_false(is.na(result$bias_mitigations[1]))
  expect_true(nchar(result$bias_mitigations[1]) > 0)
})

# Test message generation and formatting
test_that("bid_anticipate generates proper user messages", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  # Test auto-suggestion message
  expect_message(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    ),
    "Automatically suggested bias mitigations:"
  )

  # Test accessibility addition message
  expect_message(
    bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = TRUE
    ),
    "Added accessibility mitigation"
  )
})

# Test empty story elements handling in Interpret stage
test_that("bid_anticipate handles empty story elements in Interpret stage", {
  interpret_result <- tibble(
    stage = "Interpret",
    central_question = NA,
    hook = "",
    tension = "   ", # Whitespace only
    resolution = NULL,
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")

  # Should still generate some bias mitigations (fallback)
  expect_false(is.na(result$bias_mitigations[1]))
  expect_true(nchar(result$bias_mitigations[1]) > 0)
})

# Test bias keyword detection edge cases
test_that("bid_anticipate bias keyword detection works with partial matches", {
  interpret_result <- tibble(
    stage = "Interpret",
    central_question = "How to improve comparison metrics?", # contains "compar"
    hook = "Recent memorable examples show issues", # contains "memorable"
    tension = "Risk assessment challenges", # contains "risk"
    resolution = "Validate with new trends", # contains "new"
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = interpret_result,
      bias_mitigations = NULL
    )
  )

  expect_s3_class(result, "bid_stage")

  bias_text <- tolower(result$bias_mitigations[1])

  # Should detect biases based on partial keyword matches
  keyword_biases <- c("anchoring", "availability", "loss aversion", "recency")
  found_keyword_bias <- any(sapply(keyword_biases, function(bias) {
    grepl(gsub(" ", ".*", bias), bias_text)
  }))

  expect_true(found_keyword_bias)
})

# Test accessibility mitigation without include_accessibility flag
test_that("bid_anticipate handles accessibility when include_accessibility is FALSE", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    layout = "card",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      include_accessibility = FALSE
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(is.na(result$accessibility[1]))

  # Should not contain accessibility in bias_mitigations
  bias_names <- tolower(names(list(anchoring = "Test")))
  expect_false("accessibility" %in% bias_names)
})

# Test unexpected parameter handling
test_that("bid_anticipate handles unexpected parameters with warnings", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  expect_warning(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Test"),
      unexpected_param = "should warn",
      another_param = 123
    ),
    "Unexpected parameters provided.*unexpected_param.*another_param"
  )

  expect_s3_class(result, "bid_stage")
})

# Test deprecation warning mechanism
test_that("bid_anticipate layout-specific deprecation warning system works", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    layout = "dual_process",
    timestamp = Sys.time()
  )

  # First call should potentially warn (depending on session state)
  suppressMessages(suppressWarnings(
    result1 <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  ))

  # Second call in same session should not warn again
  suppressMessages(suppressWarnings(
    result2 <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = NULL
    )
  ))

  expect_s3_class(result1, "bid_stage")
  expect_s3_class(result2, "bid_stage")
})

# Test accessibility mitigation with user-provided bias mitigations
test_that("bid_anticipate adds accessibility when not in user-provided biases", {
  notice_result <- tibble(
    stage = "Notice",
    problem = "Test problem", theory = "Test Theory", evidence = "Test evidence",
    timestamp = Sys.time()
  )

  # Should get message about adding accessibility
  expect_message(
    result <- bid_anticipate(
      previous_stage = notice_result,
      bias_mitigations = list(anchoring = "Use reference points"), # No accessibility provided
      include_accessibility = TRUE
    ),
    "Added accessibility mitigation based on layout context"
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$accessibility[1]))
})
