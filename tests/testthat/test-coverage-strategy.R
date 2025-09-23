# ==============================================================================
# TELEMETRY EDGE CASES
# ==============================================================================

test_that("telemetry functions handle edge cases and errors", {
  skip_if_no_telemetry_deps()

  # test file not found
  expect_error(
    bid_ingest_telemetry("/nonexistent/path/file.json"),
    "not found"
  )

  # test empty file - this should warn, not error
  empty_file <- tempfile(fileext = ".json")
  file.create(empty_file)
  on.exit(unlink(empty_file))

  expect_warning(
    result <- bid_ingest_telemetry(empty_file),
    "No telemetry events found"
  )

  expect_length(result, 0)
})

test_that("telemetry format detection handles edge cases", {
  # test unsupported file extensions
  unsupported_file <- tempfile(fileext = ".xyz")
  file.create(unsupported_file)
  on.exit(unlink(unsupported_file))

  expect_error(
    bid_ingest_telemetry(unsupported_file),
    "Cannot auto-detect format"
  )
})

test_that("telemetry data normalization handles malformed data", {
  # test malformed JSON
  bad_json_file <- tempfile(fileext = ".json")
  writeLines('{"invalid": json}', bad_json_file)
  on.exit(unlink(bad_json_file))

  expect_warning(
    bid_ingest_telemetry(bad_json_file),
    "No telemetry events found"
  )

  # test empty JSON file
  empty_json_file <- tempfile(fileext = ".json")
  writeLines("[]", empty_json_file)
  on.exit(unlink(empty_json_file), add = TRUE)

  expect_warning(
    result <- bid_ingest_telemetry(empty_json_file),
    "No telemetry events found"
  )

  expect_length(result, 0)
})

# ==============================================================================
# ERROR AND WARNING CODE PATHS
# ==============================================================================

test_that("bid functions handle invalid previous_stage objects", {
  # test invalid previous stage structure
  invalid_stage <- list(not_a_tibble = TRUE)

  expect_error(
    bid_notice(
      previous_stage = invalid_stage,
      problem = "Test",
      evidence = "Test"
    ),
    "Invalid previous_stage format"
  )

  expect_error(
    bid_anticipate(previous_stage = invalid_stage),
    "Invalid previous_stage format"
  )
})

test_that("suggestion system handles missing dependencies", {
  # test suggestion generation when dependencies are missing
  if (exists("generate_stage_suggestions")) {
    # test with empty context
    empty_suggestions <- generate_stage_suggestions("Notice", list())
    expect_type(empty_suggestions, "character")

    # test with invalid stage name
    suppressWarnings({
      invalid_suggestions <- generate_stage_suggestions("InvalidStage", list())
      expect_type(invalid_suggestions, "character")
    })
  } else {
    skip("generate_stage_suggestions not available")
  }
})

# ==============================================================================
# COMPLEX DATA STRUCTURES
# ==============================================================================

test_that("bid functions handle complex data story structures", {
  # test nested data story with complex types
  complex_story <- list(
    hook = "Complex hook",
    context = list(nested = "context"),
    metrics = c(1, 2, 3),
    visual_approach = NA
  )

  result <- bid_interpret(
    central_question = "Complex test?",
    data_story = complex_story
  )

  validate_bid_stage_structure(result, "Interpret")
  expect_equal(result$hook[1], "Complex hook")
})

test_that("persona validation handles edge cases", {
  # test personas with missing recommended fields
  minimal_personas <- list(list(name = "Minimal Persona"))

  suppressWarnings({
    expect_true(validate_user_personas(minimal_personas))
  })

  # test personas with extra fields
  extended_personas <- list(list(
    name = "Extended Persona",
    goals = "Test goals",
    pain_points = "Test pain points",
    technical_level = "Advanced",
    extra_field = "Should be ignored"
  ))

  expect_true(validate_user_personas(extended_personas))
})

# ==============================================================================
# FORMATTING AND DISPLAY EDGE CASES
# ==============================================================================

test_that("formatting functions handle unicode and special characters", {
  unicode_text <- "Test with émojis 🎉 and àccénts"
  expect_type(truncate_text(unicode_text, 20), "character")
  expect_type(normalize_text(unicode_text), "character")

  # test very long text
  long_text <- paste(rep("word", 100), collapse = " ")
  truncated <- truncate_text(long_text, 50)
  expect_true(nchar(truncated) <= 50)
  expect_match(truncated, "\\.\\.\\.$")
})

test_that("print methods handle missing or NA values gracefully", {
  # test stage with many NA values
  sparse_stage <- bid_stage("Notice", tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    theory = NA,
    evidence = NA,
    suggestions = NA,
    timestamp = Sys.time()
  ))

  expect_output(print(sparse_stage), "BID Framework")
  expect_output(print(sparse_stage), "Problem:")
  # should not error on NA values
})

# ==============================================================================
# ACCESSIBILITY AND LAYOUT EDGE CASES
# ==============================================================================

test_that("accessibility functions handle edge cases", {
  # test unknown layout types
  unknown_layout_advice <- get_accessibility_advice("unknown_layout")
  expect_type(unknown_layout_advice, "character")
  expect_gt(nchar(unknown_layout_advice), 0)

  # test NULL layout
  null_layout_advice <- get_accessibility_advice(NULL)
  expect_type(null_layout_advice, "character")
  expect_gt(nchar(null_layout_advice), 0)

  # test complex accessibility structures
  complex_accessibility <- list(
    contrast = "AA",
    keyboard = "full",
    screen_reader = "compatible"
  )

  formatted <- format_accessibility_for_storage(complex_accessibility)
  expect_true(is.character(formatted) || is.na(formatted))
})

# ==============================================================================
# CONCEPT DETECTION AND MATCHING EDGE CASES
# ==============================================================================

test_that("concept detection handles complex text inputs", {
  # test text with no recognizable concepts
  no_concepts_text <- "This text contains no behavioral science concepts whatsoever"
  concepts <- detect_concepts_from_text(no_concepts_text)
  expect_type(concepts, "character")

  # test text with mixed case and punctuation
  mixed_text <- "COGNITIVE Load theory, and Visual-Hierarchy!!! are important."
  mixed_concepts <- detect_concepts_from_text(mixed_text)
  expect_type(mixed_concepts, "character")

  # test very long text
  long_concept_text <- paste(
    "cognitive load theory",
    paste(rep("filler word", 100), collapse = " "),
    "visual hierarchy"
  )
  long_concepts <- detect_concepts_from_text(long_concept_text)
  expect_type(long_concepts, "character")
})

# ==============================================================================
# WORKFLOW STATE AND METADATA EDGE CASES
# ==============================================================================

test_that("workflow state handling covers edge cases", {
  # test workflow with out-of-order stages
  stage1 <- bid_stage("Validate", tibble::tibble(
    stage = "Validate",
    summary_panel = "Test summary",
    timestamp = Sys.time()
  ))

  stage2 <- bid_stage("Notice", tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  ))

  workflow <- bid_result(list(stage1, stage2))
  expect_false(is_complete(workflow))

  # test extraction of non-existent stage
  missing_stage <- extract_stage(workflow, "Interpret")
  expect_null(missing_stage)
})

test_that("metadata generation handles edge cases", {
  # test metadata with unusual custom fields
  unusual_metadata <- get_stage_metadata(1, list(
    unicode_field = "🎉 emoji value",
    null_field = NULL,
    nested_field = list(inner = "value")
  ))

  expect_type(unusual_metadata, "list")
  expect_equal(unusual_metadata$stage_number, 1)
})
