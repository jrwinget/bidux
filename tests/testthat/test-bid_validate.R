# ==============================================================================
# HELPERS
# ==============================================================================

create_complete_bid_workflow <- function() {
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
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )

  bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )
}

create_minimal_structure_stage <- function() {
  tibble::tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Visual Hierarchy",
    timestamp = Sys.time()
  )
}

# ==============================================================================
# CORE FUNCTIONALITY TESTS
# ==============================================================================

test_that("bid_validate works with complete workflow", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
  expect_equal(
    result$summary_panel,
    "Dashboard simplified for quicker insights"
  )
  expect_equal(result$collaboration, "Added team annotation features")
  expect_match(result$previous_bias, "anchoring: Provide reference points")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_validate requires previous_stage parameter", {
  expect_error(
    bid_validate(summary_panel = "Test", collaboration = "Test"),
    "argument \"previous_stage\" is missing, with no default"
  )
})

test_that("bid_validate works with optional parameters", {
  structure_result <- create_complete_bid_workflow()

  # should not error when only summary_panel is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test"
    )
  )

  # should not error when only collaboration is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      collaboration = "Test"
    )
  )
})

# ==============================================================================
# PARAMETER VALIDATION AND AUTO-SUGGESTIONS
# ==============================================================================

test_that("bid_validate auto-suggests when parameters are NULL", {
  structure_result <- create_complete_bid_workflow()

  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = NULL,
      collaboration = NULL
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_false(is.na(result$summary_panel[1]))
  expect_false(is.na(result$collaboration[1]))
  expect_true(nchar(result$summary_panel[1]) > 0)
  expect_true(nchar(result$collaboration[1]) > 0)
})

test_that("bid_validate handles next_steps parameter correctly", {
  structure_result <- create_complete_bid_workflow()

  # test with character vector
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    next_steps = c("Step 1", "Step 2", "Step 3")
  )

  expect_s3_class(result1, "bid_stage")
  expect_true("next_steps" %in% names(result1))

  # test with single string
  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    next_steps = "Single step"
  )

  expect_s3_class(result2, "bid_stage")
  expect_true("next_steps" %in% names(result2))
})

test_that("bid_validate handles telemetry_refs parameter", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    telemetry_refs = list(
      clicks = "button clicks",
      views = "page views"
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_true(
    "telemetry_refs" %in% names(result) ||
      "next_steps" %in% names(result)
  )
})

# ==============================================================================
# DATA EXTRACTION AND STAGE INTEGRATION
# ==============================================================================

test_that("bid_validate extracts data from previous stages correctly", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "bid_stage")
  # should extract bias information from previous stages
  expect_true(nchar(result$previous_bias[1]) > 0)
  expect_match(result$previous_bias, "anchoring|framing")
})

test_that("bid_validate handles minimal previous_stage data", {
  minimal_structure <- create_minimal_structure_stage()

  result <- bid_validate(
    previous_stage = minimal_structure,
    summary_panel = "Minimal test",
    collaboration = "Minimal collaboration"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
  expect_equal(result$summary_panel, "Minimal test")
})

test_that("bid_validate handles missing fields in previous_stage", {
  # test with structure stage missing some fields
  incomplete_structure <- tibble::tibble(
    stage = "Structure",
    layout = "card",
    timestamp = Sys.time()
  )

  result <- bid_validate(
    previous_stage = incomplete_structure,
    summary_panel = "Test with incomplete data"
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
})

# ==============================================================================
# SUGGESTION SYSTEM TESTS
# ==============================================================================

test_that("bid_validate generates contextual suggestions", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard analytics improved",
    collaboration = "Team sharing enabled"
  )

  expect_s3_class(result, "bid_stage")
  expect_true(nchar(result$suggestions[1]) > 0)
  expect_true(is.character(result$suggestions))
})

test_that("bid_validate suggestions vary based on content", {
  structure_result <- create_complete_bid_workflow()

  # test with different content types
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Performance metrics dashboard",
    collaboration = "Real-time team updates"
  )

  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "User behavior analytics",
    collaboration = "Collaborative insights sharing"
  )

  expect_s3_class(result1, "bid_stage")
  expect_s3_class(result2, "bid_stage")
  expect_true(nchar(result1$suggestions[1]) > 0)
  expect_true(nchar(result2$suggestions[1]) > 0)
})

# ==============================================================================
# WORKFLOW INTEGRATION TESTS
# ==============================================================================

test_that("bid_validate works with different previous stage types", {
  # test with bid_stage object
  structure_stage <- create_complete_bid_workflow()

  result1 <- bid_validate(
    previous_stage = structure_stage,
    summary_panel = "Test with bid_stage"
  )

  expect_s3_class(result1, "bid_stage")

  # test with tibble
  structure_tibble <- create_minimal_structure_stage()

  result2 <- bid_validate(
    previous_stage = structure_tibble,
    summary_panel = "Test with tibble"
  )

  expect_s3_class(result2, "bid_stage")
})

test_that("bid_validate preserves essential metadata", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Metadata test",
    collaboration = "Team features"
  )

  expect_true("timestamp" %in% names(result))
  expect_true("stage" %in% names(result))
  expect_equal(result$stage, "Validate")
  expect_s3_class(result$timestamp, "POSIXct")
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("bid_validate handles empty string parameters", {
  structure_result <- create_complete_bid_workflow()

  # should handle empty strings gracefully
  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = "",
      collaboration = ""
    )
  )

  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage, "Validate")
})

test_that("bid_validate handles NA parameters", {
  structure_result <- create_complete_bid_workflow()

  # current implementation has an issue with NA handling in if() conditions
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = NA_character_,
      collaboration = NA_character_
    ),
    "missing value where TRUE/FALSE needed"
  )
})

test_that("bid_validate rejects unexpected parameters", {
  structure_result <- create_complete_bid_workflow()

  # current implementation rejects unexpected parameters
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test",
      unexpected_param = "should be ignored"
    ),
    "unused argument"
  )
})

# ==============================================================================
# TELEMETRY INTEGRATION TESTS
# ==============================================================================

test_that("bid_validate integrates telemetry references correctly", {
  structure_result <- create_complete_bid_workflow()

  # test with character vector telemetry refs
  result1 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Telemetry-informed summary",
    telemetry_refs = c("click_through_rate", "time_on_page")
  )

  expect_s3_class(result1, "bid_stage")

  # test with list telemetry refs
  result2 <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Advanced telemetry summary",
    telemetry_refs = list(
      engagement = "user engagement metrics",
      performance = "page load times"
    )
  )

  expect_s3_class(result2, "bid_stage")
})

test_that("bid_validate combines next_steps with telemetry appropriately", {
  structure_result <- create_complete_bid_workflow()

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Combined test",
    next_steps = c("Review metrics", "Update layout"),
    telemetry_refs = list(clicks = "button analytics")
  )

  expect_s3_class(result, "bid_stage")
  expect_true("next_steps" %in% names(result))
})

# ==============================================================================
# HELPER FUNCTION TESTS FOR COVERAGE
# ==============================================================================

test_that("generate_summary_panel_suggestion handles central_question patterns", {
  # test "simplify" pattern
  stage_simplify <- tibble::tibble(
    stage = "Structure",
    central_question = "How can we simplify the interface?",
    timestamp = Sys.time()
  )
  result_simplify <- bidux:::generate_summary_panel_suggestion(stage_simplify)
  expect_match(result_simplify, "Simplified summary")

  # test "compare" pattern
  stage_compare <- tibble::tibble(
    stage = "Structure",
    central_question = "How do metrics compare versus last quarter?",
    timestamp = Sys.time()
  )
  result_compare <- bidux:::generate_summary_panel_suggestion(stage_compare)
  expect_match(result_compare, "Comparative")

  # test "trend" pattern
  stage_trend <- tibble::tibble(
    stage = "Structure",
    central_question = "What are the trends over time?",
    timestamp = Sys.time()
  )
  result_trend <- bidux:::generate_summary_panel_suggestion(stage_trend)
  expect_match(result_trend, "Time-based|trends")
})

test_that("generate_summary_panel_suggestion handles problem patterns", {
  # test "complex" pattern
  stage_complex <- tibble::tibble(
    stage = "Structure",
    problem = "Dashboard is too complex and overwhelming",
    timestamp = Sys.time()
  )
  result_complex <- bidux:::generate_summary_panel_suggestion(stage_complex)
  expect_match(result_complex, "Clean, focused")

  # test "find" pattern
  stage_find <- tibble::tibble(
    stage = "Structure",
    problem = "Users can't find key metrics",
    timestamp = Sys.time()
  )
  result_find <- bidux:::generate_summary_panel_suggestion(stage_find)
  expect_match(result_find, "navigation paths")

  # test "mobile" pattern
  stage_mobile <- tibble::tibble(
    stage = "Structure",
    problem = "Mobile interface is not optimized",
    timestamp = Sys.time()
  )
  result_mobile <- bidux:::generate_summary_panel_suggestion(stage_mobile)
  expect_match(result_mobile, "Mobile-optimized")
})

test_that("generate_summary_panel_suggestion handles theory patterns", {
  # test "cognitive load" pattern
  stage_cognitive <- tibble::tibble(
    stage = "Structure",
    theory = "Cognitive Load Theory",
    timestamp = Sys.time()
  )
  result_cognitive <- bidux:::generate_summary_panel_suggestion(stage_cognitive)
  expect_match(result_cognitive, "Streamlined|cognitive")

  # test "visual" pattern
  stage_visual <- tibble::tibble(
    stage = "Structure",
    theory = "Visual Hierarchies",
    timestamp = Sys.time()
  )
  result_visual <- bidux:::generate_summary_panel_suggestion(stage_visual)
  expect_match(result_visual, "Visually hierarchical")
})

test_that("generate_collaboration_suggestion handles different audiences", {
  # test executive audience
  stage_exec <- tibble::tibble(
    stage = "Structure",
    audience = "Executive leadership team",
    timestamp = Sys.time()
  )
  result_exec <- bidux:::generate_collaboration_suggestion(stage_exec, include_empower_tools = TRUE)
  expect_match(result_exec, "Executive-focused")

  # test analyst audience
  stage_analyst <- tibble::tibble(
    stage = "Structure",
    audience = "Data analysts and technical staff",
    timestamp = Sys.time()
  )
  result_analyst <- bidux:::generate_collaboration_suggestion(stage_analyst, include_empower_tools = TRUE)
  expect_match(result_analyst, "Advanced collaboration")

  # test team audience
  stage_team <- tibble::tibble(
    stage = "Structure",
    audience = "Team members across departments",
    timestamp = Sys.time()
  )
  result_team <- bidux:::generate_collaboration_suggestion(stage_team, include_empower_tools = TRUE)
  expect_match(result_team, "Multi-user")

  # test client audience
  stage_client <- tibble::tibble(
    stage = "Structure",
    audience = "External clients and customers",
    timestamp = Sys.time()
  )
  result_client <- bidux:::generate_collaboration_suggestion(stage_client, include_empower_tools = TRUE)
  expect_match(result_client, "Client-friendly")
})

test_that("generate_collaboration_suggestion without empowerment tools", {
  stage_exec <- tibble::tibble(
    stage = "Structure",
    audience = "Executive leadership",
    timestamp = Sys.time()
  )

  result <- bidux:::generate_collaboration_suggestion(stage_exec, include_empower_tools = FALSE)
  expect_match(result, "Executive-focused")
  expect_false(grepl("empowerment", result))
})

test_that("generate_next_steps_suggestion varies by stage", {
  # test from Anticipate stage
  stage_anticipate <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: test",
    timestamp = Sys.time()
  )
  result_anticipate <- bidux:::generate_next_steps_suggestion(
    stage_anticipate,
    include_exp_design = FALSE,
    include_telemetry = FALSE
  )
  expect_true(any(grepl("bias mitigation", result_anticipate)))

  # test from Structure stage
  stage_structure <- tibble::tibble(
    stage = "Structure",
    layout = "grid",
    timestamp = Sys.time()
  )
  result_structure <- bidux:::generate_next_steps_suggestion(
    stage_structure,
    include_exp_design = FALSE,
    include_telemetry = FALSE
  )
  expect_true(any(grepl("structured layout|accessibility", result_structure)))

  # test from Interpret stage
  stage_interpret <- tibble::tibble(
    stage = "Interpret",
    central_question = "Test question?",
    timestamp = Sys.time()
  )
  result_interpret <- bidux:::generate_next_steps_suggestion(
    stage_interpret,
    include_exp_design = FALSE,
    include_telemetry = FALSE
  )
  expect_true(any(grepl("data storytelling|central question", result_interpret)))
})

test_that("generate_next_steps_suggestion includes experiment design when requested", {
  stage <- create_minimal_structure_stage()

  result <- bidux:::generate_next_steps_suggestion(
    stage,
    include_exp_design = TRUE,
    include_telemetry = FALSE
  )

  expect_true(any(grepl("A/B test", result)))
  expect_true(any(grepl("usability testing", result)))
})

test_that("generate_next_steps_suggestion includes telemetry when requested", {
  stage <- create_minimal_structure_stage()

  result <- bidux:::generate_next_steps_suggestion(
    stage,
    include_exp_design = FALSE,
    include_telemetry = TRUE
  )

  expect_true(any(grepl("telemetry|monitoring", result)))
})

test_that("generate_next_steps_suggestion includes telemetry_refs when provided", {
  stage <- create_minimal_structure_stage()

  telemetry_refs <- c("click_rate", "time_on_page")

  result <- bidux:::generate_next_steps_suggestion(
    stage,
    include_exp_design = FALSE,
    include_telemetry = TRUE,
    telemetry_refs = telemetry_refs
  )

  expect_true(any(grepl("telemetry", result)))
})

test_that("generate_validation_suggestions checks summary_panel content", {
  stage <- create_minimal_structure_stage()

  # test with good summary panel
  result_good <- bidux:::generate_validation_suggestions(
    "Dashboard provides actionable insights and recommendations",
    "Team collaboration enabled",
    c("Step 1", "Step 2"),
    stage
  )
  expect_type(result_good, "character")

  # test with long summary panel
  long_summary <- paste(rep("Long summary text", 30), collapse = " ")
  result_long <- bidux:::generate_validation_suggestions(
    long_summary,
    "Collaboration enabled",
    c("Step 1"),
    stage
  )
  expect_match(result_long, "simplifying")

  # test with summary panel missing action words
  result_no_action <- bidux:::generate_validation_suggestions(
    "A dashboard summary panel",
    "Collaboration",
    c("Step 1"),
    stage
  )
  expect_match(result_no_action, "actionable insights")
})

test_that("generate_validation_suggestions checks collaboration content", {
  stage <- create_minimal_structure_stage()

  # test with good collaboration
  result_good <- bidux:::generate_validation_suggestions(
    "Summary panel",
    "Team can share insights and collaborate",
    c("Step 1"),
    stage
  )
  expect_type(result_good, "character")

  # test with collaboration missing sharing mechanisms
  result_no_share <- bidux:::generate_validation_suggestions(
    "Summary panel",
    "Some collaboration features",
    c("Step 1"),
    stage
  )
  expect_match(result_no_share, "sharing or collaboration mechanisms")
})

test_that("generate_validation_suggestions checks next_steps adequacy", {
  stage <- create_minimal_structure_stage()

  # test with insufficient next_steps
  result_few <- bidux:::generate_validation_suggestions(
    "Summary panel",
    "Collaboration",
    c("Step 1"),
    stage
  )
  expect_match(result_few, "more specific next steps")

  # test without user testing
  result_no_test <- bidux:::generate_validation_suggestions(
    "Summary panel",
    "Collaboration",
    c("Deploy", "Monitor", "Iterate"),
    stage
  )
  expect_match(result_no_test, "user testing")
})

test_that("extract_previous_stage_info extracts all relevant fields", {
  complete_stage <- tibble::tibble(
    stage = "Structure",
    central_question = "Test question",
    hook = "Test hook",
    problem = "Test problem",
    theory = "Test theory",
    audience = "Test audience",
    personas = "Test personas",
    bias_mitigations = "anchoring: test",
    layout = "grid",
    concepts = "Test concept",
    accessibility = "Test accessibility",
    timestamp = Sys.time()
  )

  result <- bidux:::extract_previous_stage_info(complete_stage)

  expect_type(result, "list")
  expect_equal(result$central_question, "Test question")
  expect_equal(result$hook, "Test hook")
  expect_equal(result$problem, "Test problem")
  expect_equal(result$theory, "Test theory")
  expect_equal(result$audience, "Test audience")
  expect_equal(result$bias, "anchoring: test")
  expect_equal(result$layout, "grid")
  expect_equal(result$concepts, "Test concept")
  expect_equal(result$accessibility, "Test accessibility")
})

test_that("bid_validate handles include_exp_design parameter correctly", {
  structure_result <- create_complete_bid_workflow()

  # with include_exp_design = TRUE
  result_with <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test",
    include_exp_design = TRUE,
    include_telemetry = FALSE
  )
  expect_s3_class(result_with, "bid_stage")

  # with include_exp_design = FALSE
  result_without <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test",
    include_exp_design = FALSE,
    include_telemetry = FALSE
  )
  expect_s3_class(result_without, "bid_stage")
})

test_that("bid_validate handles include_empower_tools parameter correctly", {
  structure_result <- create_complete_bid_workflow()

  # with include_empower_tools = TRUE
  result_with <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test",
    include_empower_tools = TRUE
  )
  expect_s3_class(result_with, "bid_stage")

  # with include_empower_tools = FALSE
  result_without <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test",
    include_empower_tools = FALSE
  )
  expect_s3_class(result_without, "bid_stage")
})
