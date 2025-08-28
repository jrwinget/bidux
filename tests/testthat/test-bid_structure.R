# Test bid_structure refactor - comprehensive test coverage
# These tests cover all requirements from claude.md including:
# - Layout heuristics (T1-T7)
# - Error handling for legacy layout parameter (T8)
# - Return structure validation (T9)
# - CLI messages (T10)
# - Suggestions quality (T11)

# T1: Overload → breathable
test_that("bid_structure selects breathable layout for overload patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users are overwhelmed by too many options in the interface",
    evidence = "Cognitive load is causing confusion and errors",
    central_question = "How to reduce information overload?",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Structure")
  expect_equal(result$layout[1], "breathable")
  expect_true("suggestions" %in% names(result))
  expect_true(length(result$suggestions) > 0)
})

# T2: Overview vs detail → dual_process
test_that("bid_structure selects dual_process layout for overview vs detail patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users need both summary overview and detailed analysis",
    central_question = "How to provide quick vs thorough access?",
    hook = "Two modes of interaction needed",
    context = "Users want to see at a glance but also dig deeper",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "dual_process")
  expect_true(length(result$suggestions) > 0)
})

# T3: Grouping → grid
test_that("bid_structure selects grid layout for grouping patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret", 
    problem = "Related metrics need better visual hierarchy",
    evidence = "Users want to group and compare panels",
    resolution = "Use proximity to show relationships",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "grid")
})

# T4: Chunked items → card
test_that("bid_structure selects card layout for modular content patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Dashboard needs modular blocks for different data types",
    central_question = "How to organize content in chunks?",
    resolution = "Use cards and tiles for per-item summary",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "card")
})

# T5: Sections/categories → tabs
test_that("bid_structure selects tabs layout for categorical patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Dashboard needs progressive disclosure across sections",
    evidence = "Different categories require module separation",
    context = "Stepwise navigation through different areas",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "tabs")
})

# T6: Tabs suppressed by telemetry → pick non-tabs
test_that("bid_structure avoids tabs when telemetry shows navigation issues", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users need progressive disclosure across sections",
    evidence = "Categories require module separation", 
    telemetry = list(nav_dropoff_tabs = TRUE),
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_false(result$layout[1] == "tabs")
  expect_equal(result$layout[1], "grid")  # Should fallback to grid
})

# T7: Fallback → breathable
test_that("bid_structure falls back to breathable for unmatched patterns", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Generic usability issue",
    evidence = "Some minor interface problems",
    central_question = "How to improve the interface?",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "breathable")
})

# T8: Helpful error on legacy layout= via ...
test_that("bid_structure provides helpful error when layout parameter is used", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Test problem",
    timestamp = Sys.time()
  )
  
  expect_error(
    bid_structure(previous_stage, layout = "dual_process"),
    regexp = "layout.*parameter.*was removed.*0\\.2\\.0"
  )
  
  expect_error(
    bid_structure(previous_stage, concepts = NULL, layout = "grid"),
    regexp = "Layout is now auto-selected"
  )
})

# T9: Return shape - stage, layout, suggestions structure
test_that("bid_structure returns correct structure with all required fields", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users are overwhelmed by interface complexity",
    central_question = "How to simplify?",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  # Check basic structure
  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Structure")
  expect_true("layout" %in% names(result))
  expect_true("suggestions" %in% names(result))
  expect_true("concepts_detected" %in% names(result))
  
  # Check layout is valid
  valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")
  expect_true(result$layout[1] %in% valid_layouts)
  
  # Check suggestions structure
  suggestions <- result$suggestions
  expect_true(is.list(suggestions))
  expect_true(length(suggestions) > 0)
  
  # Each suggestion group should have concept and suggestions
  for (group in suggestions) {
    expect_true("concept" %in% names(group))
    expect_true("suggestions" %in% names(group))
    expect_true(is.character(group$concept))
    expect_true(is.list(group$suggestions))
    
    # Each individual suggestion should have required fields
    for (suggestion in group$suggestions) {
      expect_true("title" %in% names(suggestion))
      expect_true("details" %in% names(suggestion))
      expect_true("components" %in% names(suggestion))
      expect_true("rationale" %in% names(suggestion))
      expect_true("score" %in% names(suggestion))
      expect_true(is.numeric(suggestion$score))
      expect_true(suggestion$score >= 0 && suggestion$score <= 1)
    }
  }
  
  # Check concepts_detected
  expect_true(is.character(result$concepts_detected))
  expect_true(length(result$concepts_detected) > 0)
})

# T10: CLI messages - expect messages for auto-selection + hint
test_that("bid_structure shows appropriate CLI messages", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Dashboard has information overload issues",
    timestamp = Sys.time()
  )
  
  # Should show layout selection and rationale messages
  expect_message(
    bid_structure(previous_stage),
    "Auto-selected layout"
  )
  
  expect_message(
    bid_structure(previous_stage),
    "Tip.*bid_concept"
  )
})

# T11: Suggestions quality smoke test
test_that("bid_structure generates high-quality suggestions for complex scenario", {
  # Scenario: overwhelming filters + first-time users (from claude.md)
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "New users are overwhelmed by too many filter options",
    evidence = "High abandonment rate among first-time users",
    central_question = "How to make filtering less overwhelming for beginners?",
    hook = "First-time users struggle with complex interface",
    context = "Dashboard has many advanced filtering options",
    tension = "Power users need advanced filters but newcomers get confused",
    resolution = "Progressive disclosure with smart defaults",
    audience = "Mix of first-time and experienced users",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(previous_stage)
  )
  
  # Should select breathable layout for overload scenario
  expect_equal(result$layout[1], "breathable")
  
  # Check that we get expected concept groups
  suggestion_groups <- result$suggestions
  concept_names <- sapply(suggestion_groups, function(g) g$concept)
  
  # Should include these key concepts for this scenario
  expect_true("Cognitive Load Theory" %in% concept_names)
  expect_true("Progressive Disclosure" %in% concept_names)
  expect_true("User Onboarding" %in% concept_names) # due to "first-time users"
  
  # Check that suggestions contain relevant component pointers
  all_suggestions <- unlist(lapply(suggestion_groups, function(g) g$suggestions), recursive = FALSE)
  all_components <- unlist(lapply(all_suggestions, function(s) s$components))
  
  # Should recommend appropriate Shiny/bslib components
  expect_true(any(grepl("bslib::accordion", all_components)))
  expect_true(any(grepl("shiny::", all_components)))
  
  # Check that suggestions are ranked (highest scores first)
  for (group in suggestion_groups) {
    if (length(group$suggestions) > 1) {
      scores <- sapply(group$suggestions, function(s) s$score)
      expect_equal(scores, sort(scores, decreasing = TRUE))
    }
  }
  
  # Verify suggestions have substantive content
  for (group in suggestion_groups) {
    for (suggestion in group$suggestions) {
      expect_true(nchar(suggestion$title) > 5)
      expect_true(nchar(suggestion$details) > 20)
      expect_true(nchar(suggestion$rationale) > 15)
      expect_true(length(suggestion$components) >= 1)
    }
  }
})

# Test edge cases and error handling
test_that("bid_structure handles edge cases gracefully", {
  # Minimal previous stage
  minimal_stage <- tibble::tibble(
    stage = "Interpret",
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(minimal_stage)
  )
  
  expect_s3_class(result, "bid_stage")
  expect_equal(result$layout[1], "breathable")  # Should fallback
  expect_true(length(result$suggestions) > 0)
  
  # Test with custom concepts
  suppressMessages(
    result_custom <- bid_structure(minimal_stage, concepts = c("Visual Hierarchy"))
  )
  
  expect_s3_class(result_custom, "bid_stage")
  expect_true("Visual Hierarchy" %in% result_custom$concepts_detected)
})

test_that("layout heuristics work correctly with different field combinations", {
  # Test data story fields
  story_stage <- tibble::tibble(
    stage = "Interpret", 
    data_story = list(list(
      hook = "Users need quick access to summaries",
      context = "But also detailed breakdowns when needed", 
      tension = "Two different modes of interaction",
      resolution = "Dual mode interface"
    )),
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(story_stage)
  )
  expect_equal(result$layout[1], "dual_process")
  
  # Test nested data_story access
  nested_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Interface has cluttered design",
    data_story = list(
      audience = "First-time users who get confused easily"
    ),
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(nested_stage)
  )
  expect_equal(result$layout[1], "breathable")
  expect_true("User Onboarding" %in% result$concepts_detected)
})

# Test telemetry integration
test_that("bid_structure respects telemetry data in scoring", {
  stage_with_telemetry <- tibble::tibble(
    stage = "Interpret",
    problem = "Need progressive disclosure across sections",
    telemetry = list(nav_dropoff_tabs = TRUE),
    timestamp = Sys.time()
  )
  
  suppressMessages(
    result <- bid_structure(stage_with_telemetry)
  )
  
  # Should avoid tabs layout
  expect_false(result$layout[1] == "tabs")
  
  # Tab-related suggestions should have lower scores
  all_suggestions <- unlist(lapply(result$suggestions, function(g) g$suggestions), recursive = FALSE)
  tab_suggestions <- all_suggestions[sapply(all_suggestions, function(s) grepl("tab", s$details, ignore.case = TRUE))]
  
  if (length(tab_suggestions) > 0) {
    tab_scores <- sapply(tab_suggestions, function(s) s$score)
    all_scores <- sapply(all_suggestions, function(s) s$score)
    expect_true(mean(tab_scores) < mean(all_scores))
  }
})