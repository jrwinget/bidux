test_that("bid_structure generates suggestions for overload patterns", {
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
  expect_false("layout" %in% names(result))
  expect_true("suggestions" %in% names(result))
  expect_true(length(result$suggestions) > 0)
})


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

  # check basic structure
  expect_s3_class(result, "bid_stage")
  expect_equal(result$stage[1], "Structure")
  expect_false("layout" %in% names(result))
  expect_true("suggestions" %in% names(result))
  expect_true("concepts" %in% names(result))

  # check suggestions structure
  suggestions <- result$suggestions
  expect_true(is.list(suggestions))
  expect_true(length(suggestions) > 0)

  # each suggestion group should have concept and suggestions
  for (group in suggestions) {
    expect_true("concept" %in% names(group))
    expect_true("suggestions" %in% names(group))
    expect_true(is.character(group$concept))
    expect_true(is.list(group$suggestions))

    # each individual suggestion should have required fields
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

  # check concepts
  expect_true(is.character(result$concepts))
  expect_true(nchar(result$concepts[1]) > 0)
})

test_that("bid_structure shows appropriate CLI messages", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Dashboard has information overload issues",
    timestamp = Sys.time()
  )

  # should show concept tip message
  expect_message(
    bid_structure(previous_stage),
    "Tip.*bid_concept"
  )
})

test_that("bid_structure generates high-quality suggestions for complex scenario", {
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

  # check that we get expected concept groups
  suggestion_groups <- result$suggestions
  concept_names <- sapply(suggestion_groups, function(g) g$concept)

  # should include these key concepts for this scenario
  expect_true("Cognitive Load Theory" %in% concept_names)
  expect_true("Progressive Disclosure" %in% concept_names)
  expect_true("User Onboarding" %in% concept_names) # due to "first-time users"

  # check that suggestions contain relevant component pointers
  all_suggestions <- unlist(
    lapply(suggestion_groups, function(g) g$suggestions),
    recursive = FALSE
  )
  all_components <- unlist(lapply(all_suggestions, function(s) s$components))

  # should recommend appropriate Shiny/bslib components
  expect_true(any(grepl("bslib::accordion", all_components)))
  expect_true(any(grepl("shiny::", all_components)))

  # check that suggestions are ranked (highest scores first)
  for (group in suggestion_groups) {
    if (length(group$suggestions) > 1) {
      scores <- sapply(group$suggestions, function(s) s$score)
      expect_equal(scores, sort(scores, decreasing = TRUE))
    }
  }

  # verify suggestions have substantive content
  for (group in suggestion_groups) {
    for (suggestion in group$suggestions) {
      expect_true(nchar(suggestion$title) > 5)
      expect_true(nchar(suggestion$details) > 20)
      expect_true(nchar(suggestion$rationale) > 15)
      expect_true(length(suggestion$components) >= 1)
    }
  }
})

# test edge cases and error handling
test_that("bid_structure handles edge cases gracefully", {
  # minimal previous stage
  minimal_stage <- tibble::tibble(
    stage = "Interpret",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(minimal_stage)
  )

  expect_s3_class(result, "bid_stage")
  expect_false("layout" %in% names(result))
  expect_true(length(result$suggestions) > 0)

  # test with custom concepts
  suppressMessages(
    result_custom <- bid_structure(
      minimal_stage,
      concepts = c("Visual Hierarchy")
    )
  )

  expect_s3_class(result_custom, "bid_stage")
  expect_true(grepl("Visual Hierarchy", result_custom$concepts[1]))
})

test_that("concept detection works correctly with different field combinations", {
  # test data story fields for concept detection
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
  expect_s3_class(result, "bid_stage")
  expect_true(length(result$suggestions) > 0)

  # test nested data_story access for audience-based concepts
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
  expect_s3_class(result, "bid_stage")
  expect_true(grepl("User Onboarding", result$concepts[1]))
})

# test telemetry integration
test_that("bid_structure respects telemetry data in scoring", {
  stage_with_telemetry <- tibble::tibble(
    stage = "Interpret",
    problem = "Need progressive disclosure across sections",
    timestamp = Sys.time()
  )

  # create telemetry flags indicating navigation issues
  telemetry_flags <- list(has_navigation_issues = TRUE)

  suppressMessages(
    result <- bid_structure(stage_with_telemetry, telemetry_flags = telemetry_flags)
  )

  # tab-related suggestions should have lower scores
  all_suggestions <- unlist(
    lapply(result$suggestions, function(g) g$suggestions),
    recursive = FALSE
  )
  tab_suggestions <- all_suggestions[sapply(all_suggestions, function(s) {
    grepl("tab", s$details, ignore.case = TRUE)
  })]

  if (length(tab_suggestions) > 0) {
    tab_scores <- sapply(tab_suggestions, function(s) s$score)
    all_scores <- sapply(all_suggestions, function(s) s$score)
    expect_true(mean(tab_scores) < mean(all_scores))
  }
})
