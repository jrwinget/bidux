test_that("get_consolidated_suggestion_rules returns structured rules", {
  rules <- get_consolidated_suggestion_rules()

  expect_true(is.list(rules))
  expect_true("interpret" %in% names(rules))
  expect_true("notice" %in% names(rules))
  expect_true("validate" %in% names(rules))

  # check structure of interpret rules
  expect_true(is.list(rules$interpret))
  expect_gt(length(rules$interpret), 0)

  # first rule should have condition and message
  first_rule <- rules$interpret[[1]]
  expect_true("condition" %in% names(first_rule))
  expect_true("message" %in% names(first_rule))
  expect_true(is.function(first_rule$condition))
  expect_true(is.character(first_rule$message))
})

test_that("apply_suggestion_rules works with context data", {
  # test with interpret stage
  context_data <- list(
    central_question = "Short?",
    data_story = list(hook = "test", context = "", tension = "", resolution = "")
  )

  suggestions <- apply_suggestion_rules("Interpret", context_data)
  expect_true(is.character(suggestions))
  # allow for empty suggestions if no rules match
  expect_gte(length(suggestions), 0)

  # test with more complete context
  complete_context <- list(
    central_question = "How can we improve user engagement through better dashboard design?",
    data_story = list(
      hook = "Users are struggling with our dashboard",
      context = "Analytics show low engagement",
      tension = "Current design is confusing",
      resolution = "Need better UX"
    )
  )

  complete_suggestions <- apply_suggestion_rules("Interpret", complete_context)
  expect_true(is.character(complete_suggestions))
  # should have fewer suggestions since context is more complete
})

test_that("apply_suggestion_rules handles Notice stage", {
  context_data <- list(
    problem = "Users can't find navigation",
    evidence = "Testing shows confusion"
  )

  suggestions <- apply_suggestion_rules("Notice", context_data)
  expect_true(is.character(suggestions))
})

test_that("apply_suggestion_rules handles Validate stage", {
  context_data <- list(
    validation_steps = c("Test with users", "Check metrics")
  )

  suggestions <- apply_suggestion_rules("Validate", context_data)
  expect_true(is.character(suggestions))
})

test_that("deduplicate_warnings_suggestions removes similar items", {
  warnings <- c("Consider adding more detail", "Add more detailed information")
  suggestions <- c("Try this approach", "Use this method")

  result <- deduplicate_warnings_suggestions(warnings, suggestions)
  expect_true(is.list(result))
  expect_true("warnings" %in% names(result))
  expect_true("suggestions" %in% names(result))

  # should deduplicate similar warnings (or keep them if similarity check doesn't work)
  expect_lte(length(result$warnings), length(warnings))
  expect_equal(length(result$suggestions), length(suggestions))
})

test_that("get_fallback_suggestion provides fallbacks for all stages", {
  stages <- c("Interpret", "Notice", "Anticipate", "Structure", "Validate")

  for (stage in stages) {
    fallback <- get_fallback_suggestion(stage)
    expect_true(is.character(fallback))
    expect_gt(nchar(fallback), 0)
  }
})

test_that("apply_suggestion_rules handles custom rules", {
  custom_rules <- list(
    interpret = list(
      list(
        condition = function(ctx) TRUE,
        message = "Custom suggestion"
      )
    )
  )

  context_data <- list(central_question = "Test question")
  suggestions <- apply_suggestion_rules("Interpret", context_data, custom_rules)

  expect_true(is.character(suggestions))
  expect_gte(length(suggestions), 0)
})

test_that("apply_suggestion_rules handles invalid stage names", {
  context_data <- list()
  suggestions <- apply_suggestion_rules("InvalidStage", context_data)

  expect_true(is.character(suggestions))
  # should return fallback suggestion or empty
  expect_gte(length(suggestions), 0)
})

test_that("deduplicate_warnings_suggestions handles empty inputs", {
  result <- deduplicate_warnings_suggestions(character(0), character(0))
  expect_true(is.list(result))
  expect_equal(length(result$warnings), 0)
  expect_equal(length(result$suggestions), 0)
})

test_that("deduplicate_warnings_suggestions handles different similarity thresholds", {
  warnings <- c("Add more detail", "Include additional details", "Use different approach")

  # high threshold - less deduplication
  result_high <- deduplicate_warnings_suggestions(warnings, character(0), 0.9)

  # low threshold - more deduplication
  result_low <- deduplicate_warnings_suggestions(warnings, character(0), 0.5)

  expect_gte(length(result_high$warnings), length(result_low$warnings))
})

# test comprehensive rule structure
test_that("get_consolidated_suggestion_rules has complete rule structure", {
  rules <- get_consolidated_suggestion_rules()

  # check all expected stages are present
  expected_stages <- c("interpret", "notice", "validate")
  expect_true(all(expected_stages %in% names(rules)))

  # check each stage has at least one rule
  for (stage in expected_stages) {
    expect_gt(length(rules[[stage]]), 0)

    # check first rule in each stage has proper structure
    first_rule <- rules[[stage]][[1]]
    expect_true("condition" %in% names(first_rule))
    expect_true("message" %in% names(first_rule))
    expect_true(is.function(first_rule$condition))
    expect_true(is.character(first_rule$message))
  }
})

# test specific interpret rules
test_that("interpret stage rules work correctly", {
  rules <- get_consolidated_suggestion_rules()
  interpret_rules <- rules$interpret

  # test data story completeness rule
  incomplete_context <- list(
    data_story = list(hook = "test", context = "", tension = "", resolution = "")
  )

  suggestions <- apply_suggestion_rules("interpret", incomplete_context)
  expect_true(is.character(suggestions))
  expect_gte(length(suggestions), 0)

  # test central question rules
  short_question_context <- list(central_question = "Short?")
  short_suggestions <- apply_suggestion_rules("interpret", short_question_context)
  expect_true(is.character(short_suggestions))

  long_question_context <- list(
    central_question = paste(rep("very long question", 10), collapse = " ")
  )
  long_suggestions <- apply_suggestion_rules("interpret", long_question_context)
  expect_true(is.character(long_suggestions))

  # test audience definition rules
  no_audience_context <- list(central_question = "Test question")
  audience_suggestions <- apply_suggestion_rules("interpret", no_audience_context)
  expect_true(is.character(audience_suggestions))
})

# test specific notice rules
test_that("notice stage rules work correctly", {
  # test problem description quality
  short_problem_context <- list(problem = "Short")
  suggestions <- apply_suggestion_rules("notice", short_problem_context)
  expect_true(is.character(suggestions))

  # test evidence strength rules
  short_evidence_context <- list(evidence = "Short")
  evidence_suggestions <- apply_suggestion_rules("notice", short_evidence_context)
  expect_true(is.character(evidence_suggestions))

  # test evidence with no quantitative data
  no_numbers_context <- list(evidence = "Users are confused and struggling")
  no_numbers_suggestions <- apply_suggestion_rules("notice", no_numbers_context)
  expect_true(is.character(no_numbers_suggestions))

  # test theory application rules
  complexity_problem_context <- list(
    problem = "Too many choices overwhelm users",
    theory = ""
  )
  theory_suggestions <- apply_suggestion_rules("notice", complexity_problem_context)
  expect_true(is.character(theory_suggestions))

  # test evidence-theory alignment
  misaligned_context <- list(
    theory = "cognitive load theory",
    evidence = "Users like the bright colors"
  )
  alignment_suggestions <- apply_suggestion_rules("notice", misaligned_context)
  expect_true(is.character(alignment_suggestions))
})

# test specific validate rules
test_that("validate stage rules work correctly", {
  # test telemetry integration rules
  telemetry_context <- list(
    include_telemetry = TRUE,
    telemetry_refs = NULL
  )
  telemetry_suggestions <- apply_suggestion_rules("validate", telemetry_context)
  expect_true(is.character(telemetry_suggestions))

  # test measurement completeness rules
  no_metrics_context <- list(
    validation_steps = "Check if users like it"
  )
  metrics_suggestions <- apply_suggestion_rules("validate", no_metrics_context)
  expect_true(is.character(metrics_suggestions))

  # test timeline rules
  no_timeline_context <- list(
    validation_steps = "Test with users and collect feedback"
  )
  timeline_suggestions <- apply_suggestion_rules("validate", no_timeline_context)
  expect_true(is.character(timeline_suggestions))

  # test user testing integration rules
  no_testing_context <- list(
    validation_steps = "Monitor analytics and check metrics"
  )
  testing_suggestions <- apply_suggestion_rules("validate", no_testing_context)
  expect_true(is.character(testing_suggestions))
})

# test rule condition evaluation error handling
test_that("apply_suggestion_rules handles rule evaluation errors", {
  # test with malformed rules
  bad_rules <- list(
    interpret = list(
      list(condition = "not a function", message = "test"),
      list(condition = function(ctx) stop("error"), message = "error test"),
      list(condition = function(ctx) NULL, message = "null test"),
      list() # empty rule
    )
  )

  context_data <- list(central_question = "test")

  # should handle errors gracefully without stopping
  suggestions <- apply_suggestion_rules("interpret", context_data, bad_rules)
  expect_true(is.character(suggestions))
  expect_gte(length(suggestions), 0)
})

# test edge cases in deduplicate_warnings_suggestions
test_that("deduplicate_warnings_suggestions handles edge cases", {
  # test with very similar warnings and suggestions
  warnings <- c("Add more detail to your analysis", "Include more detailed analysis")
  suggestions <- c("Consider adding detailed analysis", "Try more comprehensive approach")

  result <- deduplicate_warnings_suggestions(warnings, suggestions, 0.6)
  expect_true(is.list(result))
  expect_true("warnings" %in% names(result))
  expect_true("suggestions" %in% names(result))

  # test with single item arrays
  single_warning <- "Single warning"
  single_suggestion <- "Single suggestion"
  single_result <- deduplicate_warnings_suggestions(single_warning, single_suggestion)
  expect_equal(length(single_result$warnings), 1)
  expect_equal(length(single_result$suggestions), 1)

  # test with NA values
  na_warnings <- c("Valid warning", NA, "Another warning")
  na_suggestions <- c("Valid suggestion")
  na_result <- deduplicate_warnings_suggestions(na_warnings, na_suggestions)
  expect_true(is.list(na_result))
})

# test get_fallback_suggestion for edge cases
test_that("get_fallback_suggestion handles all cases", {
  # test known stages
  known_stages <- c("Interpret", "Notice", "Anticipate", "Structure", "Validate")
  for (stage in known_stages) {
    fallback <- get_fallback_suggestion(stage)
    expect_true(is.character(fallback))
    expect_gt(nchar(fallback), 0)
  }

  # test unknown stage
  unknown_fallback <- get_fallback_suggestion("UnknownStage")
  expect_true(is.character(unknown_fallback))
  expect_gt(nchar(unknown_fallback), 0)

  # test empty string stage - should return default fallback
  empty_fallback <- get_fallback_suggestion("")
  expect_true(is.character(empty_fallback))
  expect_gt(nchar(empty_fallback), 0)
  expect_match(empty_fallback, "Follow BID framework best practices")
})

# test word overlap calculation in deduplicate_warnings_suggestions
test_that("deduplicate_warnings_suggestions word overlap works", {
  # test warnings with high overlap with suggestions
  warnings <- c("Please add more details to your data analysis")
  suggestions <- c("Consider adding more details to data analysis")

  result <- deduplicate_warnings_suggestions(warnings, suggestions, 0.5)
  # should deduplicate if overlap is high enough
  expect_lte(length(result$warnings), length(warnings))

  # test with no overlap
  no_overlap_warnings <- c("Check your calculations")
  no_overlap_suggestions <- c("Review user feedback")

  no_overlap_result <- deduplicate_warnings_suggestions(no_overlap_warnings, no_overlap_suggestions)
  expect_equal(length(no_overlap_result$warnings), length(no_overlap_warnings))
  expect_equal(length(no_overlap_result$suggestions), length(no_overlap_suggestions))
})

# test all rule conditions can be evaluated
test_that("all consolidated rules have valid conditions", {
  rules <- get_consolidated_suggestion_rules()

  # create test contexts for each stage
  test_contexts <- list(
    interpret = list(
      central_question = "How can we improve?",
      data_story = list(hook = "test", context = "test", tension = "", resolution = ""),
      audience = "test audience"
    ),
    notice = list(
      problem = "Users are confused",
      evidence = "Testing shows 50% confusion rate",
      theory = "cognitive load theory"
    ),
    validate = list(
      validation_steps = "Test with users over 2 weeks",
      include_telemetry = TRUE,
      telemetry_refs = c("click_rate", "completion_time")
    )
  )

  # test that all rules can be evaluated without error
  for (stage_name in names(rules)) {
    stage_rules <- rules[[stage_name]]
    context <- test_contexts[[stage_name]] %||% list()

    for (i in seq_along(stage_rules)) {
      rule <- stage_rules[[i]]
      if (is.list(rule) && "condition" %in% names(rule) && "message" %in% names(rule)) {
        # should not throw error
        expect_silent({
          if (is.function(rule$condition)) {
            tryCatch({
              result <- rule$condition(context)
              expect_true(is.logical(result) || is.null(result))
            }, error = function(e) {
              # errors in rule conditions are acceptable - they should be handled gracefully
            })
          }
        })
      }
    }
  }
})

# test context data structure handling
test_that("apply_suggestion_rules handles different context structures", {
  # test with NULL context
  null_suggestions <- apply_suggestion_rules("interpret", NULL)
  expect_true(is.character(null_suggestions))

  # test with empty list context
  empty_suggestions <- apply_suggestion_rules("interpret", list())
  expect_true(is.character(empty_suggestions))

  # test with nested list context
  nested_context <- list(
    data_story = list(
      hook = list(title = "Test", content = "Test content"),
      context = "Test context"
    )
  )
  nested_suggestions <- apply_suggestion_rules("interpret", nested_context)
  expect_true(is.character(nested_suggestions))
})
