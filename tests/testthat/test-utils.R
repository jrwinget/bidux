# Test helper to create a simple bid_stage for testing
create_test_stage <- function(
    stage_name = "Notice",
    problem = "Test problem",
    evidence = "Test evidence") {
  tibble::tibble(
    stage = stage_name,
    problem = problem,
    evidence = evidence,
    timestamp = Sys.time()
  )
}

# Test fixtures for common data structures
test_personas <- list(
  list(
    name = "Analyst",
    goals = "Analyze data",
    pain_points = "Complex UI",
    technical_level = "Advanced"
  ),
  list(
    name = "Manager",
    goals = "Review reports",
    pain_points = "Time constraints",
    technical_level = "Intermediate"
  )
)

test_concepts_data <- tibble::tibble(
  concept = c(
    "Visual Hierarchy",
    "Cognitive Load Theory",
    "Principle of Proximity"
  )
)

# ==============================================================================
# CORE OPERATORS AND BASIC UTILITIES
# ==============================================================================

test_that("%||% operator works correctly", {
  expect_equal(5 %||% "default", 5)
  expect_equal("hello" %||% "default", "hello")
  expect_equal(NULL %||% "default", "default")
  expect_equal(NA %||% "default", NA) # NA is not NULL
})

test_that("is_empty correctly identifies empty values", {
  # Empty cases
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(c(NA, NA)))

  # Non-empty cases
  expect_false(is_empty("test"))
  expect_false(is_empty("  test  "))
  expect_false(is_empty(0))
  expect_false(is_empty(FALSE))
})

test_that("truncate_text handles various inputs correctly", {
  expect_equal(truncate_text("short", 10), "short")
  expect_equal(truncate_text("this is a very long text", 10), "this is...")
  expect_equal(truncate_text(NULL, 10), "")
  expect_equal(truncate_text(NA, 10), "")
  expect_equal(truncate_text("exactly10!", 10), "exactly10!")
  expect_equal(truncate_text("exactly11!!", 10), "exactly...")
  expect_equal(truncate_text("abc", 3), "abc") # Exactly at limit
  expect_equal(truncate_text("abcd", 3), "...") # One over limit
})

# ==============================================================================
# QUIET MODE FUNCTIONS
# ==============================================================================

test_that("quiet mode functions work correctly", {
  # Store original setting
  original_quiet <- getOption("bidux.quiet", FALSE)
  on.exit(options(bidux.quiet = original_quiet))

  # Test bid_set_quiet
  old_value <- bid_set_quiet(TRUE)
  expect_equal(bid_get_quiet(), TRUE)
  expect_equal(old_value, original_quiet)

  bid_set_quiet(FALSE)
  expect_equal(bid_get_quiet(), FALSE)

  # Test bid_with_quiet
  options(bidux.quiet = FALSE)
  result <- bid_with_quiet({
    expect_true(bid_get_quiet())
    "test_result"
  })
  expect_equal(result, "test_result")
  expect_false(bid_get_quiet()) # Should be restored
})

test_that("bid_message respects quiet mode", {
  original_quiet <- getOption("bidux.quiet", FALSE)
  on.exit(options(bidux.quiet = original_quiet))

  # Test with quiet = FALSE
  options(bidux.quiet = FALSE)
  expect_output(bid_message("Title", "Message 1", "Message 2"), "Title")
  expect_output(bid_message("Title", "Message 1", "Message 2"), "Message 1")

  # Test with quiet = TRUE
  options(bidux.quiet = TRUE)
  expect_silent(bid_message("Title", "Message 1", "Message 2"))

  # Test parameter override
  expect_output(bid_message("Title", "Message", quiet = FALSE), "Title")
  expect_silent(bid_message("Title", "Message", quiet = TRUE))

  # Test edge cases
  expect_silent(bid_message(NULL))
  expect_silent(bid_message(""))
  expect_silent(bid_message("Something", NULL, NA, ""))
})

test_that("bid_alert_info respects quiet mode", {
  original_quiet <- getOption("bidux.quiet", FALSE)
  on.exit(options(bidux.quiet = original_quiet))

  # when not quiet, should produce output (but we can't test cli output easily)
  options(bidux.quiet = FALSE)
  expect_no_error(bid_alert_info("test message"))

  # when quiet, should be silent
  options(bidux.quiet = TRUE)
  expect_silent(bid_alert_info("test message"))
})

# ==============================================================================
# TEXT NORMALIZATION AND FORMATTING
# ==============================================================================

test_that("normalize_text works correctly", {
  expect_equal(normalize_text(" hello world. "), "Hello world")
  expect_equal(normalize_text("hi!", capitalize_first = FALSE), "hi")
  expect_equal(normalize_text("hello", capitalize_first = FALSE), "hello")
  expect_equal(normalize_text("hi!", remove_trailing_punct = FALSE), "Hi!")
  expect_equal(normalize_text("wow???", remove_trailing_punct = TRUE), "Wow")

  # Edge cases
  expect_null(normalize_text(NULL))
  expect_equal(normalize_text(character(0)), character(0))
  expect_equal(normalize_text("   "), "   ") # whitespace-only returns as-is
  expect_equal(normalize_text("a"), "A")
  expect_equal(normalize_text("a!"), "A")
})

test_that("format_suggestions works correctly", {
  expect_equal(
    format_suggestions(c("suggestion 1", "suggestion 2")),
    "Suggestion 1, Suggestion 2"
  )
  expect_equal(format_suggestions(c("test.", "another!")), "Test, Another")
  expect_equal(format_suggestions(character(0)), "")
  expect_equal(format_suggestions(c("test"), " | "), "Test")
})

test_that("format_next_steps and parse_next_steps work together", {
  # single string
  expect_equal(format_next_steps("Step 1"), "Step 1")

  # multiple strings
  formatted <- format_next_steps(c("Step 1", "Step 2", "Step 3"))
  expect_equal(formatted, "Step 1; Step 2; Step 3")

  # parse back
  parsed <- parse_next_steps(formatted)
  expect_equal(parsed, c("Step 1", "Step 2", "Step 3"))

  # edge cases
  expect_true(is.na(format_next_steps(NULL)))
  expect_equal(parse_next_steps(NULL), character(0))
  expect_equal(parse_next_steps(NA), character(0))
})

# ==============================================================================
# VALIDATION FUNCTIONS
# ==============================================================================

test_that("validate_required_params works correctly", {
  expect_silent(validate_required_params(problem = "test", evidence = "test"))
  expect_error(
    validate_required_params(problem = NULL, evidence = "test"),
    "Required parameter 'problem' is missing"
  )
  # validate_required_params only checks for NULL, not empty strings
  expect_silent(validate_required_params(problem = "", evidence = "test"))
  expect_silent(validate_required_params(problem = "   ", evidence = "test"))
  # validate_required_params only checks for NULL, not NA
  expect_silent(validate_required_params(problem = NA, evidence = "test"))
})

test_that("validate_character_param works correctly", {
  expect_silent(validate_character_param("test", "param"))
  expect_silent(validate_character_param("  test  ", "param"))
  expect_silent(validate_character_param(NULL, "param", allow_null = TRUE))

  expect_error(validate_character_param(NULL, "param", allow_null = FALSE))
  expect_error(validate_character_param(123, "param"))
  expect_error(validate_character_param(c("a", "b"), "param"))
  expect_error(validate_character_param("", "param"))
  expect_error(validate_character_param("   ", "param"))

  # Minimum length validation
  expect_silent(validate_character_param("test", "param", min_length = 3))
  expect_error(validate_character_param("ab", "param", min_length = 3))
})

test_that("validate_list_param works correctly", {
  test_list <- list(a = 1, b = 2)
  expect_silent(validate_list_param(test_list, "param"))
  expect_silent(validate_list_param(NULL, "param", allow_null = TRUE))
  expect_silent(
    validate_list_param(test_list, "param", required_names = c("a"))
  )

  expect_error(validate_list_param(NULL, "param", allow_null = FALSE))
  expect_error(validate_list_param("not a list", "param"))
  expect_error(validate_list_param(123, "param"))
  expect_error(
    validate_list_param(test_list, "param", required_names = c("missing"))
  )
})

test_that("validate_logical_param works correctly", {
  expect_silent(validate_logical_param(TRUE, "param"))
  expect_silent(validate_logical_param(FALSE, "param"))
  expect_silent(validate_logical_param(NULL, "param", allow_null = TRUE))

  expect_error(validate_logical_param(NULL, "param", allow_null = FALSE))
  expect_error(validate_logical_param("true", "param"))
  expect_error(validate_logical_param(1, "param"))
  expect_error(validate_logical_param(c(TRUE, FALSE), "param"))
})

test_that("validate_previous_stage works correctly", {
  # Valid progressions
  expect_silent(validate_previous_stage(NULL, "Interpret"))
  expect_silent(validate_previous_stage("Interpret", "Notice"))
  expect_silent(validate_previous_stage("Notice", "Anticipate"))
  expect_silent(validate_previous_stage("Anticipate", "Structure"))
  expect_silent(validate_previous_stage("Structure", "Validate"))

  # Invalid stages
  expect_error(validate_previous_stage(NULL, "InvalidStage"), "Invalid current stage: InvalidStage")
  expect_error(validate_previous_stage("NotAStage", "Notice"), "Invalid previous stage name: NotAStage")

  # Discouraged progressions
  expect_warning(validate_previous_stage("Interpret", "Structure"), "Discouraged stage progression")
  expect_warning(validate_previous_stage("Notice", "Interpret"), "Invalid stage progression")
})

test_that("validate_user_personas works correctly", {
  expect_true(validate_user_personas(test_personas))

  # Invalid structure
  expect_error(validate_user_personas("not a list"))
  expect_error(validate_user_personas(list(list(goals = "missing name"))))

  # Missing recommended fields should warn but not error
  minimal_personas <- list(list(name = "Test"))
  expect_warning(expect_true(validate_user_personas(minimal_personas)))
})

# ==============================================================================
# SAFE ACCESS FUNCTIONS
# ==============================================================================

test_that("safe_check functions work correctly", {
  # safe_check
  expect_true(safe_check("test"))
  expect_true(safe_check(123))
  expect_false(safe_check(NULL))
  expect_false(safe_check(character(0)))
  expect_false(safe_check(NA))

  # With condition functions
  expect_true(safe_check("test", function(x) is.character(x)))
  expect_false(safe_check(123, function(x) is.character(x)))
  expect_false(safe_check("test", function(x) stop("error")))
})

test_that("safe_df_check works correctly", {
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  empty_df <- data.frame()

  expect_true(safe_df_check(test_df))
  expect_true(safe_df_check(test_df, min_rows = 2))
  expect_false(safe_df_check(test_df, min_rows = 5))
  expect_false(safe_df_check(empty_df))
  expect_false(safe_df_check(NULL))
  expect_false(safe_df_check("not a df"))
})

test_that("safe_column_access works correctly", {
  test_df <- data.frame(a = c(1, 2), b = c("x", "y"), c = c(NA, "z"), d = c(NA, NA))

  expect_equal(safe_column_access(test_df, "a"), 1)
  expect_equal(safe_column_access(test_df, "b"), "x")
  expect_equal(safe_column_access(test_df, "missing", "default"), "default")
  expect_equal(safe_column_access(NULL, "a", "default"), "default")
  expect_equal(safe_column_access(test_df, "c", default = "custom"), NA_character_)
  expect_equal(safe_column_access(test_df, "d", "custom"), "custom")
})

test_that("safe_list_access works correctly", {
  test_list <- list(a = 1, b = "test", c = NULL)
  test_vector <- c(1, 2, 3)

  expect_equal(safe_list_access(test_list, "a"), 1)
  expect_equal(safe_list_access(test_list, 1), 1)
  expect_equal(safe_list_access(test_vector, 2), 2)
  expect_equal(safe_list_access(test_list, "missing", "default"), "default")
  expect_equal(safe_list_access(test_list, 10, "default"), "default")
  expect_equal(safe_list_access(NULL, "a", "default"), "default")
  expect_equal(safe_list_access(test_list, "c", "default"), "default")
})

test_that("safe_string_check works correctly", {
  expect_true(safe_string_check("test"))
  expect_true(safe_string_check(c("a", "b")))
  expect_true(safe_string_check("test", min_length = 3))

  expect_false(safe_string_check("a", min_length = 3))
  expect_false(safe_string_check(NULL))
  expect_false(safe_string_check(123))
  expect_false(safe_string_check(""))
  expect_false(safe_string_check("   "))
})

# ==============================================================================
# STAGE DATA MANAGEMENT
# ==============================================================================

test_that("extract_stage_data works correctly", {
  test_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    extra_col = "not extracted"
  )

  columns <- c("problem", "evidence", "nonexistent")
  defaults <- list(nonexistent = "default_value")

  result <- extract_stage_data(test_stage, columns, defaults)

  expect_equal(result$problem, "Test problem")
  expect_equal(result$evidence, "Test evidence")
  expect_equal(result$nonexistent, "default_value")
})

test_that("get_stage_metadata works correctly", {
  metadata <- get_stage_metadata(3)
  expect_equal(metadata$stage_number, 3)
  expect_equal(metadata$total_stages, 5)
  expect_equal(metadata$validation_status, "completed")

  # With custom metadata
  custom <- list(custom_field = "test")
  metadata_custom <- get_stage_metadata(2, custom)
  expect_equal(metadata_custom$stage_number, 2)
  expect_equal(metadata_custom$custom_field, "test")
})

test_that("normalize_previous_stage works correctly", {
  # test with tibble containing legacy field names
  test_tibble <- tibble::tibble(
    stage = "Notice",
    previous_question = "Old field name",
    audience = NA,
    previous_audience = "Test audience",
    timestamp = Sys.time()
  )

  result <- normalize_previous_stage(test_tibble)
  expect_s3_class(result, "tbl_df")
  expect_true("previous_central_question" %in% names(result))
  expect_false("previous_question" %in% names(result))

  # test with NULL
  expect_null(normalize_previous_stage(NULL))
})

test_that("get_audience_from_previous works correctly", {
  test_data <- tibble::tibble(
    stage = "Notice",
    audience = "Data analysts",
    target_audience = "Secondary audience",
    timestamp = Sys.time()
  )

  result <- get_audience_from_previous(test_data)
  expect_true(is.character(result))
  expect_false(is.na(result))

  # test with NULL
  expect_true(is.na(get_audience_from_previous(NULL)))

  # test with no audience fields
  no_audience <- tibble::tibble(
    stage = "Notice",
    problem = "Test",
    timestamp = Sys.time()
  )
  expect_true(is.na(get_audience_from_previous(no_audience)))
})

test_that("get_personas_from_previous works correctly", {
  test_data <- tibble::tibble(
    stage = "Notice",
    user_personas = "Test personas",
    timestamp = Sys.time()
  )

  result <- get_personas_from_previous(test_data)
  expect_true(is.character(result))
  expect_false(is.na(result))

  # Test with NULL
  expect_true(is.na(get_personas_from_previous(NULL)))
})

# ==============================================================================
# ERROR HANDLING AND MESSAGING
# ==============================================================================

# standard_error_msg function has been removed in favor of modern cli error handling

# ==============================================================================
# DOMAIN-SPECIFIC FUNCTIONS
# ==============================================================================

test_that("find_best_concept_match works correctly", {
  # exact match
  expect_equal(
    find_best_concept_match("Visual Hierarchy", test_concepts_data),
    "Visual Hierarchy"
  )

  # case insensitive match
  expect_equal(
    find_best_concept_match("visual hierarchy", test_concepts_data),
    "Visual Hierarchy"
  )

  # partial match
  result <- find_best_concept_match("visual", test_concepts_data)
  expect_true(is.character(result) || is.null(result))

  # no match
  expect_null(find_best_concept_match("nonexistent", test_concepts_data))
  expect_null(find_best_concept_match("", test_concepts_data))
  expect_null(find_best_concept_match(NA, test_concepts_data))
})

test_that("detect_concepts_from_text works correctly", {
  text_with_concepts <- "The interface is too complex and has focus issues with attention"
  concepts <- detect_concepts_from_text(text_with_concepts)
  expect_true(is.character(concepts))
  expect_gte(length(concepts), 0)

  # Empty/NA text
  empty_result <- detect_concepts_from_text("")
  expect_true(is.character(empty_result))
  expect_equal(length(empty_result), 0)

  na_result <- detect_concepts_from_text(NA)
  expect_true(is.character(na_result))
  expect_equal(length(na_result), 0)
})

test_that("format_accessibility_for_storage works correctly", {
  # String input
  result1 <- format_accessibility_for_storage("High contrast colors")
  expect_true(is.character(result1) || is.na(result1))

  # List input
  acc_list <- list(contrast = "AA", keyboard = "full")
  result2 <- format_accessibility_for_storage(acc_list)
  expect_true(is.character(result2) || is.na(result2))

  # NULL input
  result3 <- format_accessibility_for_storage(NULL)
  expect_true(is.na(result3))
})

test_that("get_accessibility_advice works correctly", {
  expect_match(get_accessibility_advice("tabs"), "keyboard navigation")
  expect_match(get_accessibility_advice("grid"), "screen readers")
  expect_match(get_accessibility_advice("card"), "focus management")

  # Unknown/NULL layout
  result <- get_accessibility_advice("unknown")
  expect_true(is.character(result))
  expect_gt(nchar(result), 0)

  result_null <- get_accessibility_advice(NULL)
  expect_true(is.character(result_null))
  expect_gt(nchar(result_null), 0)
})

test_that("safe_data_story_access works correctly", {
  data_story <- list(
    hook = "Test hook",
    context = "Test context",
    resolution = ""
  )

  expect_equal(safe_data_story_access(data_story, "hook"), "Test hook")
  expect_equal(safe_data_story_access(data_story, "context"), "Test context")
  expect_true(is.na(safe_data_story_access(data_story, "resolution"))) # empty
  expect_true(is.na(safe_data_story_access(data_story, "missing")))
  expect_true(is.na(safe_data_story_access(NULL, "hook")))
})

# ==============================================================================
# SUGGESTION SYSTEM
# ==============================================================================

test_that("generate_stage_suggestions works correctly", {
  # check if dependencies are available before testing
  if (exists("apply_suggestion_rules") && exists("get_fallback_suggestion")) {
    interpret_context <- list(
      central_question = "How to improve?",
      data_story = list(hook = "Users struggle", context = "")
    )

    suggestions <- generate_stage_suggestions("Interpret", interpret_context)
    expect_true(is.character(suggestions))
    expect_gte(nchar(suggestions), 0)

    # test with empty context
    empty_suggestions <- generate_stage_suggestions("Notice", list())
    expect_true(is.character(empty_suggestions))
    expect_gte(nchar(empty_suggestions), 0)
  } else {
    skip("generate_stage_suggestions dependencies not available")
  }
})

test_that("evaluate_suggestion_condition works correctly", {
  # Valid function condition
  valid_condition <- function(ctx) !is.null(ctx$problem)
  context_data <- list(problem = "Test problem")
  result <- evaluate_suggestion_condition(valid_condition, context_data)
  expect_true(is.logical(result))

  # Condition that returns FALSE
  false_condition <- function(ctx) is.null(ctx$problem)
  result_false <- evaluate_suggestion_condition(false_condition, context_data)
  expect_true(is.logical(result_false))

  # Non-function condition
  suppressWarnings({
    result_invalid <- evaluate_suggestion_condition(
      "not a function",
      context_data
    )
    expect_true(is.logical(result_invalid))
    expect_false(result_invalid)
  })

  # Condition that throws error
  error_condition <- function(ctx) stop("Test error")
  suppressWarnings({
    result_error <- evaluate_suggestion_condition(error_condition, context_data)
    expect_true(is.logical(result_error))
    expect_false(result_error)
  })
})

# ==============================================================================
# PRIVATE/INTERNAL FUNCTIONS
# ==============================================================================

test_that("time wrapper .now works correctly", {
  result <- .now()
  expect_s3_class(result, "POSIXct")
  expect_true(result <= Sys.time())
})

test_that(".format_telemetry_refs_for_validation works correctly", {
  # Character vector input
  char_refs <- c("metric1", "metric2")
  result1 <- .format_telemetry_refs_for_validation(char_refs)
  expect_true(is.character(result1))
  expect_match(result1, "Track specific metrics")

  # List input
  list_refs <- list(clicks = "button clicks", views = "page views")
  result2 <- .format_telemetry_refs_for_validation(list_refs)
  expect_true(is.character(result2))
  expect_length(result2, 3) # header + 2 items

  # NULL/empty input
  result3 <- .format_telemetry_refs_for_validation(NULL)
  expect_equal(result3, character(0))

  result4 <- .format_telemetry_refs_for_validation(character(0))
  expect_equal(result4, character(0))
})

# ==============================================================================
# INTEGRATION TESTS
# ==============================================================================

test_that("validation functions work together correctly", {
  # test validate_bid_stage_params with various parameters
  previous_stage <- create_test_stage()

  additional_params <- list(
    test_char = list(
      value = "test value",
      type = "character",
      min_length = 3,
      allow_null = FALSE
    ),
    test_list = list(
      value = list(a = 1, b = 2),
      type = "list",
      required_names = c("a"),
      allow_null = FALSE
    )
  )

  expect_silent(
    validate_bid_stage_params(previous_stage, "Notice", additional_params)
  )

  # test with Interpret stage (no previous_stage required)
  expect_silent(
    validate_bid_stage_params(NULL, "Interpret", additional_params)
  )
})

test_that("utility functions handle Unicode and edge cases", {
  # unicode characters in text processing
  unicode_text <- "Test with émojis 🎉 and àccénts"
  expect_type(truncate_text(unicode_text, 20), "character")
  expect_type(normalize_text(unicode_text), "character")

  # very long text
  long_text <- paste(rep("word", 100), collapse = " ")
  truncated <- truncate_text(long_text, 50)
  expect_true(nchar(truncated) <= 50)
  expect_match(truncated, "\\.\\.\\.$")
})

test_that("functions are deterministic and consistent", {
  test_problem <- "Test problem for unit testing purposes"

  # multiple calls should return same result
  truncated1 <- truncate_text(test_problem, 15)
  truncated2 <- truncate_text(test_problem, 15)
  expect_equal(truncated1, truncated2)

  # validation should be consistent
  expect_silent(
    validate_required_params(problem = test_problem, evidence = "test")
  )
  expect_silent(
    validate_required_params(problem = test_problem, evidence = "test")
  )
})
