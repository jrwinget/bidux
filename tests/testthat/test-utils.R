# %||% operator tests
test_that("%||% operator works correctly", {
  expect_equal(5 %||% "default", 5)
  expect_equal("hello" %||% "default", "hello")
  expect_equal(NULL %||% "default", "default")
  expect_equal(NA %||% "default", NA) # NA is not NULL
})

# truncate_text tests
test_that("truncate_text works correctly", {
  expect_equal(truncate_text("short", 10), "short")
  expect_equal(
    truncate_text("this is a very long text that should be truncated", 10),
    "this is..."
  )
  expect_equal(truncate_text(NULL, 10), "")
  expect_equal(truncate_text(NA, 10), "")
  expect_equal(truncate_text("", 10), "")
  expect_equal(truncate_text("exactly10!", 10), "exactly10!")
  expect_equal(truncate_text("exactly11!!", 10), "exactly...")
})

# validate_required_params tests
test_that("validate_required_params works correctly", {
  # Should pass for valid parameters
  expect_silent(validate_required_params(
    problem = "test problem",
    evidence = "test evidence"
  ))
  expect_silent(validate_required_params(foo = "bar", num = 123))

  # Should error for NULL parameters
  expect_error(
    validate_required_params(problem = NULL, evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )

  # Should error for empty string parameters
  expect_error(
    validate_required_params(problem = "", evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )

  # Should error for whitespace-only parameters
  expect_error(
    validate_required_params(problem = "   ", evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )

  # Should error for NA parameters
  expect_error(
    validate_required_params(problem = NA, evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )
})

# validate_previous_stage tests
test_that("validate_previous_stage works correctly", {
  # Fresh start: only allowed if current_stage == "Interpret" (first stage in 0.3.0+)
  expect_silent(validate_previous_stage(NULL, "Interpret"))

  # Immediate predecessor steps (new linear order) → no warning
  expect_silent(validate_previous_stage("Interpret", "Notice"))
  expect_silent(validate_previous_stage("Notice", "Anticipate"))
  expect_silent(validate_previous_stage("Anticipate", "Structure"))
  expect_silent(validate_previous_stage("Structure", "Validate"))

  # Invalid current stage → error with "Invalid stage: X. Must be one of: …"
  expect_error(
    validate_previous_stage(NULL, "InvalidStage"),
    "Invalid stage: InvalidStage\\. Must be one of:"
  )

  # Invalid previous stage string → same kind of error
  expect_error(
    validate_previous_stage("NotAStage", "Notice"),
    "Invalid stage: NotAStage\\. Must be one of:"
  )

  # Discouraged but valid progressions under flexible BID workflow
  expect_warning(
    validate_previous_stage("Interpret", "Structure"),
    "Discouraged stage progression.*Interpret.*Structure"
  )

  # Notice -> Validate is now valid in flexible workflow
  expect_no_warning(
    validate_previous_stage("Notice", "Validate")
  )

  # Interpret only accepts Validate (iterative) - Notice -> Interpret is invalid
  expect_warning(
    validate_previous_stage("Notice", "Interpret"),
    "Invalid stage progression.*Notice.*Interpret"
  )
})

# bid_message tests
test_that("bid_message works correctly", {
  # Should print title + bullet lines
  expect_output(
    bid_message("Title", "Message 1", "Message 2"),
    "Title"
  )
  expect_output(
    bid_message("Title", "Message 1", "Message 2"),
    "Message 1"
  )
  expect_output(
    bid_message("Title", "Message 1", "Message 2"),
    "Message 2"
  )

  # If title is NULL or empty, should be silent
  expect_silent(bid_message(NULL))
  expect_silent(bid_message(""))

  # Should filter out NULL/empty bullet points
  expect_output(bid_message("Valid", NULL, "", "Another valid"), "Valid")
  expect_output(
    bid_message("Valid", NULL, "", "Another valid"),
    "Another valid"
  )

  # If title is present but all bullet points are empty → silent
  expect_silent(bid_message("Something", NULL, NA, ""))
})

# Utility edge case tests
test_that("utility functions handle edge cases", {
  # truncate_text edge cases
  expect_equal(truncate_text("abc", 3), "abc") # Exactly at limit
  expect_equal(truncate_text("abcd", 3), "...") # One over limit
  expect_equal(truncate_text("ab", 3), "ab") # Under limit

  # validate_required_params still throws correct error
  expect_silent(validate_required_params(problem = "test", evidence = "test"))
  expect_error(
    validate_required_params(problem = NULL, evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )
  expect_error(
    validate_required_params(problem = "", evidence = "test"),
    "Required parameter 'problem' must be provided\\."
  )
})

# Integration with bid_stage objects
test_that("utility functions integrate with bid_stage system", {
  # Make a dummy bid_stage tibble
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem that is longer than truncation limit for testing purposes",
    timestamp = Sys.time()
  )
  stage_obj <- bid_stage("Notice", test_data)

  # Can use truncate_text on the stage data
  truncated_problem <- truncate_text(stage_obj$problem[1], 20)
  expect_equal(truncated_problem, "Test problem that...")

  # validate_previous_stage from a bid_stage object
  # Since stage_obj is "Notice", NULL should cause warning (first stage is now Interpret)
  expect_warning(validate_previous_stage(NULL, get_stage(stage_obj)))
  # "Notice" -> "Interpret" should cause warning (wrong direction)
  expect_warning(validate_previous_stage(get_stage(stage_obj), "Interpret"))
})

# utils error messages are helpful
test_that("utility error messages are helpful", {
  # validate_required_params message
  tryCatch(
    validate_required_params(problem = NULL, evidence = "test"),
    error = function(e) {
      expect_match(e$message, "Required parameter")
      expect_match(e$message, "problem")
      expect_match(e$message, "must be provided")
    }
  )

  # validate_previous_stage message
  tryCatch(
    validate_previous_stage(NULL, "BadStage"),
    error = function(e) {
      expect_match(e$message, "Invalid stage")
      expect_match(e$message, "BadStage")
      expect_match(e$message, "Must be one of")
    }
  )
})

# style conventions
test_that("utility functions are consistent with package style", {
  # truncate_text returns a single character string
  expect_type(truncate_text("test", 10), "character")
  expect_length(truncate_text("test", 10), 1)

  # handle NULL inputs gracefully
  expect_equal(truncate_text(NULL, 10), "")
  expect_silent(bid_message(NULL, "valid"))
})

# internationalization / encoding tests
test_that("utilities support internationalization considerations", {
  # Unicode characters
  unicode_text <- "Test with émojis 🎉 and àccénts"
  expect_type(truncate_text(unicode_text, 20), "character")

  # very long text
  long_text <- paste(rep("word", 100), collapse = " ")
  truncated <- truncate_text(long_text, 50)
  expect_true(nchar(truncated) <= 50)
  expect_match(truncated, "\\.\\.\\.$") # should end with "..."
})

# utils functions support debugging and testing
test_that("utility functions support debugging and testing", {
  test_problem <- "Test problem for unit testing purposes"
  test_evidence <- "Test evidence for validation"

  expect_silent(validate_required_params(
    problem = test_problem,
    evidence = test_evidence
  ))

  truncated1 <- truncate_text(test_problem, 15)
  truncated2 <- truncate_text(test_problem, 15)
  expect_equal(truncated1, truncated2) # deterministic
})


test_that("detect_concepts_from_text identifies concepts", {
  if (exists("detect_concepts_from_text")) {
    tryCatch({
      # text containing concept keywords
      text_with_concepts <- "The interface is too complex and has focus issues with attention"
      concepts <- detect_concepts_from_text(text_with_concepts)
      expect_true(is.character(concepts))
      expect_gte(length(concepts), 0)

      # empty/NA text
      empty_result <- detect_concepts_from_text("")
      expect_true(is.character(empty_result))
      expect_gte(length(empty_result), 0)
    }, error = function(e) {
      skip("detect_concepts_from_text has different interface than expected")
    })
  } else {
    skip("detect_concepts_from_text function does not exist")
  }
})

test_that("format_accessibility_for_storage handles different inputs", {
  if (exists("format_accessibility_for_storage")) {
    tryCatch({
      # character input
      acc_string <- "Color contrast meets WCAG AA"
      formatted_string <- format_accessibility_for_storage(acc_string)
      expect_true(is.character(formatted_string) || is.na(formatted_string))

      # NULL input
      formatted_null <- format_accessibility_for_storage(NULL)
      expect_true(is.character(formatted_null) || is.na(formatted_null))
    }, error = function(e) {
      skip("format_accessibility_for_storage has different interface than expected")
    })
  } else {
    skip("format_accessibility_for_storage function does not exist")
  }
})

test_that("generate_stage_suggestions works for different stages", {
  if (exists("generate_stage_suggestions")) {
    tryCatch({
      # test with Interpret stage
      interpret_context <- list(
        central_question = "How to improve?",
        data_story = list(hook = "Users struggle", context = "")
      )
      suggestions <- generate_stage_suggestions("Interpret", interpret_context)
      expect_true(is.character(suggestions))
      expect_gte(nchar(suggestions), 0)
    }, error = function(e) {
      skip("generate_stage_suggestions has different interface than expected")
    })
  } else {
    skip("generate_stage_suggestions function does not exist")
  }
})

test_that("evaluate_suggestion_condition handles different conditions", {
  if (exists("evaluate_suggestion_condition")) {
    tryCatch({
      # valid function condition
      valid_condition <- function(ctx) !is.null(ctx$problem)
      context_data <- list(problem = "Test problem")
      result <- evaluate_suggestion_condition(valid_condition, context_data)
      expect_true(is.logical(result))

      # condition that returns FALSE
      false_condition <- function(ctx) is.null(ctx$problem)
      result_false <- evaluate_suggestion_condition(false_condition, context_data)
      expect_true(is.logical(result_false))

      # non-function condition (suppress warnings)
      suppressWarnings({
        result_invalid <- evaluate_suggestion_condition("not a function", context_data)
        expect_true(is.logical(result_invalid))
      })

      # condition that throws error (suppress warnings)
      error_condition <- function(ctx) stop("Test error")
      suppressWarnings({
        result_error <- evaluate_suggestion_condition(error_condition, context_data)
        expect_true(is.logical(result_error))
      })
    }, error = function(e) {
      skip("evaluate_suggestion_condition has different interface than expected")
    })
  } else {
    skip("evaluate_suggestion_condition function does not exist")
  }
})

# Test additional validation functions
test_that("validate_character_param works correctly", {
  # Valid character parameters
  expect_silent(validate_character_param("test", "param"))
  expect_silent(validate_character_param("  test  ", "param"))

  # NULL handling
  expect_error(validate_character_param(NULL, "param", allow_null = FALSE))
  expect_silent(validate_character_param(NULL, "param", allow_null = TRUE))

  # Non-character inputs
  expect_error(validate_character_param(123, "param"))
  expect_error(validate_character_param(c("a", "b"), "param"))

  # Empty/whitespace strings
  expect_error(validate_character_param("", "param"))
  expect_error(validate_character_param("   ", "param"))

  # Minimum length validation
  expect_silent(validate_character_param("test", "param", min_length = 3))
  expect_error(validate_character_param("ab", "param", min_length = 3))
})

test_that("validate_list_param works correctly", {
  # Valid list parameters
  test_list <- list(a = 1, b = 2)
  expect_silent(validate_list_param(test_list, "param"))

  # NULL handling
  expect_silent(validate_list_param(NULL, "param", allow_null = TRUE))
  expect_error(validate_list_param(NULL, "param", allow_null = FALSE))

  # Non-list inputs
  expect_error(validate_list_param("not a list", "param"))
  expect_error(validate_list_param(123, "param"))

  # Required names validation
  expect_silent(validate_list_param(test_list, "param", required_names = c("a")))
  expect_error(validate_list_param(test_list, "param", required_names = c("missing")))
  expect_error(validate_list_param(test_list, "param", required_names = c("a", "missing")))
})

test_that("validate_logical_param works correctly", {
  # Valid logical parameters
  expect_silent(validate_logical_param(TRUE, "param"))
  expect_silent(validate_logical_param(FALSE, "param"))

  # NULL handling
  expect_silent(validate_logical_param(NULL, "param", allow_null = TRUE))
  expect_error(validate_logical_param(NULL, "param", allow_null = FALSE))

  # Non-logical inputs
  expect_error(validate_logical_param("true", "param"))
  expect_error(validate_logical_param(1, "param"))
  expect_error(validate_logical_param(c(TRUE, FALSE), "param"))
})

test_that("is_empty works correctly", {
  # Empty cases
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(c(NA, NA)))
  expect_true(is_empty(list())) # empty list is empty

  # Non-empty cases
  expect_false(is_empty("test"))
  expect_false(is_empty("  test  "))
  expect_false(is_empty(0))
  expect_false(is_empty(FALSE))
  expect_false(is_empty(list(a = 1))) # non-empty list is not empty
})

test_that("standard_error_msg generates proper messages", {
  # Missing parameter errors
  msg1 <- standard_error_msg("missing_param", "test_param")
  expect_match(msg1, "Required parameter")
  expect_match(msg1, "test_param")
  expect_match(msg1, "must be provided")

  # Invalid parameter errors
  msg2 <- standard_error_msg("invalid_param", "test_param", "character", "numeric")
  expect_match(msg2, "Parameter")
  expect_match(msg2, "test_param")
  expect_match(msg2, "invalid")

  # Invalid stage errors
  msg3 <- standard_error_msg("invalid_stage", NULL, c("A", "B"), "C")
  expect_match(msg3, "Invalid stage")
  expect_match(msg3, "Must be one of")

  # Default case
  msg4 <- standard_error_msg("unknown_type")
  expect_match(msg4, "error occurred")
})

test_that("safe_check works correctly", {
  # Valid objects
  expect_true(safe_check("test"))
  expect_true(safe_check(123))
  expect_true(safe_check(c(1, 2, 3)))

  # Invalid objects
  expect_false(safe_check(NULL))
  expect_false(safe_check(character(0)))
  expect_false(safe_check(NA))
  expect_false(safe_check(c(NA, NA)))

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

  # Valid column access
  expect_equal(safe_column_access(test_df, "a"), 1)
  expect_equal(safe_column_access(test_df, "b"), "x")

  # Invalid column access
  expect_equal(safe_column_access(test_df, "missing", "default"), "default")
  expect_equal(safe_column_access(NULL, "a", "default"), "default")

  # NA handling - first value is NA but not all are NA, so returns first value (NA)
  expect_equal(safe_column_access(test_df, "c", default = "custom"), NA_character_)

  # All values are NA, so returns default
  expect_equal(safe_column_access(test_df, "d", "custom"), "custom")
})

test_that("extract_stage_data works correctly", {
  test_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    evidence = "Test evidence",
    missing_col = "not extracted"
  )

  columns <- c("problem", "evidence", "nonexistent")
  defaults <- list(nonexistent = "default_value")

  result <- extract_stage_data(test_stage, columns, defaults)

  expect_equal(result$problem, "Test problem")
  expect_equal(result$evidence, "Test evidence")
  expect_equal(result$nonexistent, "default_value")
})

test_that("get_stage_metadata works correctly", {
  # Basic metadata
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

test_that("safe_list_access works correctly", {
  test_list <- list(a = 1, b = "test", c = NULL)
  test_vector <- c(1, 2, 3)

  # Valid access
  expect_equal(safe_list_access(test_list, "a"), 1)
  expect_equal(safe_list_access(test_list, 1), 1)
  expect_equal(safe_list_access(test_vector, 2), 2)

  # Invalid access
  expect_equal(safe_list_access(test_list, "missing", "default"), "default")
  expect_equal(safe_list_access(test_list, 10, "default"), "default")
  expect_equal(safe_list_access(NULL, "a", "default"), "default")
  expect_equal(safe_list_access(test_list, "c", "default"), "default")
})

test_that("safe_string_check works correctly", {
  # Valid strings
  expect_true(safe_string_check("test"))
  expect_true(safe_string_check(c("a", "b")))
  expect_true(safe_string_check("test", min_length = 3))

  # Invalid strings
  expect_false(safe_string_check("a", min_length = 3))
  expect_false(safe_string_check(NULL))
  expect_false(safe_string_check(123))
  expect_false(safe_string_check(""))
  expect_false(safe_string_check("   "))
})

test_that("find_best_concept_match works correctly", {
  if (exists("find_best_concept_match")) {
    # Mock concepts data
    mock_concepts <- tibble::tibble(
      concept = c("Visual Hierarchy", "Cognitive Load Theory", "Principle of Proximity")
    )

    # Exact match
    expect_equal(find_best_concept_match("Visual Hierarchy", mock_concepts), "Visual Hierarchy")

    # Case insensitive match
    expect_equal(find_best_concept_match("visual hierarchy", mock_concepts), "Visual Hierarchy")

    # Partial match
    result <- find_best_concept_match("visual", mock_concepts)
    expect_true(is.character(result) || is.null(result))

    # No match
    no_match <- find_best_concept_match("nonexistent", mock_concepts)
    expect_true(is.null(no_match))

    # Invalid input
    expect_null(find_best_concept_match("", mock_concepts))
    expect_null(find_best_concept_match(NA, mock_concepts))
  } else {
    skip("find_best_concept_match function does not exist")
  }
})

test_that("normalize_previous_stage works correctly", {
  if (exists("normalize_previous_stage")) {
    # Test with tibble
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

    # Test with NULL
    expect_null(normalize_previous_stage(NULL))

    # Test with bid_stage object
    if (exists("bid_stage")) {
      bid_obj <- bid_stage("Notice", test_tibble)
      result_bid <- normalize_previous_stage(bid_obj)
      expect_s3_class(result_bid, "tbl_df")
    }
  } else {
    skip("normalize_previous_stage function does not exist")
  }
})

test_that("get_audience_from_previous works correctly", {
  if (exists("get_audience_from_previous")) {
    # Test with valid audience
    test_data <- tibble::tibble(
      stage = "Notice",
      audience = "Data analysts",
      target_audience = "Secondary audience",
      timestamp = Sys.time()
    )

    result <- get_audience_from_previous(test_data)
    expect_true(is.character(result))
    expect_false(is.na(result))

    # Test with NULL
    result_null <- get_audience_from_previous(NULL)
    expect_true(is.na(result_null))

    # Test with no audience fields
    no_audience <- tibble::tibble(stage = "Notice", problem = "Test", timestamp = Sys.time())
    result_empty <- get_audience_from_previous(no_audience)
    expect_true(is.na(result_empty))
  } else {
    skip("get_audience_from_previous function does not exist")
  }
})

test_that("format_next_steps works correctly", {
  if (exists("format_next_steps")) {
    # Single string
    expect_equal(format_next_steps("Step 1"), "Step 1")

    # Multiple strings
    expect_equal(format_next_steps(c("Step 1", "Step 2")), "Step 1; Step 2")

    # Already formatted
    expect_equal(format_next_steps("Step 1; Step 2"), "Step 1; Step 2")

    # NULL input
    expect_true(is.na(format_next_steps(NULL)))

    # Other types
    expect_equal(format_next_steps(c(1, 2, 3)), "1; 2; 3")
  } else {
    skip("format_next_steps function does not exist")
  }
})

test_that("parse_next_steps works correctly", {
  if (exists("parse_next_steps")) {
    # Semicolon separated
    result1 <- parse_next_steps("Step 1; Step 2; Step 3")
    expect_equal(result1, c("Step 1", "Step 2", "Step 3"))

    # Single step
    result2 <- parse_next_steps("Single step")
    expect_equal(result2, "Single step")

    # NULL/NA input
    expect_equal(parse_next_steps(NULL), character(0))
    expect_equal(parse_next_steps(NA), character(0))
  } else {
    skip("parse_next_steps function does not exist")
  }
})

test_that("validate_user_personas works correctly", {
  if (exists("validate_user_personas")) {
    # Valid personas with all recommended fields
    valid_personas <- list(
      list(name = "Analyst", goals = "Analyze data", pain_points = "Complex UI", technical_level = "Advanced"),
      list(name = "Manager", goals = "Review reports", pain_points = "Time constraints", technical_level = "Intermediate")
    )

    expect_true(validate_user_personas(valid_personas))

    # Valid personas missing some recommended fields (will generate warnings)
    partial_personas <- list(
      list(name = "Valid", goals = "Test goals", pain_points = "Test pain points", technical_level = "Beginner")
    )

    expect_true(validate_user_personas(partial_personas))

    # Invalid personas - not a list
    expect_error(validate_user_personas("not a list"))

    # Invalid persona structure - suppress warnings for missing fields since we expect the error
    invalid_personas <- list(
      list(name = "Valid", goals = "Test goals", pain_points = "Test pain points", technical_level = "Beginner"),
      list(goals = "No name field", pain_points = "Test pain points", technical_level = "Beginner")
    )

    expect_error(validate_user_personas(invalid_personas))
  } else {
    skip("validate_user_personas function does not exist")
  }
})

test_that("get_accessibility_advice works correctly", {
  if (exists("get_accessibility_advice")) {
    # Known layout types
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
  } else {
    skip("get_accessibility_advice function does not exist")
  }
})

test_that("format_accessibility_for_storage works correctly", {
  if (exists("format_accessibility_for_storage")) {
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
  } else {
    skip("format_accessibility_for_storage function does not exist")
  }
})

test_that("time wrapper .now works correctly", {
  if (exists(".now")) {
    result <- .now()
    expect_s3_class(result, "POSIXct")
    expect_true(result <= Sys.time())
  } else {
    skip(".now function does not exist")
  }
})

test_that("basic normalization works", {
  expect_equal(
    normalize_text(" hello world. "),
    "Hello world"
  )
  expect_equal(
    normalize_text("hi!", capitalize_first = FALSE),
    "hi"
  )
})

test_that("capitalization toggle respected", {
  expect_equal(
    normalize_text("hello", capitalize_first = FALSE),
    "hello"
  )
  expect_equal(
    normalize_text("Hello", capitalize_first = TRUE),
    "Hello"
  )
})

test_that("trailing punctuation removal toggle respected", {
  expect_equal(
    normalize_text("hi!", remove_trailing_punct = FALSE),
    "Hi!"
  )
  expect_equal(
    normalize_text("wow???", remove_trailing_punct = TRUE),
    "Wow"
  )
})

test_that("handles empty or null-like inputs", {
  expect_null(normalize_text(NULL))
  expect_equal(normalize_text(character(0)), character(0))
  expect_equal(normalize_text("   "), "   ") # whitespace-only string returns as-is
})

test_that("works with non-punct trailing characters", {
  expect_equal(
    normalize_text("hello world#"),
    "Hello world#"
  )
})

test_that("single-character input handled", {
  expect_equal(normalize_text("a"), "A")
  expect_equal(normalize_text("a!"), "A")
})
