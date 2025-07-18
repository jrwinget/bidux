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
  # Fresh start: only allowed if current_stage == "Notice"
  expect_silent(validate_previous_stage(NULL, "Notice"))

  # Immediate predecessor steps (linear order) → no warning
  expect_silent(validate_previous_stage("Notice", "Interpret"))
  expect_silent(validate_previous_stage("Interpret", "Structure"))
  expect_silent(validate_previous_stage("Structure", "Anticipate"))
  expect_silent(validate_previous_stage("Anticipate", "Validate"))

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

  # Unusual progression (skipping an immediate predecessor) → warning
  expect_warning(
    validate_previous_stage("Notice", "Structure"),
    "Unusual stage progression: Notice -> Structure"
  )
  expect_warning(
    validate_previous_stage("Interpret", "Validate"),
    "Unusual stage progression: Interpret -> Validate"
  )

  # If current_stage=="Notice" but previous_stage non-NULL → warning
  expect_warning(
    validate_previous_stage("Interpret", "Notice"),
    "Unusual stage progression: Interpret -> Notice"
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
  expect_silent(validate_previous_stage(NULL, get_stage(stage_obj)))
  expect_silent(validate_previous_stage(get_stage(stage_obj), "Interpret"))
})

# Utility error messages are helpful
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

# Package style conventions
test_that("utility functions are consistent with package style", {
  # truncate_text returns a single character string
  expect_type(truncate_text("test", 10), "character")
  expect_length(truncate_text("test", 10), 1)

  # handle NULL inputs gracefully
  expect_equal(truncate_text(NULL, 10), "")
  expect_silent(bid_message(NULL, "valid"))
})

# Internationalization / encoding tests
test_that("utilities support internationalization considerations", {
  # Unicode characters
  unicode_text <- "Test with émojis 🎉 and àccénts"
  expect_type(truncate_text(unicode_text, 20), "character")

  # Very long text
  long_text <- paste(rep("word", 100), collapse = " ")
  truncated <- truncate_text(long_text, 50)
  expect_true(nchar(truncated) <= 50)
  expect_match(truncated, "\\.\\.\\.$") # Should end with "..."
})

# Utility functions support debugging and testing
test_that("utility functions support debugging and testing", {
  test_problem <- "Test problem for unit testing purposes"
  test_evidence <- "Test evidence for validation"

  expect_silent(validate_required_params(
    problem = test_problem,
    evidence = test_evidence
  ))

  truncated1 <- truncate_text(test_problem, 15)
  truncated2 <- truncate_text(test_problem, 15)
  expect_equal(truncated1, truncated2) # Deterministic
})
