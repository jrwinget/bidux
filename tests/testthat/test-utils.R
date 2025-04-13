library(testthat)
library(tibble)

# access non-exported helper functions.
`%||%` <- bidux:::`%||%`
is_empty <- bidux:::is_empty
standard_error_msg <- bidux:::standard_error_msg
validate_required_params <- bidux:::validate_required_params
validate_previous_stage <- bidux:::validate_previous_stage
bid_message <- bidux:::bid_message

# null coalescing operator %||%
test_that("%||% returns left-hand side if not NULL", {
  expect_equal(5 %||% "default", 5)
  expect_equal("hello" %||% "default", "hello")
})

test_that("%||% returns right-hand side if left-hand side is NULL", {
  expect_equal(NULL %||% "default", "default")
})

# is_empty function
test_that("is_empty identifies NULL, NA, and empty strings as empty", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
})

test_that("is_empty identifies non-empty values correctly", {
  expect_false(is_empty("hello"))
  expect_false(is_empty(123))
  expect_false(is_empty(0))
})

# standard_error_msg function
test_that("standard_error_msg returns expected message for missing_param", {
  msg <- standard_error_msg("missing_param", "foo")
  expect_equal(msg, "Required parameter 'foo' must be provided.")
})

test_that("standard_error_msg returns expected message for invalid_param", {
  msg <- standard_error_msg("invalid_param", "foo", "numeric", "character")
  expect_equal(msg, "Parameter 'foo' is invalid. Expected: numeric, Actual: character.")
})

test_that("standard_error_msg returns expected default error message for unknown type", {
  msg <- standard_error_msg("unknown_type")
  expect_equal(msg, "An error occurred in the implementation of the BID framework.")
})

test_that("standard_error_msg returns expected message for invalid_stage", {
  msg <- standard_error_msg("invalid_stage", expected = "Notice", actual = "Test")
  expect_equal(
    msg,
    "Expected previous_stage from 'Notice', but got 'Test'. Please ensure you're following the BID framework stages in order."
  )
})

# validate_required_params function
test_that("validate_required_params passes for non-empty parameters", {
  expect_silent(validate_required_params(foo = "bar", num = 123))
})

test_that("validate_required_params errors when a parameter is empty", {
  expect_error(
    validate_required_params(foo = NULL),
    regexp = "Required parameter 'foo' must be provided."
  )
  expect_error(
    validate_required_params(foo = "", bar = "something"),
    regexp = "Required parameter 'foo' must be provided."
  )
})

# validate_previous_stage function
test_that("validate_previous_stage errors if previous_stage is not a tibble with a 'stage' column", {
  not_tibble <- list(stage = "Notice")
  expect_error(
    validate_previous_stage(not_tibble, "Interpret"),
    regexp = "Parameter 'previous_stage' is invalid"
  )
  no_stage <- tibble(foo = "bar")
  expect_error(
    validate_previous_stage(no_stage, "Interpret"),
    regexp = "Parameter 'previous_stage' is invalid"
  )
})

test_that("validate_previous_stage errors when previous_stage has an invalid stage", {
  previous_stage <- tibble(stage = "Foo")
  expect_error(
    validate_previous_stage(previous_stage, "Interpret"),
    regexp = "Invalid previous stage. For the 'Interpret' stage, the previous stage must be one of:"
  )
})

test_that("validate_previous_stage passes for valid previous_stage", {
  previous_stage <- tibble(stage = "Notice")
  expect_silent(validate_previous_stage(previous_stage, "Interpret"))
})

# bid_message function
test_that("bid_message outputs the correct formatted message", {
  expect_message(
    bid_message("Test Title", "First bullet", "Second bullet"),
    regexp = "Test Title\n  - First bullet\n  - Second bullet"
  )
})
