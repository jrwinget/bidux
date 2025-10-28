# Test comprehensive validation utilities created in Phase 4 refactoring

test_that("validate_param works correctly for character inputs", {
  # valid character input
  expect_silent(validate_param("test", "test_arg", "character"))
  expect_silent(validate_param(c("a", "b"), "test_arg", "character"))

  # missing parameter
  expect_error(
    validate_param(, "test_arg", "character"),
    "Argument 'test_arg' is missing with no default"
  )

  # wrong type
  expect_error(
    validate_param(123, "test_arg", "character"),
    "Argument 'test_arg' must be a character vector"
  )

  # length constraints
  expect_error(
    validate_param(character(0), "test_arg", "character", min_length = 1),
    "Argument 'test_arg' must have at least 1 element"
  )

  expect_error(
    validate_param(c("a", "b", "c"), "test_arg", "character", max_length = 2),
    "Argument 'test_arg' must have at most 2 element"
  )

  # na values
  expect_error(
    validate_param(c("a", NA), "test_arg", "character", allow_na = FALSE),
    "Argument 'test_arg' cannot contain NA values"
  )

  expect_silent(
    validate_param(c("a", NA), "test_arg", "character", allow_na = TRUE)
  )

  # choice validation
  expect_error(
    validate_param("invalid", "test_arg", "character", choices = c("a", "b")),
    "Argument 'test_arg' must be one of: a, b"
  )

  expect_silent(
    validate_param("a", "test_arg", "character", choices = c("a", "b"))
  )
})

test_that("validate_param works correctly for logical inputs", {
  # valid logical input
  expect_silent(validate_param(TRUE, "test_arg", "logical"))
  expect_silent(validate_param(c(TRUE, FALSE), "test_arg", "logical"))

  # wrong type
  expect_error(
    validate_param("true", "test_arg", "logical"),
    "Argument 'test_arg' must be a logical vector"
  )

  # single logical value requirement
  expect_error(
    validate_param(c(TRUE, FALSE), "test_arg", "logical", max_length = 1),
    "Argument 'test_arg' must have at most 1 element"
  )

  expect_error(
    validate_param(NA, "test_arg", "logical", max_length = 1),
    "Argument 'test_arg' cannot contain NA values"
  )
})

test_that("validate_param works correctly for numeric inputs", {
  # valid numeric input
  expect_silent(validate_param(123, "test_arg", "numeric"))
  expect_silent(validate_param(c(1, 2, 3), "test_arg", "numeric"))

  # wrong type
  expect_error(
    validate_param("123", "test_arg", "numeric"),
    "Argument 'test_arg' must be a numeric vector"
  )
})

test_that("create_bid_result creates consistent result structures", {
  # basic result creation
  data_list <- list(text = c("a", "b"), value = c(1, 2))
  result <- create_bid_result(data_list, "test_class", return_tibble = FALSE)

  expect_s3_class(result, "test_class")
  expect_s3_class(result, "data.frame")
  expect_true("timestamp" %in% names(result))
  expect_equal(nrow(result), 2)

  # with attributes
  attributes <- list(method = "test", context = "example")
  result_with_attrs <- create_bid_result(data_list, "test_class",
    attributes = attributes,
    return_tibble = FALSE
  )

  expect_equal(attr(result_with_attrs, "method"), "test")
  expect_equal(attr(result_with_attrs, "context"), "example")

  # preserves existing timestamp
  data_with_timestamp <- list(text = "a", timestamp = Sys.time())
  result_with_time <- create_bid_result(data_with_timestamp, "test_class",
    return_tibble = FALSE
  )

  expect_true("timestamp" %in% names(result_with_time))
  expect_equal(length(unique(result_with_time$timestamp)), 1)
})

test_that("create_bid_result handles tibble creation when available", {
  skip_if_not_installed("tibble")

  data_list <- list(text = c("a", "b"), value = c(1, 2))
  result <- create_bid_result(data_list, "test_class", return_tibble = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "test_class")
})

test_that("create_bid_result falls back gracefully without tibble", {
  # simulate tibble not available by setting return_tibble = FALSE
  data_list <- list(text = c("a", "b"), value = c(1, 2))
  result <- create_bid_result(data_list, "test_class", return_tibble = FALSE)

  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "test_class")
  expect_false(inherits(result, "tbl_df"))
})

test_that("validate_param edge cases", {
  # empty choices should not cause errors
  expect_silent(
    validate_param("test", "test_arg", "character", choices = character(0))
  )

  # infinity length limits
  expect_silent(
    validate_param(rep("a", 1000), "test_arg", "character", max_length = Inf)
  )

  # zero min_length
  expect_silent(
    validate_param(character(0), "test_arg", "character", min_length = 0)
  )
})
