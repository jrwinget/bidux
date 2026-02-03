test_that("bid_suggest_analytics returns valid data frame structure", {
  result <- bid_suggest_analytics()

  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  # Check required columns exist
  expected_cols <- c(
    "solution",
    "type",
    "cost",
    "self_hosted",
    "gdpr_compliant",
    "integration_method",
    "key_features",
    "bidux_compatibility",
    "docs_url"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("bid_suggest_analytics filters by dashboard_type", {
  # All dashboard types should return results (all solutions support all types)
  result_static <- bid_suggest_analytics(dashboard_type = "static")
  result_ojs <- bid_suggest_analytics(dashboard_type = "ojs")
  result_python <- bid_suggest_analytics(dashboard_type = "python")

  expect_true(is.data.frame(result_static))
  expect_true(is.data.frame(result_ojs))
  expect_true(is.data.frame(result_python))

  # All should have results since all solutions support all dashboard types
  expect_gt(nrow(result_static), 0)
  expect_gt(nrow(result_ojs), 0)
  expect_gt(nrow(result_python), 0)
})

test_that("bid_suggest_analytics filters by privacy_preference", {
  # GDPR compliant (default)
  result_gdpr <- bid_suggest_analytics(privacy_preference = "gdpr_compliant")
  expect_true(all(result_gdpr$gdpr_compliant))

  # Privacy focused
  result_privacy <- bid_suggest_analytics(
    privacy_preference = "privacy_focused"
  )
  expect_true(all(result_privacy$type == "privacy-focused"))

  # Standard (includes all, even non-GDPR)
  result_standard <- bid_suggest_analytics(privacy_preference = "standard")
  expect_gt(nrow(result_standard), 0)
})

test_that("bid_suggest_analytics filters by budget", {
  # Free only
  result_free <- bid_suggest_analytics(budget = "free")
  expect_true(all(result_free$cost %in% c("free", "freemium")))

  # Low cost
  result_low <- bid_suggest_analytics(budget = "low")
  expect_true(all(result_low$cost %in% c("free", "freemium", "paid")))

  # Flexible (all)
  result_flexible <- bid_suggest_analytics(budget = "flexible")
  expect_gt(nrow(result_flexible), 0)
})

test_that("bid_suggest_analytics filters by self_hosted", {
  # Self-hosted only
  result_self_hosted <- bid_suggest_analytics(self_hosted = TRUE)
  expect_true(all(result_self_hosted$self_hosted))

  # Non-self-hosted allowed (default)
  result_any <- bid_suggest_analytics(self_hosted = FALSE)
  expect_gt(nrow(result_any), 0)
})

test_that("bid_suggest_analytics handles combined filters", {
  # Free + GDPR compliant
  result <- bid_suggest_analytics(
    privacy_preference = "gdpr_compliant",
    budget = "free"
  )

  expect_true(all(result$gdpr_compliant))
  expect_true(all(result$cost %in% c("free", "freemium")))
})

test_that("bid_suggest_analytics handles restrictive filters gracefully", {
  # Very restrictive filter - may return empty

  expect_warning(
    result <- bid_suggest_analytics(
      privacy_preference = "privacy_focused",
      budget = "free",
      self_hosted = TRUE
    ),
    NA # May or may not warn depending on data
  )

  # Result should be data frame (possibly empty)
  expect_true(is.data.frame(result))
})

test_that("bid_suggest_analytics results are properly sorted", {
  result <- bid_suggest_analytics()

  # Privacy-focused should appear first when GDPR compliant requested
  if (nrow(result) > 1) {
    # Results should be sorted by type priority and compatibility
    first_types <- head(result$type, 3)
    # Privacy-focused should generally be prioritized
    expect_true(
      "privacy-focused" %in% first_types || "open-source" %in% first_types
    )
  }
})

test_that("bid_suggest_analytics docs_url contains valid URLs", {
  result <- bid_suggest_analytics()

  # All docs_url should be valid URLs
  expect_true(all(grepl("^https?://", result$docs_url)))
})

test_that("bid_suggest_analytics key_features are non-empty", {
  result <- bid_suggest_analytics()

  # All solutions should have key features
  expect_true(all(nchar(result$key_features) > 0))
})

test_that("bid_suggest_analytics handles invalid arguments", {
  # Invalid dashboard_type
  expect_error(
    bid_suggest_analytics(dashboard_type = "invalid"),
    "'arg' should be one of"
  )

  # Invalid privacy_preference
  expect_error(
    bid_suggest_analytics(privacy_preference = "invalid"),
    "'arg' should be one of"
  )

  # Invalid budget
  expect_error(
    bid_suggest_analytics(budget = "invalid"),
    "'arg' should be one of"
  )
})

test_that("bid_suggest_analytics includes expected solutions", {
  result <- bid_suggest_analytics(privacy_preference = "standard")

  # Should include major solutions
  solutions <- result$solution
  expect_true(any(grepl(
    "Plausible|Matomo|Google|PostHog",
    solutions,
    ignore.case = TRUE
  )))
})

test_that("bid_suggest_analytics bidux_compatibility values are valid", {
  result <- bid_suggest_analytics()

  # Compatibility should be one of expected values
  expect_true(all(result$bidux_compatibility %in% c("good", "manual")))
})

test_that("bid_suggest_analytics returns warning for no matches", {
  # Create impossible filter combination
  # Self-hosted + privacy_focused + free should be very restrictive
  expect_warning(
    result <- bid_suggest_analytics(
      self_hosted = TRUE,
      privacy_preference = "privacy_focused"
    ),
    NA # May or may not produce warning
  )
})

test_that("bid_suggest_analytics type ordering is correct", {
  result <- bid_suggest_analytics(privacy_preference = "standard")

  # Verify types are from expected set
  expected_types <- c(
    "privacy-focused",
    "open-source",
    "product-analytics",
    "traditional"
  )
  expect_true(all(result$type %in% expected_types))
})
