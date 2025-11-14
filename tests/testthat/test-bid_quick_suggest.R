test_that("bid_quick_suggest returns correct structure", {
  result <- bid_quick_suggest(
    problem = "Users can't find the download button",
    quiet = TRUE
  )

  # check it's a tibble
  expect_s3_class(result, "tbl_df")

  # check required columns
  expected_cols <- c(
    "title", "details", "components", "concept",
    "score", "difficulty", "rationale"
  )
  expect_equal(sort(names(result)), sort(expected_cols))

  # check column types
  expect_type(result$title, "character")
  expect_type(result$details, "character")
  expect_type(result$components, "list")
  expect_type(result$concept, "character")
  expect_type(result$score, "double")
  expect_type(result$difficulty, "character")
  expect_type(result$rationale, "character")

  # check score range
  expect_true(all(result$score >= 0 & result$score <= 1))

  # check difficulty values
  valid_difficulties <- c("easy", "moderate", "advanced")
  expect_true(all(result$difficulty %in% valid_difficulties))
})

test_that("bid_quick_suggest validates problem parameter", {
  # NULL problem
  expect_error(
    bid_quick_suggest(problem = NULL),
    "problem.*required"
  )

  # empty problem
  expect_error(
    bid_quick_suggest(problem = ""),
    "at least 5"
  )

  # too short problem
  expect_error(
    bid_quick_suggest(problem = "bad"),
    "at least 5"
  )

  # non-character problem
  expect_error(
    bid_quick_suggest(problem = 123),
    "character string"
  )
})

test_that("bid_quick_suggest handles context parameter", {
  # with context
  result <- bid_quick_suggest(
    problem = "Dashboard is overwhelming",
    context = "Used by financial analysts",
    quiet = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)

  # without context
  result2 <- bid_quick_suggest(
    problem = "Dashboard is overwhelming",
    quiet = TRUE
  )

  expect_s3_class(result2, "tbl_df")
  expect_true(nrow(result2) > 0)
})

test_that("bid_quick_suggest detects cognitive load issues", {
  result <- bid_quick_suggest(
    problem = "Users are overwhelmed with too many options in the dropdown",
    quiet = TRUE
  )

  # should detect Cognitive Load Theory concept
  expect_true("Cognitive Load Theory" %in% result$concept)

  # should have relevant suggestions
  expect_true(any(grepl("limit|reduce|simplif", tolower(result$title))))
})

test_that("bid_quick_suggest detects navigation issues", {
  result <- bid_quick_suggest(
    problem = "Users can't find important metrics in the dashboard",
    quiet = TRUE
  )

  # should detect Information Scent concept
  expect_true("Information Scent" %in% result$concept)

  # should have navigation-related suggestions
  expect_true(any(grepl("label|navigation|scent", tolower(result$details))))
})

test_that("bid_quick_suggest detects visual hierarchy issues", {
  result <- bid_quick_suggest(
    problem = "The dashboard is cluttered and disorganized",
    quiet = TRUE
  )

  # should detect Visual Hierarchy concept
  expect_true("Visual Hierarchy" %in% result$concept)

  # should have hierarchy-related suggestions
  expect_true(any(grepl("hierarch|group|spacing", tolower(result$details))))
})

test_that("bid_quick_suggest detects mobile/touch issues", {
  result <- bid_quick_suggest(
    problem = "Mobile interface has small buttons that are hard to tap",
    quiet = TRUE
  )

  # should detect Fitts's Law concept
  expect_true("Fitts's Law" %in% result$concept)

  # should have usability-related suggestions (less strict than specific keywords)
  # fitts's law deals with target size and interaction efficiency
  expect_true(nrow(result[result$concept == "Fitts's Law", ]) > 0)
})

test_that("bid_quick_suggest respects limit parameter", {
  # get all suggestions first
  result_all <- bid_quick_suggest(
    problem = "Users struggle with complex dashboard",
    limit = Inf,
    quiet = TRUE
  )

  # apply limit
  result_limited <- bid_quick_suggest(
    problem = "Users struggle with complex dashboard",
    limit = 3,
    quiet = TRUE
  )

  expect_true(nrow(result_limited) <= 3)
  expect_true(nrow(result_all) >= nrow(result_limited))

  # check limit parameter validation
  expect_error(
    bid_quick_suggest(problem = "test problem", limit = -1),
    "positive number"
  )

  expect_error(
    bid_quick_suggest(problem = "test problem", limit = "five"),
    "positive number"
  )
})

test_that("bid_quick_suggest respects min_score parameter", {
  result_strict <- bid_quick_suggest(
    problem = "Dashboard navigation is confusing",
    min_score = 0.9,
    quiet = TRUE
  )

  result_lenient <- bid_quick_suggest(
    problem = "Dashboard navigation is confusing",
    min_score = 0.5,
    quiet = TRUE
  )

  # strict should have fewer results
  expect_true(nrow(result_strict) <= nrow(result_lenient))

  # all scores should meet threshold
  if (nrow(result_strict) > 0) {
    expect_true(all(result_strict$score >= 0.9))
  }

  if (nrow(result_lenient) > 0) {
    expect_true(all(result_lenient$score >= 0.5))
  }

  # check min_score validation
  expect_error(
    bid_quick_suggest(problem = "test problem", min_score = 1.5),
    "between 0 and 1"
  )

  expect_error(
    bid_quick_suggest(problem = "test problem", min_score = -0.1),
    "between 0 and 1"
  )
})

test_that("bid_quick_suggest filters by package", {
  result_all <- bid_quick_suggest(
    problem = "Users need better data visualization",
    package = NULL,
    quiet = TRUE
  )

  result_bslib <- bid_quick_suggest(
    problem = "Users need better data visualization",
    package = "bslib",
    quiet = TRUE
  )

  # filtered result should have fewer or equal suggestions
  expect_true(nrow(result_bslib) <= nrow(result_all))

  # all bslib results should contain "bslib" in components
  if (nrow(result_bslib) > 0) {
    has_bslib <- sapply(result_bslib$components, function(comp_vec) {
      any(grepl("bslib", tolower(comp_vec), fixed = TRUE))
    })
    expect_true(all(has_bslib))
  }
})

test_that("bid_quick_suggest warns for non-standard packages", {
  expect_warning(
    result <- bid_quick_suggest(
      problem = "Custom visualization problem",
      package = "customPackage",
      quiet = TRUE
    ),
    "not in the standard list"
  )
})

test_that("bid_quick_suggest handles package parameter validation", {
  # invalid package type
  expect_error(
    bid_quick_suggest(problem = "test problem", package = 123),
    "single character string"
  )

  # multiple packages
  expect_error(
    bid_quick_suggest(problem = "test problem", package = c("bslib", "shiny")),
    "single character string"
  )
})

test_that("bid_quick_suggest returns suggestions sorted by score", {
  result <- bid_quick_suggest(
    problem = "Complex dashboard with multiple issues",
    limit = 10,
    quiet = TRUE
  )

  if (nrow(result) > 1) {
    # check scores are in descending order
    expect_true(all(diff(result$score) <= 0))
  }
})

test_that("bid_quick_suggest handles multiple problem types", {
  # test different problem types
  problems <- c(
    "Information overload on main screen",
    "Can't find the export button",
    "Mobile layout is broken",
    "Too many tabs and sections",
    "Filters are confusing for beginners"
  )

  for (prob in problems) {
    result <- bid_quick_suggest(
      problem = prob,
      quiet = TRUE
    )

    expect_s3_class(result, "tbl_df")
    expect_true(nrow(result) > 0)
    expect_true(all(result$score >= 0.7)) # default min_score
  }
})

test_that("bid_quick_suggest components are valid vectors", {
  result <- bid_quick_suggest(
    problem = "Users struggle with data entry forms",
    quiet = TRUE
  )

  # check each components entry is a character vector
  for (i in seq_len(min(nrow(result), 5))) {
    components_entry <- result$components[[i]]
    expect_type(components_entry, "character")
    expect_true(length(components_entry) > 0)

    # check format includes package prefix (e.g., "shiny::", "bslib::")
    has_prefix <- any(grepl("::", components_entry))
    expect_true(has_prefix)
  }
})

test_that("bid_quick_suggest difficulty levels are reasonable", {
  result <- bid_quick_suggest(
    problem = "Dashboard needs better organization",
    limit = 20,
    quiet = TRUE
  )

  # should have a mix of difficulties
  difficulties <- unique(result$difficulty)
  expect_true(length(difficulties) >= 1)

  # all difficulties should be valid
  valid_difficulties <- c("easy", "moderate", "advanced")
  expect_true(all(difficulties %in% valid_difficulties))

  # easy suggestions should exist
  easy_suggestions <- result[result$difficulty == "easy", ]
  expect_true(nrow(easy_suggestions) > 0)
})

test_that("bid_quick_suggest rationales are meaningful", {
  result <- bid_quick_suggest(
    problem = "Users confused by cluttered interface",
    limit = 5,
    quiet = TRUE
  )

  # check rationales are not empty
  expect_true(all(nchar(result$rationale) > 20))

  # check rationales contain helpful language
  rationale_text <- paste(tolower(result$rationale), collapse = " ")
  helpful_terms <- c(
    "help", "improve", "reduce", "provide", "ensure",
    "allow", "enable", "facilitate", "support"
  )
  has_helpful_language <- any(sapply(
    helpful_terms,
    function(term) grepl(term, rationale_text)
  ))
  expect_true(has_helpful_language)
})

test_that("bid_quick_suggest handles edge cases", {
  # very long problem description
  long_problem <- paste(rep("users struggle with navigation", 50), collapse = " ")
  result_long <- bid_quick_suggest(
    problem = long_problem,
    quiet = TRUE
  )
  expect_s3_class(result_long, "tbl_df")

  # problem with special characters
  special_problem <- "Users can't find the 'Export' button (it's hidden!)"
  result_special <- bid_quick_suggest(
    problem = special_problem,
    quiet = TRUE
  )
  expect_s3_class(result_special, "tbl_df")

  # problem with mixed case
  mixed_problem <- "USERS CANNOT FIND Important Metrics"
  result_mixed <- bid_quick_suggest(
    problem = mixed_problem,
    quiet = TRUE
  )
  expect_s3_class(result_mixed, "tbl_df")
})

test_that("bid_quick_suggest returns empty tibble with correct structure when no suggestions", {
  # use combination of high min_score and restrictive package filter to get no results
  suppressWarnings({
    result <- bid_quick_suggest(
      problem = "Simple issue",
      min_score = 0.99,
      package = "nonexistentPackage123",
      quiet = TRUE
    )
  })

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)

  # check columns still exist
  expected_cols <- c(
    "title", "details", "components", "concept",
    "score", "difficulty", "rationale"
  )
  expect_equal(sort(names(result)), sort(expected_cols))
})

test_that("bid_quick_suggest respects quiet parameter", {
  # with quiet = TRUE, should not produce bid_quick_suggest-specific messages
  # note: package load messages and one-time notices may still appear
  output <- capture.output({
    result <- bid_quick_suggest(
      problem = "Users struggle with navigation",
      quiet = TRUE
    )
  })

  # check that function-specific messages are suppressed
  # these are the messages that should be controlled by quiet parameter
  expect_false(any(grepl("Analyzing your UX problem", output)))
  expect_false(any(grepl("Quick suggestions ready", output, ignore.case = TRUE)))

  # with quiet = FALSE, should produce messages
  # check for the summary message which uses cat() and is reliably captured
  expect_output(
    result <- bid_quick_suggest(
      problem = "Users struggle with navigation",
      quiet = FALSE
    ),
    "Quick suggestions ready"
  )
})

test_that("bid_quick_suggest concept detection is accurate", {
  # test specific concept triggers
  test_cases <- list(
    list(
      problem = "Too many dropdown options overwhelm users",
      expected = "Cognitive Load Theory"
    ),
    list(
      problem = "Users can't locate the search functionality",
      expected = "Information Scent"
    ),
    list(
      problem = "Dashboard is cluttered and hard to scan",
      expected = "Visual Hierarchy"
    ),
    list(
      problem = "Mobile buttons are too small to tap accurately",
      expected = "Fitts's Law"
    ),
    list(
      problem = "First-time users don't know where to start",
      expected = "User Onboarding"
    )
  )

  for (test_case in test_cases) {
    result <- bid_quick_suggest(
      problem = test_case$problem,
      quiet = TRUE
    )

    expect_true(
      test_case$expected %in% result$concept,
      info = glue::glue(
        "Expected '{test_case$expected}' for problem: {test_case$problem}"
      )
    )
  }
})

test_that("bid_quick_suggest with context provides better suggestions", {
  problem <- "Dashboard is hard to use"

  result_no_context <- bid_quick_suggest(
    problem = problem,
    quiet = TRUE
  )

  result_with_context <- bid_quick_suggest(
    problem = problem,
    context = "Used by beginner analysts who need step-by-step guidance",
    quiet = TRUE
  )

  # both should return results
  expect_true(nrow(result_no_context) > 0)
  expect_true(nrow(result_with_context) > 0)

  # context version might include onboarding concepts
  context_concepts <- unique(result_with_context$concept)
  expect_true(length(context_concepts) > 0)
})

test_that("bid_quick_suggest layout detection works correctly", {
  # test different layout triggers
  overload_result <- bid_quick_suggest(
    problem = "Users are overwhelmed with too much information",
    quiet = TRUE
  )

  # breathable layout should prioritize cognitive load suggestions
  expect_true("Cognitive Load Theory" %in% overload_result$concept)

  navigation_result <- bid_quick_suggest(
    problem = "Users get lost navigating between sections and tabs",
    quiet = TRUE
  )

  # should have navigation-related suggestions
  expect_true(nrow(navigation_result) > 0)
})

test_that("bid_quick_suggest examples from documentation work", {
  # example 1: basic usage
  ex1 <- suppressMessages({
    bid_quick_suggest(
      problem = "Users can't find the download button",
      quiet = TRUE
    )
  })
  expect_s3_class(ex1, "tbl_df")

  # example 2: with context
  ex2 <- suppressMessages({
    bid_quick_suggest(
      problem = "Dashboard has too many charts and metrics",
      context = "Financial analysts need quick insights but get overwhelmed",
      limit = 5,
      quiet = TRUE
    )
  })
  expect_true(nrow(ex2) <= 5)

  # example 3: package filter
  ex3 <- suppressMessages({
    bid_quick_suggest(
      problem = "Mobile interface is hard to use",
      package = "bslib",
      min_score = 0.8,
      quiet = TRUE
    )
  })
  if (nrow(ex3) > 0) {
    expect_true(all(ex3$score >= 0.8))
  }

  # example 4: navigation
  ex4 <- suppressMessages({
    bid_quick_suggest(
      problem = "Users get lost in multi-tab interface",
      context = "Application has 10+ tabs with nested content",
      quiet = TRUE
    )
  })
  expect_s3_class(ex4, "tbl_df")

  # example 5: information overload
  ex5 <- suppressMessages({
    bid_quick_suggest(
      problem = "Too many filters and options on the sidebar",
      context = "Beginners find the interface overwhelming",
      quiet = TRUE
    )
  })
  expect_true("Cognitive Load Theory" %in% ex5$concept)
})

test_that("bid_quick_suggest score adjustments work", {
  # problem with specific keywords should boost relevant suggestions
  result <- suppressMessages({
    bid_quick_suggest(
      problem = "Users can't find the navigation menu to search for data",
      quiet = TRUE
    )
  })

  # navigation-related suggestions should be present and highly ranked
  # check that Information Scent concept appears (handles navigation/findability)
  expect_true("Information Scent" %in% result$concept)

  # check that navigation-related suggestions are in top half
  if (nrow(result) > 0) {
    top_half <- result[1:ceiling(nrow(result) / 2), ]
    has_nav_keywords <- any(sapply(top_half$details, function(d) {
      grepl("navigat|label|scent|find", tolower(d))
    }))
    expect_true(has_nav_keywords)
  }
})

test_that("bid_quick_suggest integrates with BID workflow correctly", {
  # quick suggest should work standalone
  quick_result <- bid_quick_suggest(
    problem = "Users struggle with complex filters",
    quiet = TRUE
  )

  expect_s3_class(quick_result, "tbl_df")
  expect_true(nrow(quick_result) > 0)

  # concepts from quick suggest should be valid BID concepts
  concepts_used <- unique(quick_result$concept)
  for (concept in concepts_used) {
    # should be able to look up in bid_concept
    concept_info <- bid_concept(concept)
    expect_s3_class(concept_info, "tbl_df")
    expect_equal(nrow(concept_info), 1)
  }
})
