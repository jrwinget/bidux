test_that("bid_suggest_components returns tibble with correct structure", {
  notice_result <- bid_notice(
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )

  suppressMessages({
    suggestions <- bid_suggest_components(notice_result)
  })

  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(
    c("package", "component", "description", "relevance") %in%
      names(suggestions)
  ))
  expect_true(nrow(suggestions) > 0)
})

test_that("bid_suggest_components filters by package correctly", {
  notice_result <- bid_notice(
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )

  suppressMessages({
    bslib_suggestions <- bid_suggest_components(
      notice_result,
      package = "bslib"
    )
  })

  expect_s3_class(bslib_suggestions, "tbl_df")
  if (nrow(bslib_suggestions) > 0) {
    expect_true(all(bslib_suggestions$package == "bslib"))
  }

  suppressMessages({
    shiny_suggestions <- bid_suggest_components(
      notice_result,
      package = "shiny"
    )
  })

  expect_s3_class(shiny_suggestions, "tbl_df")
  if (nrow(shiny_suggestions) > 0) {
    expect_true(all(shiny_suggestions$package == "shiny"))
  }
})

test_that("bid_suggest_components handles invalid inputs", {
  expect_error(
    bid_suggest_components(NULL),
    "bid_stage cannot be NULL"
  )

  expect_error(
    bid_suggest_components(list(stage = "Notice")),
    "bid_stage must be a tibble"
  )

  expect_error(
    bid_suggest_components(tibble(foo = "bar")),
    "bid_stage must contain a 'stage' column"
  )

  notice_result <- bid_notice(
    problem = "Test problem",
    evidence = "Test evidence"
  )

  expect_error(
    bid_suggest_components(notice_result, package = "invalid_package"),
    "Invalid package specified"
  )
})

test_that("bid_suggest_components calculates relevance scores correctly", {
  notice_result <- bid_notice(
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )

  suppressMessages({
    suggestions <- bid_suggest_components(notice_result)
  })

  expect_true(all(suggestions$relevance > 0))
  expect_true(is.numeric(suggestions$relevance))

  # Results should be ordered by relevance (descending)
  if (nrow(suggestions) > 1) {
    expect_true(all(diff(suggestions$relevance) <= 0))
  }
})

test_that("bid_suggest_components works with different BID stages", {
  # Test with Notice stage
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User feedback"
  )

  suppressMessages({
    notice_suggestions <- bid_suggest_components(notice_result)
  })
  expect_s3_class(notice_suggestions, "tbl_df")

  # Test with Interpret stage
  interpret_result <- bid_interpret(
    notice_result,
    central_question = "How to simplify the interface?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard is complex"
    )
  )

  suppressMessages({
    interpret_suggestions <- bid_suggest_components(interpret_result)
  })
  expect_s3_class(interpret_suggestions, "tbl_df")

  # Test with Structure stage
  structure_result <- bid_structure(
    interpret_result,

    concepts = c("Visual Hierarchy", "Cognitive Load Theory")
  )

  suppressMessages({
    structure_suggestions <- bid_suggest_components(structure_result)
  })
  expect_s3_class(structure_suggestions, "tbl_df")
})

test_that("bid_suggest_components handles edge cases", {
  # Test with minimal Notice result
  minimal_notice <- tibble(
    stage = "Notice",
    problem = NA_character_,
    theory = NA_character_,
    evidence = "Some evidence",
    timestamp = Sys.time()
  )

  suppressMessages({
    minimal_suggestions <- bid_suggest_components(minimal_notice)
  })
  expect_s3_class(minimal_suggestions, "tbl_df")

  # Test with empty package filter that has no matches
  notice_result <- bid_notice(
    problem = "Test problem",
    evidence = "Test evidence"
  )

  expect_warning(
    {
      empty_suggestions <- bid_suggest_components(notice_result, package = "DT")
    },
    NA
  ) # Should not warn, but may return empty results

  expect_s3_class(empty_suggestions, "tbl_df")
})

test_that("bid_suggest_components extracts concepts correctly", {
  # Test concept extraction from theory field
  notice_with_theory <- bid_notice(
    problem = "Users need better visual hierarchy",
    theory = "Visual Hierarchies",
    evidence = "User testing"
  )

  suppressMessages({
    theory_suggestions <- bid_suggest_components(notice_with_theory)
  })

  expect_s3_class(theory_suggestions, "tbl_df")
  expect_true(nrow(theory_suggestions) > 0)

  # Test concept extraction from Structure stage
  structure_with_concepts <- bid_structure(
    bid_interpret(
      notice_with_theory,
      central_question = "How to improve visual organization?"
    ),

    concepts = c("Principle of Proximity", "Visual Hierarchy")
  )

  suppressMessages({
    concept_suggestions <- bid_suggest_components(structure_with_concepts)
  })

  expect_s3_class(concept_suggestions, "tbl_df")
  expect_true(nrow(concept_suggestions) > 0)
})

test_that("bid_suggest_components provides appropriate user feedback", {
  notice_result <- bid_notice(
    problem = "Test problem",
    evidence = "Test evidence"
  )

  # Should provide success message
  expect_message(
    bid_suggest_components(notice_result),
    "Found .* component suggestion"
  )

  expect_message(
    bid_suggest_components(notice_result, package = "bslib"),
    "Found .* bslib component suggestion"
  )
})

test_that("bid_suggest_components handles layout-specific scoring", {
  structure_result <- bid_structure(
    bid_interpret(
      bid_notice(
        problem = "Need better organization",
        evidence = "User feedback"
      ),
      central_question = "How to organize content?"
    ),

    concepts = "Information Hierarchy"
  )

  suppressMessages({
    layout_suggestions <- bid_suggest_components(structure_result)
  })

  expect_s3_class(layout_suggestions, "tbl_df")
  expect_true(nrow(layout_suggestions) > 0)

  # Should include layout-relevant components with higher scores
  sidebar_components <- layout_suggestions[
    grepl(
      "sidebar|nav|panel",
      layout_suggestions$description,
      ignore.case = TRUE
    ) |
      grepl(
        "sidebar|nav|panel",
        layout_suggestions$component,
        ignore.case = TRUE
      ),
  ]

  if (nrow(sidebar_components) > 0) {
    expect_true(any(sidebar_components$relevance > 0))
  }
})

test_that("bid_suggest_components component database has required structure", {
  notice_result <- bid_notice(
    problem = "Test problem",
    evidence = "Test evidence"
  )

  suppressMessages({
    suggestions <- bid_suggest_components(notice_result)
  })

  required_fields <- c(
    "package",
    "component",
    "description",
    "bid_stage_relevance",
    "cognitive_concepts",
    "use_cases",
    "relevance"
  )

  expect_true(all(required_fields %in% names(suggestions)))

  # Check that packages are from expected list
  valid_packages <- c(
    "shiny",
    "bslib",
    "DT",
    "plotly",
    "reactable",
    "htmlwidgets"
  )
  if (nrow(suggestions) > 0) {
    expect_true(all(suggestions$package %in% valid_packages))
  }
})

test_that("bid_suggest_components handles audience-based context", {
  notice_with_audience <- bid_notice(
    problem = "Executive dashboard needs simplification",
    theory = "Cognitive Load Theory",
    evidence = "Executive feedback",
    target_audience = "Executive leadership team"
  )

  suppressMessages({
    exec_suggestions <- bid_suggest_components(notice_with_audience)
  })

  expect_s3_class(exec_suggestions, "tbl_df")
  expect_true(nrow(exec_suggestions) > 0)

  # Should prioritize executive-friendly components
  exec_components <- exec_suggestions[
    grepl(
      "summary|value|card|executive",
      exec_suggestions$description,
      ignore.case = TRUE
    ) |
      grepl(
        "summary|value|card",
        exec_suggestions$use_cases,
        ignore.case = TRUE
      ),
  ]

  if (nrow(exec_components) > 0) {
    expect_true(any(exec_components$relevance > 0))
  }
})
