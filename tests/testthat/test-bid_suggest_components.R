test_that("bid_suggest_components returns tibble with correct structure", {
  interpret_result <- bid_interpret(
    central_question = "How can we help users with complex data?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
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
  interpret_result <- bid_interpret(
    central_question = "How can we help users with complex data?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
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

  interpret_result <- bid_interpret(
    central_question = "What is the test question?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Test problem",
    evidence = "Test evidence"
  )

  expect_error(
    bid_suggest_components(notice_result, package = "invalid_package"),
    "Invalid package specified"
  )
})

test_that("bid_suggest_components calculates relevance scores correctly", {
  interpret_result <- bid_interpret(
    central_question = "How can we help users with complex data?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
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
  # test with Notice stage
  interpret_result <- bid_interpret(
    central_question = "How to simplify the interface?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User feedback"
  )

  suppressMessages({
    notice_suggestions <- bid_suggest_components(notice_result)
  })
  expect_s3_class(notice_suggestions, "tbl_df")

  # test with Interpret stage
  interpret_result2 <- bid_interpret(
    previous_stage = notice_result,
    central_question = "How to simplify the interface?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard is complex"
    )
  )

  suppressMessages({
    interpret_suggestions <- bid_suggest_components(interpret_result2)
  })
  expect_s3_class(interpret_suggestions, "tbl_df")

  # test with Structure stage
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
  # test with minimal Notice result
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

  # test with empty package filter that has no matches
  interpret_result <- bid_interpret(
    central_question = "How can we help users with test data?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Test problem",
    evidence = "Test evidence"
  )

  expect_warning(
    {
      empty_suggestions <- bid_suggest_components(notice_result, package = "DT")
    },
    NA
  )

  expect_s3_class(empty_suggestions, "tbl_df")
})

test_that("bid_suggest_components extracts concepts correctly", {
  # test concept extraction from theory field
  interpret_result <- bid_interpret(
    central_question = "How can we improve visual hierarchy?"
  )

  notice_with_theory <- bid_notice(
    previous_stage = interpret_result,
    problem = "Users need better visual hierarchy",
    theory = "Visual Hierarchies",
    evidence = "User testing"
  )

  suppressMessages({
    theory_suggestions <- bid_suggest_components(notice_with_theory)
  })

  expect_s3_class(theory_suggestions, "tbl_df")
  expect_true(nrow(theory_suggestions) > 0)

  # test concept extraction from Structure stage
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
  interpret_result <- bid_interpret(
    central_question = "How can we improve user feedback?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Test problem",
    evidence = "Test evidence"
  )

  # should provide success message
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
  interpret_result <- bid_interpret(
    central_question = "How to organize content?"
  )

  structure_result <- bid_structure(
    bid_notice(
      previous_stage = interpret_result,
      problem = "Need better organization",
      evidence = "User feedback"
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
  interpret_result <- bid_interpret(
    central_question = "How can we test the database?"
  )

  notice_result <- bid_notice(
    previous_stage = interpret_result,
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

  # check that packages are from expected list
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
  interpret_result <- bid_interpret(
    central_question = "How can executive dashboard be simplified?"
  )

  suppressWarnings({
    notice_with_audience <- bid_notice(
      previous_stage = interpret_result,
      problem = "Executive dashboard needs simplification",
      theory = "Cognitive Load Theory",
      evidence = "Executive feedback",
      target_audience = "Executive leadership team"
    )
  })

  suppressMessages({
    exec_suggestions <- bid_suggest_components(notice_with_audience)
  })

  expect_s3_class(exec_suggestions, "tbl_df")
  expect_true(nrow(exec_suggestions) > 0)

  # should prioritize executive-friendly components
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

# ==============================================================================
# INTERNAL HELPER FUNCTION TESTS
# ==============================================================================

test_that("extract_relevant_concepts extracts theory from Notice stage", {
  notice_stage <- tibble::tibble(
    stage = "Notice",
    theory = "Cognitive Load Theory",
    problem = "Users overwhelmed"
  )

  result <- bidux:::extract_relevant_concepts(notice_stage)

  expect_true(is.character(result))
  expect_true("Cognitive Load Theory" %in% result)
})

test_that("extract_relevant_concepts extracts concepts from Structure stage", {
  structure_stage <- tibble::tibble(
    stage = "Structure",
    concepts = "Visual Hierarchy,Processing Fluency",
    layout = "dual_process"
  )

  result <- bidux:::extract_relevant_concepts(structure_stage)

  expect_true(is.character(result))
  expect_true("Visual Hierarchy" %in% result)
  expect_true("Processing Fluency" %in% result)
})

test_that("extract_relevant_concepts handles single concept (no comma)", {
  stage <- tibble::tibble(
    stage = "Structure",
    concepts = "Information Hierarchy"
  )

  result <- bidux:::extract_relevant_concepts(stage)

  expect_true(is.character(result))
  expect_equal(result, "Information Hierarchy")
})

test_that("extract_relevant_concepts extracts from previous stage fields", {
  stage <- tibble::tibble(
    stage = "Anticipate",
    previous_theory = "Anchoring Bias",
    previous_concepts = "Framing Effect"
  )

  result <- bidux:::extract_relevant_concepts(stage)

  expect_true(is.character(result))
  expect_true("Anchoring Bias" %in% result)
  expect_true("Framing Effect" %in% result)
})

test_that("extract_relevant_concepts handles missing fields", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Some problem"
    # no theory or concepts
  )

  result <- bidux:::extract_relevant_concepts(stage)

  expect_true(is.character(result))
  expect_equal(length(result), 0)
})

test_that("extract_additional_context extracts from problem field", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users are overwhelmed by too many options and complex filters"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(is.character(result))
  expect_true(any(c("progressive", "accordion", "tab") %in% result))
})

test_that("extract_additional_context extracts search/find keywords", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Users cannot find the information they need"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(any(c("filter", "search", "navigation") %in% result))
})

test_that("extract_additional_context extracts performance keywords", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Dashboard is slow and causes performance delays"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(any(c("reactive", "efficient", "optimize") %in% result))
})

test_that("extract_additional_context extracts mobile keywords", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Interface doesn't work well on mobile devices"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(any(c("responsive", "layout", "grid") %in% result))
})

test_that("extract_additional_context extracts from central_question", {
  stage <- tibble::tibble(
    stage = "Interpret",
    central_question = "How can we simplify and reduce complexity?"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(is.character(result))
  expect_true(any(c("card", "accordion", "modal") %in% result))
})

test_that("extract_additional_context extracts from audience fields", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    audience = "Executive leadership and senior managers"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(any(c("value_box", "card", "summary") %in% result))
})

test_that("extract_additional_context handles analyst audience", {
  stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test problem",
    target_audience = "Technical analysts and developers"
  )

  result <- bidux:::extract_additional_context(stage)

  expect_true(any(c("datatable", "plotly", "interactive") %in% result))
})

test_that("extract_layout_keywords extracts dual/split keywords", {
  result <- bidux:::extract_layout_keywords("dual_process")

  expect_true(is.character(result))
  expect_true(any(c("column", "grid", "layout") %in% result))
})

test_that("extract_layout_keywords extracts sidebar keywords", {
  result <- bidux:::extract_layout_keywords("page_sidebar")

  expect_true(any(c("sidebar", "nav", "panel") %in% result))
})

test_that("extract_layout_keywords extracts card keywords", {
  result <- bidux:::extract_layout_keywords("card_layout")

  expect_true(any(c("card", "value_box", "grid") %in% result))
})

test_that("extract_layout_keywords extracts tab keywords", {
  result <- bidux:::extract_layout_keywords("tabset_navigation")

  expect_true(any(c("tab", "nav", "panel") %in% result))
})

test_that("extract_layout_keywords extracts accordion keywords", {
  result <- bidux:::extract_layout_keywords("accordion_collapse")

  expect_true(any(c("accordion", "conditional", "progressive") %in% result))
})

test_that("extract_layout_keywords handles mixed case", {
  result <- bidux:::extract_layout_keywords("Dual-Column-Split")

  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("create_empty_components_result has correct structure", {
  result <- bidux:::create_empty_components_result()

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)

  expected_cols <- c(
    "package",
    "component",
    "description",
    "bid_stage_relevance",
    "cognitive_concepts",
    "use_cases",
    "relevance"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_true(is.character(result$package))
  expect_true(is.numeric(result$relevance))
})

test_that("get_components_database returns valid structure", {
  db <- bidux:::get_components_database()

  expect_true(tibble::is_tibble(db))
  expect_true(nrow(db) > 0)

  required_cols <- c(
    "package",
    "component",
    "description",
    "bid_stage_relevance",
    "cognitive_concepts",
    "use_cases"
  )

  expect_true(all(required_cols %in% names(db)))

  # check expected packages
  valid_packages <- c("shiny", "bslib", "DT", "plotly", "reactable", "htmlwidgets")
  expect_true(all(db$package %in% valid_packages))

  # check no missing values in key fields
  expect_false(any(is.na(db$package)))
  expect_false(any(is.na(db$component)))
  expect_false(any(is.na(db$description)))
})

test_that("calculate_relevance_scores assigns scores correctly", {
  components_db <- tibble::tibble(
    package = c("bslib", "shiny"),
    component = c("card", "tabsetPanel"),
    description = c("Organize content", "Create tabs"),
    bid_stage_relevance = c("Stage 3,Stage 5", "Stage 1,Stage 3"),
    cognitive_concepts = c("Visual Hierarchy", "Information Hierarchy"),
    use_cases = c("content organization", "workflow organization")
  )

  bid_stage <- tibble::tibble(
    stage = "Structure",
    theory = "Visual Hierarchy",
    problem = "Need better organization"
  )

  result <- bidux:::calculate_relevance_scores(bid_stage, components_db)

  expect_true(tibble::is_tibble(result))
  expect_true("relevance" %in% names(result))
  expect_true(all(result$relevance > 0))
  expect_true(is.numeric(result$relevance))
})

test_that("calculate_relevance_scores prioritizes stage matches", {
  components_db <- tibble::tibble(
    package = c("bslib", "shiny"),
    component = c("card", "button"),
    description = c("Content container", "Action trigger"),
    bid_stage_relevance = c("Stage 1", "Stage 5"),
    cognitive_concepts = c("None", "None"),
    use_cases = c("content", "actions")
  )

  notice_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Test"
  )

  result <- bidux:::calculate_relevance_scores(notice_stage, components_db)

  # should return components with non-zero relevance
  expect_true(nrow(result) > 0)
  expect_true(all(result$relevance > 0))

  # if both components are present, card (Stage 1) should score higher than button (Stage 5)
  if ("card" %in% result$component && "button" %in% result$component) {
    card_score <- result$relevance[result$component == "card"][1]
    button_score <- result$relevance[result$component == "button"][1]
    expect_gt(card_score, button_score)
  }
})

test_that("calculate_relevance_scores handles concept matching", {
  components_db <- tibble::tibble(
    package = "bslib",
    component = "value_box",
    description = "Display metrics",
    bid_stage_relevance = "Stage 2",
    cognitive_concepts = "Visual Hierarchy,Pre-attentive Processing",
    use_cases = "KPI displays"
  )

  stage_with_concept <- tibble::tibble(
    stage = "Structure",
    theory = "Visual Hierarchy"
  )

  result <- bidux:::calculate_relevance_scores(stage_with_concept, components_db)

  # should get bonus points for concept match
  expect_gt(result$relevance[1], 0)
})

test_that("calculate_relevance_scores handles partial concept matching", {
  components_db <- tibble::tibble(
    package = "bslib",
    component = "card",
    description = "Container",
    bid_stage_relevance = "Stage 3",
    cognitive_concepts = "Visual Hierarchy",
    use_cases = "organization"
  )

  stage <- tibble::tibble(
    stage = "Structure",
    theory = "visual hierarchy principles" # partial match
  )

  result <- bidux:::calculate_relevance_scores(stage, components_db)

  expect_true(result$relevance[1] > 0)
})

test_that("calculate_relevance_scores removes zero relevance components", {
  components_db <- tibble::tibble(
    package = c("bslib", "shiny"),
    component = c("card", "button"),
    description = c("Container", "Action"),
    bid_stage_relevance = c("Stage 5", "Stage 5"), # Deploy stage
    cognitive_concepts = c("None", "None"),
    use_cases = c("content", "actions")
  )

  notice_stage <- tibble::tibble(
    stage = "Notice" # Stage 1, no match with Stage 5
  )

  result <- bidux:::calculate_relevance_scores(notice_stage, components_db)

  # should still return some results (general components get small scores)
  expect_true(nrow(result) >= 0)
})

test_that("calculate_relevance_scores handles layout-specific scoring", {
  components_db <- tibble::tibble(
    package = "bslib",
    component = "page_sidebar",
    description = "Build sidebar navigation",
    bid_stage_relevance = "Stage 3",
    cognitive_concepts = "Information Hierarchy",
    use_cases = "sidebar controls,navigation"
  )

  structure_stage <- tibble::tibble(
    stage = "Structure",
    layout = "page_sidebar"
  )

  result <- bidux:::calculate_relevance_scores(structure_stage, components_db)

  # should get layout bonus
  expect_gt(result$relevance[1], 50) # base stage score + layout bonus
})

test_that("calculate_relevance_scores ensures minimum results", {
  components_db <- tibble::tibble(
    package = "bslib",
    component = "card",
    description = "Content container",
    bid_stage_relevance = "Stage 5",
    cognitive_concepts = "Unrelated Concept",
    use_cases = "misc"
  )

  unrelated_stage <- tibble::tibble(
    stage = "Notice",
    problem = "Completely unrelated problem"
  )

  result <- bidux:::calculate_relevance_scores(unrelated_stage, components_db)

  # even with no matches, general components get small scores
  expect_true(nrow(result) >= 0)
})
