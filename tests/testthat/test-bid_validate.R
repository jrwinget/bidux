test_that("bid_validate works with valid inputs", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure  
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Validate")
  expect_equal(
    result$summary_panel,
    "Dashboard simplified for quicker insights"
  )
  expect_equal(result$collaboration, "Added team annotation features")
  expect_match(result$previous_bias, "anchoring: Provide reference points")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_validate fails with missing previous_stage", {
  expect_error(
    bid_validate(summary_panel = "Test", collaboration = "Test"),
    "argument \"previous_stage\" is missing, with no default"
  )
})

test_that("bid_validate allows optional parameters", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory", 
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  # Should not error when only summary_panel is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test"
    )
  )

  # Should not error when only collaboration is provided
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      collaboration = "Test"
    )
  )
})

test_that("bid_validate validates boolean parameters", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(anchoring = "Provide reference points")
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result
  )

  # Test invalid include_exp_design
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      include_exp_design = "not_logical"
    ),
    "Parameter 'include_exp_design' must be a single logical value \\(TRUE/FALSE\\)"
  )

  # Test invalid include_telemetry
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      include_telemetry = c(TRUE, FALSE)
    ),
    "Parameter 'include_telemetry' must be a single logical value \\(TRUE/FALSE\\)"
  )

  # Test invalid include_empower_tools
  expect_error(
    bid_validate(
      previous_stage = structure_result,
      include_empower_tools = 1
    ),
    "Parameter 'include_empower_tools' must be a single logical value \\(TRUE/FALSE\\)"
  )

  # Test valid boolean values work
  expect_no_error(
    bid_validate(
      previous_stage = structure_result,
      include_exp_design = FALSE,
      include_telemetry = TRUE,
      include_empower_tools = FALSE
    )
  )
})

test_that("bid_validate provides contextual suggestions", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Dashboard improved",
    collaboration = "Added team features"
  )

  # Check that suggestions are contextual (not specific patterns)
  expect_true(nchar(result$suggestions) > 0)
  expect_type(result$suggestions, "character")
})

test_that("bid_validate auto-suggests summary_panel when NULL", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = NULL,
      collaboration = "Added team annotation features"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$summary_panel[1]))
  expect_true(nchar(result$summary_panel[1]) > 0)
  # Auto-suggested summary should be meaningful
  expect_true(nchar(result$summary_panel[1]) > 10)
})

test_that("bid_validate auto-suggests collaboration when NULL", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    ),
    interaction_principles = list(
      hover = "Show details on hover",
      feedback = "Visual feedback for selected items"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test summary",
      collaboration = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$collaboration[1]))
  expect_true(nchar(result$collaboration[1]) > 0)
  # Auto-suggested collaboration should be meaningful
  expect_true(nchar(result$collaboration[1]) > 10)
})

test_that("bid_validate auto-suggests next_steps when NULL", {
  # New workflow: Interpret -> Notice -> Anticipate -> Structure
  interpret_result <- bid_interpret(
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  anticipate_result <- bid_anticipate(
    previous_stage = notice_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  
  structure_result <- bid_structure(
    previous_stage = anticipate_result,
    concepts = c("Principle of Proximity", "Default Effect")
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = NULL
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))
  expect_true(nchar(result$next_steps[1]) > 0)
  # Should contain multiple steps (semicolon-separated)
  expect_gt(stringr::str_count(result$next_steps[1], ";"), 0)
})

test_that("bid_validate handles NA values in previous_stage fields", {
  anticipate_result <- tibble(
    stage = "Anticipate",
    bias_mitigations = NA_character_,
    interaction_principles = NA_character_,
    previous_layout = NA_character_,
    previous_concepts = NA_character_,
    previous_accessibility = NA_character_,
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_validate(
      previous_stage = anticipate_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration"
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$previous_bias[1]))
  # previous_interaction was removed, no longer testing it
  expect_true(is.na(result$previous_layout[1]))
  expect_true(is.na(result$previous_concepts[1]))
  expect_true(is.na(result$previous_accessibility[1]))

  expect_false(is.na(result$summary_panel[1]))
  expect_false(is.na(result$collaboration[1]))
  expect_false(is.na(result$suggestions[1]))
})

test_that("bid_validate handles next_steps edge cases", {
  structure_result <- tibble(
    stage = "Structure",
    components_overview = "test components",
    timestamp = Sys.time()
  )

  # Test with short steps - should work without warning
  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration",
    next_steps = c("OK", "Good", "Review dashboard", "Implement changes")
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  # Test with long steps - should work without warning
  long_step <- paste(
    rep(
      "This is a very long next step description that goes into excessive detail. ",
      5
    ),
    collapse = ""
  )

  result <- bid_validate(
    previous_stage = structure_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration",
    next_steps = c("Step 1", long_step, "Step 3")
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))

  # Test with empty steps - should auto-suggest
  suppressMessages(
    result <- bid_validate(
      previous_stage = structure_result,
      summary_panel = "Test summary",
      collaboration = "Test collaboration",
      next_steps = c("", "  ", "")
    )
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$next_steps[1]))
  expect_true(nchar(result$next_steps[1]) > 0)
})

test_that("bid_validate handles summary_panel and collaboration variations", {
  anticipate_result <- tibble(
    stage = "Anticipate",
    bias_mitigations = "test: value",
    timestamp = Sys.time()
  )

  # Test with short summary - should still work
  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Too short",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, "Too short")
  expect_true(nchar(result$suggestions) > 0)

  # Test with long summary - should still work
  long_summary <- paste(
    rep(
      "This is a very detailed summary that contains excessive information about the dashboard. ",
      10
    ),
    collapse = ""
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = long_summary,
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$summary_panel, long_summary)
  expect_true(nchar(result$suggestions) > 0)

  # Test with basic collaboration
  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Basic features only"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$collaboration, "Basic features only")
  expect_true(nchar(result$suggestions) > 0)
})

test_that("bid_validate properly handles interaction_principles JSON", {
  anticipate_result <- tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: Test",
    interaction_principles = "{\"hover\":\"Show on hover\",\"selection\":\"Highlight selected\"}",
    timestamp = Sys.time()
  )

  result <- bid_validate(
    previous_stage = anticipate_result,
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  expect_s3_class(result, "tbl_df")
  # previous_interaction was removed in refactor, no longer testing it
  expect_type(result$suggestions, "character")
})

test_that("bid_validate tailors collaboration by audience and empower flag", {
  # Executive audience with empower tools (default TRUE)
  prev_exec <- tibble::tibble(
    stage = "Interpret",
    audience = "Executive managers",
    central_question = "How to compare regions?",
    timestamp = Sys.time()
  )

  suppressMessages({
    res_exec <- bid_validate(
      previous_stage = prev_exec,
      summary_panel = "Summary",
      collaboration = NULL
    )
  })
  expect_true(grepl(
    "Executive-focused collaboration",
    res_exec$collaboration,
    ignore.case = TRUE
  ))
  expect_true(
    grepl(
      "decision tracking|empowerment",
      res_exec$suggestions,
      ignore.case = TRUE
    ) ||
      grepl("Executive", res_exec$collaboration, ignore.case = TRUE)
  )

  # Analyst audience without empower tools
  prev_analyst <- tibble::tibble(
    stage = "Interpret",
    audience = "Data analysts",
    central_question = "How to compare models?",
    timestamp = Sys.time()
  )

  suppressMessages({
    res_analyst <- bid_validate(
      previous_stage = prev_analyst,
      summary_panel = "Summary",
      collaboration = NULL,
      include_empower_tools = FALSE
    )
  })
  expect_true(grepl(
    "Advanced collaboration tools",
    res_analyst$collaboration,
    ignore.case = TRUE
  ))
  # ensure empower phrasing is not forced when flag is FALSE
  expect_false(grepl("empower", res_analyst$collaboration, ignore.case = TRUE))
})

test_that("bid_validate adds exp design and telemetry suggestions when missing", {
  prev <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = "anchoring: Provide reference points",
    timestamp = Sys.time()
  )

  res <- bid_validate(
    previous_stage = prev,
    summary_panel = "Brief summary",
    collaboration = "Basic sharing",
    next_steps = c("Document decisions"), # no 'test'/'experiment'/'telemetry' terms
    include_exp_design = TRUE,
    include_telemetry = TRUE
  )

  expect_s3_class(res, "tbl_df")
  expect_true(grepl(
    "experimental design|A/B",
    res$suggestions,
    ignore.case = TRUE
  ))
  expect_true(grepl("telemetry|monitor", res$suggestions, ignore.case = TRUE))
})

test_that("bid_validate auto-summary handles compare/trend and problem/find/mobile/theory branches", {
  # central_question contains 'compare'
  prev_compare <- tibble::tibble(
    stage = "Interpret",
    central_question = "How do we compare A vs B?",
    timestamp = Sys.time()
  )
  suppressMessages({
    r1 <- bid_validate(
      previous_stage = prev_compare,
      summary_panel = NULL,
      collaboration = "x"
    )
  })
  expect_true(grepl(
    "Comparative summary",
    r1$summary_panel,
    ignore.case = TRUE
  ))

  # central_question contains 'trend'
  prev_trend <- tibble::tibble(
    stage = "Interpret",
    central_question = "What trend over time should we watch?",
    timestamp = Sys.time()
  )
  suppressMessages({
    r2 <- bid_validate(
      previous_stage = prev_trend,
      summary_panel = NULL,
      collaboration = "x"
    )
  })
  expect_true(grepl("Time-based summary", r2$summary_panel, ignore.case = TRUE))

  # problem contains 'find/search' and 'mobile'
  prev_problem <- tibble::tibble(
    stage = "Notice",
    problem = "Hard to find information on mobile",
    timestamp = Sys.time()
  )
  suppressMessages({
    r3 <- bid_validate(
      previous_stage = prev_problem,
      summary_panel = NULL,
      collaboration = "x"
    )
  })
  expect_true(grepl(
    "navigation paths|Mobile-optimized",
    r3$summary_panel,
    ignore.case = TRUE
  ))

  # theory contains 'visual'
  prev_theory <- tibble::tibble(
    stage = "Notice",
    theory = "Visual Hierarchy",
    timestamp = Sys.time()
  )
  suppressMessages({
    r4 <- bid_validate(
      previous_stage = prev_theory,
      summary_panel = NULL,
      collaboration = "x"
    )
  })
  expect_true(grepl(
    "Visually hierarchical summary",
    r4$summary_panel,
    ignore.case = TRUE
  ))
})

test_that("bid_validate extracts previous info for Structure and cooperation concept path", {
  prev_structure <- tibble::tibble(
    stage = "Structure",
    layout = "grid",
    concepts = "Cooperation",
    accessibility = "Color contrast AA",
    previous_central_question = "How to simplify?",
    timestamp = Sys.time()
  )

  suppressMessages({
    res <- bid_validate(
      previous_stage = prev_structure,
      summary_panel = "S",
      collaboration = NULL
    ) # triggers concept-based collab path
  })
  expect_equal(res$previous_layout, "grid")
  expect_true(grepl(
    "Structured collaboration workflows",
    res$collaboration,
    ignore.case = TRUE
  ))
  expect_match(res$previous_concepts, "Cooperation")
  expect_match(res$previous_accessibility, "Color contrast AA")
})
