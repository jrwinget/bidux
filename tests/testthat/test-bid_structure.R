test_that("bid_structure works with valid inputs", {
  interpret <- bid_interpret(
    bid_notice(
      problem = "Complex interface",
      theory = "Cognitive Load Theory",
      evidence = "User complaints"
    ),
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  result <- bid_structure(
    previous_stage = interpret,
    layout = "dual_process",
    concepts = c("Principle of Proximity", "Default Effect")
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Structure")
  expect_equal(result$layout, "dual_process")
  expect_equal(result$concepts, "Principle of Proximity, Default Effect")
  expect_equal(result$previous_question, "How to simplify?")
  expect_true(!is.na(result$suggestions))
})

test_that("bid_structure warns about unknown concepts", {
  interpret <- bid_interpret(
    bid_notice(
      problem = "Complex interface",
      theory = "Cognitive Load Theory",
      evidence = "User complaints"
    ),
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  expect_warning(
    bid_structure(
      previous_stage = interpret,
      layout = "dual_process",
      concepts = c("Principle of Proximity", "Unknown Concept")
    ),
    "not recognized"
  )
})

test_that("bid_structure fails with missing parameters", {
  interpret <- bid_interpret(
    bid_notice(
      problem = "Complex interface",
      theory = "Cognitive Load Theory",
      evidence = "User complaints"
    ),
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  expect_error(bid_structure(previous_stage = interpret, layout = "dual_process"), "must be provided")
  expect_error(bid_structure(previous_stage = interpret, concepts = c("Default Effect")), "must be provided")
  expect_error(bid_structure(layout = "dual_process", concepts = c("Default Effect")), "must be provided")
})

test_that("bid_structure provides layout-specific suggestions", {
  interpret <- bid_interpret(
    bid_notice(
      problem = "Complex interface",
      theory = "Cognitive Load Theory",
      evidence = "User complaints"
    ),
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  # Test different layouts
  card_result <- bid_structure(
    previous_stage = interpret,
    layout = "card",
    concepts = c("Principle of Proximity")
  )
  expect_match(card_result$suggestions, "cards", ignore.case = TRUE)
  
  tabs_result <- bid_structure(
    previous_stage = interpret,
    layout = "tabs",
    concepts = c("Principle of Proximity")
  )
  expect_match(tabs_result$suggestions, "tabs", ignore.case = TRUE)
})
