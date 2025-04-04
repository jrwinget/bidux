test_that("bid_interpret works with minimal data story elements", {
  notice <- bid_notice(
    problem = "Complex interface", 
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  result <- bid_interpret(
    previous_stage = notice,
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$stage, "Interpret")
  expect_equal(result$central_question, "How to simplify?")
  expect_equal(result$hook, "Users are confused")
  expect_equal(result$context, "Dashboard has evolved over time")
  expect_equal(result$tension, NA_character_)
  expect_equal(result$resolution, NA_character_)
  expect_equal(result$previous_problem, "Complex interface")
})

test_that("bid_interpret works with full data story elements", {
  notice <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  result <- bid_interpret(
    previous_stage = notice,
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time",
      tension = "Management wants more features",
      resolution = "Simplify core views, move extras to secondary screens",
      audience = "Marketing team",
      metrics = c("Time to insight", "User satisfaction"),
      visual_approach = "Before/after comparison"
    )
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(result$audience, "Marketing team")
  expect_equal(result$metrics, "Time to insight, User satisfaction")
  expect_equal(result$visual_approach, "Before/after comparison")
})

test_that("bid_interpret fails with missing parameters", {
  notice <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  expect_error(bid_interpret(previous_stage = notice, central_question = "Test"), "must be provided")
  expect_error(bid_interpret(previous_stage = notice, data_story = list()), "must be provided")
  expect_error(bid_interpret(central_question = "Test", data_story = list()), "must be provided")
})

test_that("bid_interpret provides appropriate suggestions", {
  notice <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  # Test with incomplete story
  incomplete_result <- bid_interpret(
    previous_stage = notice,
    central_question = "How to simplify?",
    data_story = list(hook = "Users are confused")
  )
  expect_match(incomplete_result$suggestions, "enhancing|missing", ignore.case = TRUE)
  
  # Test with very short question
  short_q_result <- bid_interpret(
    previous_stage = notice,
    central_question = "Simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved",
      tension = "Management wants more features",
      resolution = "Simplify core views"
    )
  )
  expect_match(short_q_result$suggestions, "specificity", ignore.case = TRUE)
})
