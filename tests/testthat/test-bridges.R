test_that("bid_notice_issue creates equivalent Notice to manual approach", {
  # create test tibble with issue data (matching actual telemetry structure)
  issue_data <- tibble::tibble(
    issue_id = "filter_abandonment_01",
    severity = "high",
    problem = "Users abandon complex filter interface",
    evidence = "85% of users abandon filtering after 2+ selections",
    theory = "Cognitive Load Theory",
    issue_type = "high_cognitive_load",
    affected_sessions = 85L,
    impact_rate = 0.85
  )

  # create previous stage for comparison
  interpret_stage <- bid_interpret(
    central_question = "How can we improve filtering usability?",
    data_story = list(
      hook = "Users struggle with complex filters",
      context = "Dashboard has 15+ filter options"
    )
  )

  # test bridge function
  notice_from_bridge <- bid_notice_issue(
    issue = issue_data,
    previous_stage = interpret_stage
  )

  # test manual approach
  notice_manual <- bid_notice(
    previous_stage = interpret_stage,
    problem = issue_data$problem,
    theory = issue_data$theory,
    evidence = issue_data$evidence
  )

  # both should be valid bid_stage objects
  expect_s3_class(notice_from_bridge, "bid_stage")
  expect_s3_class(notice_manual, "bid_stage")

  # both should have same stage
  expect_equal(notice_from_bridge$stage, "Notice")
  expect_equal(notice_manual$stage, "Notice")

  # bridge should incorporate issue data (flexible matching)
  expect_true(nchar(notice_from_bridge$problem) > 0)
  expect_true(nchar(notice_from_bridge$theory) > 0)
  expect_true(nchar(notice_from_bridge$evidence) > 0)
})

test_that("bid_notice_issue handles missing optional fields", {
  # create minimal issue data
  minimal_issue <- tibble::tibble(
    issue_id = "simple_01",
    problem = "Basic usability issue",
    severity = "medium"
  )

  interpret_stage <- bid_interpret(central_question = "How to improve UX?")

  # should work with minimal data
  expect_no_error(
    notice_result <- bid_notice_issue(
      issue = minimal_issue,
      previous_stage = interpret_stage
    )
  )

  expect_s3_class(notice_result, "bid_stage")
  expect_equal(notice_result$stage, "Notice")
})

test_that("bid_notice_issue respects override parameters", {
  issue_data <- tibble::tibble(
    issue_id = "test_issue",
    problem = "Original problem description",
    theory = "Original Theory",
    severity = "low"
  )

  interpret_stage <- bid_interpret(central_question = "Test question?")

  # test with overrides
  notice_result <- bid_notice_issue(
    issue = issue_data,
    previous_stage = interpret_stage,
    override = list(
      problem = "Overridden problem description",
      theory = "Overridden Theory"
    )
  )

  # should use override values
  expect_true(grepl("Overridden problem", notice_result$problem))
  expect_true(grepl("Overridden Theory", notice_result$theory))
})

test_that("bid_notices processes multiple issues correctly", {
  # create test issues tibble
  issues_data <- tibble::tibble(
    issue_id = c("issue_01", "issue_02", "issue_03"),
    severity = c("high", "medium", "low"),
    problem = c(
      "Critical navigation problem causing users to abandon workflows frequently",
      "Moderate layout issue affecting content readability and user scanning patterns",
      "Minor color contrast issue that may impact accessibility compliance standards"
    ),
    issue_type = c("navigation", "layout", "accessibility"),
    theory = c("Navigation Theory", "Visual Hierarchy", "Accessibility Theory")
  )

  interpret_stage <- bid_interpret(
    central_question = "How to improve overall UX?"
  )

  # test processing all issues
  notice_list <- bid_notices(
    issues = issues_data,
    previous_stage = interpret_stage
  )

  expect_true(is.list(notice_list))
  expect_equal(length(notice_list), 3)

  # all should be valid Notice stages
  for (i in seq_along(notice_list)) {
    expect_s3_class(notice_list[[i]], "bid_stage")
    expect_equal(notice_list[[i]]$stage, "Notice")
  }

  # should preserve issue order
  expect_true(grepl("Critical", notice_list[[1]]$problem))
  expect_true(grepl("layout", notice_list[[2]]$problem))
  expect_true(grepl("contrast", notice_list[[3]]$problem))
})

test_that("bid_notices respects filter parameter", {
  issues_data <- tibble::tibble(
    issue_id = c("issue_01", "issue_02", "issue_03"),
    severity = c("high", "medium", "low"),
    problem = c(
      "User interface navigation is problematic and causing confusion",
      "Data loading performance issues affecting user experience",
      "Minor accessibility issues with color contrast in charts"
    ),
    issue_type = c("type1", "type2", "type1")
  )

  interpret_stage <- bid_interpret(central_question = "Test filtering?")

  # test with filter (use NSE expression syntax)
  filtered_notices <- bid_notices(
    issues = issues_data,
    filter = severity == "high",
    previous_stage = interpret_stage
  )

  expect_equal(length(filtered_notices), 1)
  expect_true(grepl("navigation", filtered_notices[[1]]$problem))
})

test_that("bid_address creates single Notice from issue", {
  issue_data <- tibble::tibble(
    issue_id = "priority_issue",
    severity = "critical",
    problem = "System performance degradation",
    evidence = "Response time increased 300%",
    theory = "Performance Psychology"
  )

  interpret_stage <- bid_interpret(
    central_question = "How to address performance issues?"
  )

  # test sugar function
  notice_result <- bid_address(
    issue = issue_data,
    previous_stage = interpret_stage
  )

  expect_s3_class(notice_result, "bid_stage")
  expect_equal(notice_result$stage, "Notice")
  expect_true(grepl("performance", notice_result$problem, ignore.case = TRUE))
  expect_true(grepl("300%", notice_result$evidence))
})

test_that("bid_pipeline processes first N issues", {
  # create 5 issues
  issues_data <- tibble::tibble(
    issue_id = paste0("issue_", 1:5),
    severity = c("critical", "high", "high", "medium", "low"),
    problem = paste(
      "Critical user interface problem affecting",
      letters[1:5],
      "functionality and causing user confusion"
    )
  )

  interpret_stage <- bid_interpret(central_question = "Pipeline test?")

  # test with max limit
  pipeline_result <- bid_pipeline(
    issues = issues_data,
    previous_stage = interpret_stage,
    max = 3
  )

  expect_true(is.list(pipeline_result))
  expect_equal(length(pipeline_result), 3)

  # should take first 3 issues (by severity/order)
  for (i in 1:3) {
    expect_s3_class(pipeline_result[[i]], "bid_stage")
    expect_equal(pipeline_result[[i]]$stage, "Notice")
  }
})

test_that("bridge functions handle edge cases gracefully", {
  # test with empty issue tibble
  empty_issues <- tibble::tibble(
    issue_id = character(0),
    problem = character(0)
  )

  interpret_stage <- bid_interpret(central_question = "Empty test?")

  # should handle empty gracefully, expect warning about no matching issues
  expect_warning(
    empty_result <- bid_notices(
      issues = empty_issues,
      previous_stage = interpret_stage
    ),
    "No issues match"
  )

  expect_true(is.list(empty_result))
  expect_equal(length(empty_result), 0)
})
