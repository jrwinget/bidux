# ==============================================================================
# TELEMETRY NOTICE CREATION TESTS
# ==============================================================================

test_that("create_unused_input_notice generates valid notice stage", {
  # test with zero usage
  input_info_zero <- list(
    input_id = "unused_filter",
    sessions_used = 0,
    usage_rate = 0
  )

  result_zero <- bidux:::create_unused_input_notice(input_info_zero, total_sessions = 100)

  expect_s3_class(result_zero, "bid_stage")
  expect_equal(get_stage(result_zero), "Notice")
  expect_true(grepl("unused_filter", result_zero$problem[1]))
  expect_true(grepl("0 out of 100", result_zero$evidence[1]))

  # test with low usage
  input_info_low <- list(
    input_id = "filter_button",
    sessions_used = 5,
    usage_rate = 0.05
  )

  result_low <- bidux:::create_unused_input_notice(input_info_low, total_sessions = 100)

  expect_s3_class(result_low, "bid_stage")
  expect_true(grepl("5 out of 100", result_low$evidence[1]))
  expect_true(grepl("5.0%", result_low$evidence[1]))
})

test_that("create_delay_notice generates valid notice stage", {
  delay_info <- list(
    median_delay = 45.5,
    no_action_rate = 0.15,
    rate_over_threshold = 0.25
  )

  result <- bidux:::create_delay_notice(delay_info, total_sessions = 50, threshold = 30)

  expect_s3_class(result, "bid_stage")
  expect_equal(get_stage(result), "Notice")
  expect_true(grepl("long time", result$problem[1]))
  expect_true(grepl("46 seconds", result$evidence[1])) # rounded median
  expect_true(grepl("15%.*no interactions", result$evidence[1]))
  expect_true(grepl("25%.*over 30 seconds", result$evidence[1]))
})

test_that("create_delay_notice handles missing median delay", {
  delay_info <- list(
    median_delay = NA,
    no_action_rate = 0.20,
    rate_over_threshold = 0.10
  )

  result <- bidux:::create_delay_notice(delay_info, total_sessions = 30, threshold = 60)

  expect_s3_class(result, "bid_stage")
  # should not mention median delay in evidence
  expect_false(grepl("Median time", result$evidence[1]))
  expect_true(grepl("20%.*no interactions", result$evidence[1]))
})

test_that("create_error_notice generates valid notice stage", {
  error_info <- list(
    error_message = "Database connection failed unexpectedly during query execution",
    count = 23,
    session_rate = 0.35,
    output_id = "data_table",
    associated_input = "refresh_button"
  )

  result <- bidux:::create_error_notice(error_info, total_sessions = 100)

  expect_s3_class(result, "bid_stage")
  expect_equal(get_stage(result), "Notice")
  expect_true(grepl("error", result$problem[1], ignore.case = TRUE))
  expect_true(grepl("23 times", result$evidence[1]))
  expect_true(grepl("35%", result$evidence[1]))
  expect_true(grepl("data_table", result$evidence[1]))
  expect_true(grepl("refresh_button", result$evidence[1]))
})

test_that("create_error_notice handles NULL optional fields", {
  error_info <- list(
    error_message = NULL,
    count = 10,
    session_rate = 0.10,
    output_id = NULL,
    associated_input = NULL
  )

  result <- bidux:::create_error_notice(error_info, total_sessions = 100)

  expect_s3_class(result, "bid_stage")
  expect_true(grepl("Unknown error", result$evidence[1]))
  # should not mention output_id or associated_input
  expect_false(grepl("in output", result$evidence[1]))
  expect_false(grepl("after changing", result$evidence[1]))
})

test_that("create_navigation_notice generates valid notice stage", {
  nav_info <- list(
    page = "Advanced Settings",
    unique_sessions = 12,
    visit_rate = 0.08,
    exit_rate = 0.65
  )

  result <- bidux:::create_navigation_notice(nav_info, total_sessions = 150)

  expect_s3_class(result, "bid_stage")
  expect_equal(get_stage(result), "Notice")
  expect_true(grepl("Advanced Settings", result$problem[1]))
  expect_true(grepl("12 sessions", result$evidence[1]))
  expect_true(grepl("8.0%", result$evidence[1]))
  expect_true(grepl("65%.*ended there", result$evidence[1]))
})

test_that("create_navigation_notice handles low exit rate", {
  nav_info <- list(
    page = "Dashboard",
    unique_sessions = 25,
    visit_rate = 0.15,
    exit_rate = 0.20 # below 0.5 threshold
  )

  result <- bidux:::create_navigation_notice(nav_info, total_sessions = 200)

  expect_s3_class(result, "bid_stage")
  # should not mention exit rate if below 50%
  expect_false(grepl("ended there", result$evidence[1]))
})

test_that("create_confusion_notice generates valid notice stage", {
  confusion_info <- list(
    input_id = "date_range_picker",
    affected_sessions = 18,
    total_rapid_changes = 90,
    avg_time_window = 12.5
  )

  result <- bidux:::create_confusion_notice(confusion_info, total_sessions = 100)

  expect_s3_class(result, "bid_stage")
  expect_equal(get_stage(result), "Notice")
  expect_true(grepl("confusion", result$problem[1], ignore.case = TRUE))
  expect_true(grepl("date_range_picker", result$problem[1]))
  expect_true(grepl("18 sessions", result$evidence[1]))
  expect_true(grepl("5 changes", result$evidence[1])) # 90/18 = 5
  expect_true(grepl("12.5 seconds", result$evidence[1]))
})

# ==============================================================================
# BID_ISSUES CLASS HELPER FUNCTION TESTS
# ==============================================================================

test_that(".create_issues_tibble handles empty issues list", {
  result <- bidux:::.create_issues_tibble(list(), total_sessions = 50, events = data.frame())

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
  expect_true(all(c("issue_id", "severity", "problem", "evidence") %in% names(result)))
})

test_that(".create_issues_tibble processes valid notice issues", {
  # create mock notice issues
  notice1 <- bid_notice(
    previous_stage = bid_interpret(central_question = "Test?"),
    problem = "Users struggle with navigation",
    evidence = "50% abandon the page"
  )

  notice2 <- bid_notice(
    previous_stage = bid_interpret(central_question = "Test?"),
    problem = "Error rates are high",
    evidence = "30% encounter errors"
  )

  notice_issues <- list(
    "unused_input_test" = notice1,
    "error_pattern_critical" = notice2
  )

  events_df <- data.frame(
    session_id = c("s1", "s2", "s3"),
    event_type = c("input", "error", "input"),
    input_id = c("btn", NA, "btn"),
    stringsAsFactors = FALSE
  )

  result <- bidux:::.create_issues_tibble(notice_issues, total_sessions = 100, events = events_df)

  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 2)
  expect_true("unused_input_test" %in% result$issue_id)
  expect_true("error_pattern_critical" %in% result$issue_id)
  expect_true(all(c("severity", "affected_sessions", "impact_rate") %in% names(result)))
})

test_that(".classify_issue_type identifies issue types correctly", {
  expect_equal(bidux:::.classify_issue_type("unused_input_filter"), "unused_input")
  expect_equal(bidux:::.classify_issue_type("delayed_interaction_01"), "delayed_interaction")
  expect_equal(bidux:::.classify_issue_type("error_pattern_critical"), "error_pattern")
  expect_equal(bidux:::.classify_issue_type("navigation_dropoff_page2"), "navigation_dropoff")
  expect_equal(bidux:::.classify_issue_type("confusion_pattern_slider"), "confusion_pattern")
  expect_equal(bidux:::.classify_issue_type("unknown_issue_type"), "unknown")
})

test_that(".calculate_severity_metrics handles unused input issues", {
  events_df <- data.frame(
    session_id = c("s1", "s2", "s3", "s4", "s5"),
    event_type = c("input", "input", "click", "click", "click"),
    input_id = c("filter x", "filter x", NA, NA, NA), # note: spaces, not underscores
    stringsAsFactors = FALSE
  )

  # only 2 of 5 sessions used "filter x", so 3 sessions didn't use it
  # issue_key "unused_input_filter_x" converts to input_id "filter x"
  result <- bidux:::.calculate_severity_metrics("unused_input_filter_x", events_df, total_sessions = 5)

  expect_equal(result$severity, "critical") # 60% impact >= 30% = critical
  expect_equal(result$affected_sessions, 3L)
  expect_equal(result$impact_rate, 0.6, tolerance = 0.01)
})

test_that(".calculate_severity_metrics handles error patterns", {
  events_df <- data.frame(
    session_id = c("s1", "s1", "s2", "s2", "s3"),
    event_type = c("click", "error", "click", "error", "click"),
    stringsAsFactors = FALSE
  )

  result <- bidux:::.calculate_severity_metrics("error_pattern_1", events_df, total_sessions = 3)

  expect_equal(result$severity, "critical") # 2/3 = 66% >= 30% threshold = critical
  expect_equal(result$affected_sessions, 2L)
  expect_gt(result$impact_rate, 0.5)
})

test_that(".calculate_severity_metrics returns correct severity levels", {
  events_df <- data.frame(session_id = character(0), event_type = character(0))

  # test critical (>= 30%)
  result_critical <- bidux:::.calculate_severity_metrics("delayed_01", events_df, total_sessions = 100)
  expect_equal(result_critical$severity, "critical")
  expect_equal(result_critical$impact_rate, 0.3)

  # test high (20% = high since >= 10%)
  result_high <- bidux:::.calculate_severity_metrics("navigation_page1", events_df, total_sessions = 100)
  expect_equal(result_high$severity, "high") # 20% >= 10% threshold = high
  expect_equal(result_high$impact_rate, 0.2)
})

test_that(".calculate_severity_metrics handles invalid input_id safely", {
  events_df <- data.frame(
    session_id = c("s1", "s2"),
    event_type = c("input", "input"),
    input_id = c("valid", "valid")
  )

  # test with malformed issue_key that would extract invalid input_id
  result <- bidux:::.calculate_severity_metrics("unused_input_", events_df, total_sessions = 10)

  # should fallback to conservative estimate
  expect_equal(result$affected_sessions, 1L) # 10% of 10
  expect_equal(result$impact_rate, 0.1)
})

test_that(".flags_from_issues creates correct flag structure", {
  issues_tbl <- tibble::tibble(
    issue_id = c("unused_input_x", "error_pattern_1", "navigation_page2"),
    issue_type = c("unused_input", "error_pattern", "navigation_dropoff"),
    severity = c("critical", "high", "medium")
  )

  events_df <- data.frame(
    session_id = c("s1", "s2", "s3"),
    event_type = c("input", "error", "navigation")
  )

  thresholds <- list(
    unused_input_threshold = 0.05,
    delay_threshold_seconds = 30,
    error_rate_threshold = 0.1
  )

  result <- bidux:::.flags_from_issues(issues_tbl, events_df, thresholds)

  expect_true(is.list(result))
  expect_true(result$has_issues)
  expect_true(result$has_critical_issues)
  expect_true(result$has_input_issues)
  expect_true(result$has_navigation_issues)
  expect_true(result$has_error_patterns)
  expect_false(result$has_confusion_patterns)
  expect_equal(result$session_count, 3)
  expect_equal(result$unused_input_threshold, 0.05)
})

test_that(".flags_from_issues handles empty issues", {
  empty_issues <- tibble::tibble(
    issue_id = character(0),
    issue_type = character(0),
    severity = character(0)
  )

  events_df <- data.frame(session_id = c("s1"))

  thresholds <- list(
    unused_input_threshold = 0.05,
    delay_threshold_seconds = 30,
    error_rate_threshold = 0.1
  )

  result <- bidux:::.flags_from_issues(empty_issues, events_df, thresholds)

  expect_false(result$has_issues)
  expect_false(result$has_critical_issues)
  expect_false(result$has_input_issues)
  expect_equal(result$session_count, 1)
})

# ==============================================================================
# BID_ISSUES S3 METHOD TESTS (additional edge cases)
# ==============================================================================

test_that("print.bid_issues handles empty issues gracefully", {
  # create mock empty bid_issues object
  empty_issues <- list()
  attr(empty_issues, "issues_tbl") <- tibble::tibble(
    issue_id = character(0),
    severity = character(0),
    problem = character(0),
    evidence = character(0)
  )
  attr(empty_issues, "flags") <- list(
    has_issues = FALSE,
    session_count = 10
  )
  attr(empty_issues, "created_at") <- Sys.time()
  class(empty_issues) <- c("bid_issues", "list")

  # main test: print method should not error
  expect_no_error(print(empty_issues))
})

test_that("as_tibble.bid_issues validates object structure", {
  # create invalid bid_issues object (missing issues_tbl)
  invalid_obj <- list()
  class(invalid_obj) <- c("bid_issues", "list")

  expect_error(
    as_tibble(invalid_obj),
    "missing issues_tbl attribute"
  )
})

test_that("bid_flags.bid_issues validates object structure", {
  # create invalid bid_issues object (missing flags)
  invalid_obj <- list()
  attr(invalid_obj, "issues_tbl") <- tibble::tibble()
  class(invalid_obj) <- c("bid_issues", "list")

  expect_error(
    bid_flags(invalid_obj),
    "missing flags attribute"
  )
})

test_that("bid_flags.default extracts flags from list element", {
  # object with flags as list element
  obj_with_flags <- list(
    flags = list(
      has_issues = TRUE,
      session_count = 5
    )
  )

  result <- bid_flags(obj_with_flags)

  expect_true(is.list(result))
  expect_equal(result$has_issues, TRUE)
  expect_equal(result$session_count, 5)
})

# ==============================================================================
# CONCISE TELEMETRY API TESTS (additional coverage)
# ==============================================================================

test_that("bid_notice_issue validates input structure", {
  # test with multiple rows (should error)
  multi_row_issue <- tibble::tibble(
    issue_id = c("issue1", "issue2"),
    problem = c("Problem 1", "Problem 2")
  )

  interpret <- bid_interpret(central_question = "Test?")

  expect_error(
    bid_notice_issue(multi_row_issue, previous_stage = interpret),
    "exactly one row"
  )
})

test_that("bid_notice_issue validates override parameter", {
  issue <- tibble::tibble(
    issue_id = "test",
    problem = "Test problem"
  )

  interpret <- bid_interpret(central_question = "Test?")

  # test with invalid override (not a list)
  expect_error(
    bid_notice_issue(issue, previous_stage = interpret, override = "not a list"),
    "override must be a list"
  )
})

test_that("bid_notice_issue creates default interpret stage if missing", {
  issue <- tibble::tibble(
    issue_id = "test",
    problem = "Test problem",
    severity = "medium"
  )

  # call without previous_stage - should create default internally
  result <- bid_notice_issue(issue, previous_stage = NULL)

  expect_s3_class(result, "bid_stage")
  expect_equal(get_stage(result), "Notice")
  # should have problem and evidence populated
  expect_true(nchar(result$problem[1]) > 0)
  expect_true(nchar(result$evidence[1]) > 0)
})

test_that("bid_notice_issue builds evidence from telemetry data", {
  issue <- tibble::tibble(
    issue_id = "test_id",
    problem = "Test problem",
    severity = "high",
    affected_sessions = 42L,
    impact_rate = 0.35
  )

  interpret <- bid_interpret(central_question = "Test?")

  result <- bid_notice_issue(issue, previous_stage = interpret)

  # check that telemetry data was incorporated into evidence
  expect_true(grepl("42", result$evidence[1]))
  expect_true(grepl("35", result$evidence[1])) # 35% impact rate
  expect_true(grepl("high", result$evidence[1]))
})

test_that("bid_notice_issue adds telemetry metadata to result", {
  issue <- tibble::tibble(
    issue_id = "nav_issue_01",
    issue_type = "navigation_dropoff",
    problem = "Navigation problem"
  )

  interpret <- bid_interpret(central_question = "Test?")

  result <- bid_notice_issue(issue, previous_stage = interpret)

  # check for telemetry metadata
  metadata <- attr(result, "metadata")
  expect_true("telemetry_issue_type" %in% names(metadata))
  expect_equal(metadata$telemetry_issue_type, "navigation_dropoff")
  expect_equal(metadata$telemetry_issue_id, "nav_issue_01")
})

test_that("bid_notices validates input data frame", {
  # test with non-data.frame input
  expect_error(
    bid_notices(issues = "not a data frame"),
    "must be a data frame"
  )
})

test_that("bid_notices limits results based on max_issues", {
  issues <- tibble::tibble(
    issue_id = paste0("issue_", 1:10),
    severity = rep(c("critical", "high"), 5),
    impact_rate = seq(0.5, 0.05, length.out = 10),
    problem = paste("Problem", 1:10)
  )

  interpret <- bid_interpret(central_question = "Test?")

  # capture the inform message
  expect_message(
    result <- bid_notices(issues, previous_stage = interpret, max_issues = 3),
    "Limiting to top 3"
  )

  expect_equal(length(result), 3)
})

test_that("bid_notices sorts by severity and impact_rate", {
  issues <- tibble::tibble(
    issue_id = c("low_impact", "high_impact", "critical_impact"),
    severity = c("low", "high", "critical"),
    impact_rate = c(0.02, 0.15, 0.35),
    problem = c("Low problem", "High problem", "Critical problem")
  )

  interpret <- bid_interpret(central_question = "Test?")

  result <- bid_notices(issues, previous_stage = interpret)

  # should be sorted by severity (critical first), then impact
  expect_true(grepl("Critical", result[[1]]$problem))
  expect_true(grepl("High", result[[2]]$problem))
  expect_true(grepl("Low", result[[3]]$problem))
})

test_that("bid_pipeline validates input and sorts by priority", {
  # test validation
  expect_error(
    bid_pipeline(issues = "not a data frame", previous_stage = NULL),
    "must be a data frame"
  )

  # test sorting behavior
  issues <- tibble::tibble(
    issue_id = paste0("i", 1:5),
    severity = c("low", "critical", "high", "medium", "high"),
    impact_rate = c(0.02, 0.40, 0.20, 0.08, 0.25),
    problem = paste("Problem", letters[1:5])
  )

  interpret <- bid_interpret(central_question = "Pipeline?")

  result <- bid_pipeline(issues, interpret, max = 3)

  # should prioritize: critical (b), then high by impact (e > c), etc
  expect_true(grepl("b", result[[1]]$problem)) # critical
  expect_true(grepl("e", result[[2]]$problem)) # high, 0.25 impact
  expect_true(grepl("c", result[[3]]$problem)) # high, 0.20 impact
})

test_that("bid_pipeline handles issues without impact_rate", {
  issues <- tibble::tibble(
    issue_id = c("i1", "i2"),
    severity = c("high", "low"),
    problem = c("High severity", "Low severity")
    # no impact_rate column
  )

  interpret <- bid_interpret(central_question = "Test?")

  # should still work, sorting only by severity
  expect_no_error(
    result <- bid_pipeline(issues, interpret, max = 2)
  )

  expect_equal(length(result), 2)
  expect_true(grepl("High", result[[1]]$problem))
})
