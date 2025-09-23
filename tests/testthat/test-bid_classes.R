test_that("bid_stage constructor creates valid objects", {
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )

  result <- bid_stage("Notice", test_data)
  validate_bid_stage_structure(result, "Notice")
})

test_that("bid_stage validation works correctly", {
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )

  # valid stage should work
  expect_silent(bid_stage("Notice", test_data))

  # invalid stage should error
  expect_error(
    bid_stage("InvalidStage", test_data),
    "Stage must be one of:"
  )

  # mismatched stage attribute and column should error
  wrong_data <- tibble(
    stage = "Interpret",
    problem = "Test problem",
    timestamp = Sys.time()
  )
  expect_error(
    bid_stage("Notice", wrong_data),
    "Stage attribute must match stage column value"
  )

  # missing timestamp should error
  no_timestamp <- tibble(
    stage = "Notice",
    problem = "Test problem"
  )
  expect_error(
    bid_stage("Notice", no_timestamp),
    "BID stage object must contain a 'timestamp' column"
  )
})

test_that("is_bid_stage works correctly", {
  stage_obj <- bid_stage("Notice", tibble(
    stage = "Notice", problem = "Test", timestamp = Sys.time()
  ))

  expect_true(is_bid_stage(stage_obj))
  expect_false(is_bid_stage(tibble(a = 1)))
  expect_false(is_bid_stage(NULL))
  expect_false(is_bid_stage("string"))
})

test_that("get_stage and get_metadata work correctly", {
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )

  metadata <- list(auto_suggested = TRUE, confidence = 0.8)
  stage_obj <- bid_stage("Notice", test_data, metadata)

  expect_equal(get_stage(stage_obj), "Notice")
  expect_equal(get_metadata(stage_obj), metadata)

  # should error for non-bid_stage objects
  expect_error(get_stage(test_data), "Object is not a bid_stage")
  expect_error(get_metadata(test_data), "Object is not a bid_stage")
})

test_that("print.bid_stage displays core information", {
  # test key stages with essential output
  stages_data <- list(
    Notice = list(problem = "Test problem", theory = "Test theory"),
    Interpret = list(central_question = "Test question", hook = "Test hook"),
    Structure = list(layout = "breathable", concepts = "Visual Hierarchy"),
    Anticipate = list(bias_mitigations = "anchoring: test"),
    Validate = list(summary_panel = "Clear summary")
  )

  for (stage_name in names(stages_data)) {
    stage_data <- stages_data[[stage_name]]
    stage_data$stage <- stage_name
    stage_data$suggestions <- "Test suggestions"
    stage_data$timestamp <- Sys.time()

    stage_obj <- bid_stage(stage_name, tibble::tibble(!!!stage_data))

    expect_output(print(stage_obj), "BID Framework")
    expect_output(print(stage_obj), paste(stage_name, "Stage"))
  }
})

test_that("summary.bid_stage works correctly", {
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    theory = "Test theory",
    evidence = "Test evidence",
    suggestions = "Test suggestions",
    timestamp = Sys.time()
  )

  metadata <- list(
    auto_suggested_theory = TRUE,
    theory_confidence = 0.8,
    stage_number = 1
  )

  stage_obj <- bid_stage("Notice", test_data, metadata)

  expect_output(summary(stage_obj), "BID Framework:")
  expect_output(summary(stage_obj), "Notice Stage Summary")
  expect_output(summary(stage_obj), "Metadata:")
  expect_output(summary(stage_obj), "Stage Data:")
  expect_output(summary(stage_obj), "auto_suggested_theory")
})

test_that("as_tibble.bid_stage works correctly", {
  test_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )

  stage_obj <- bid_stage("Notice", test_data)
  tibble_result <- as_tibble(stage_obj)

  expect_s3_class(tibble_result, "tbl_df")
  expect_false(inherits(tibble_result, "bid_stage"))
  expect_equal(names(tibble_result), names(test_data))
  expect_equal(nrow(tibble_result), 1)
})

test_that("bid_result constructor and validation work", {
  # create test stages
  stage1_data <- tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )
  stage1 <- bid_stage("Interpret", stage1_data)

  stage2_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )
  stage2 <- bid_stage("Notice", stage2_data)

  # create bid_result
  result <- bid_result(list(stage1, stage2))

  expect_s3_class(result, "bid_result")
  expect_s3_class(result, "list")
  expect_length(result, 2)

  # should error with non-bid_stage objects
  expect_error(
    bid_result(list(stage1_data, stage2_data)),
    "All elements in BID result must be bid_stage objects"
  )
})

test_that("extract_stage works correctly", {
  stage1_data <- tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )
  stage1 <- bid_stage("Interpret", stage1_data)

  stage2_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )
  stage2 <- bid_stage("Notice", stage2_data)

  workflow <- bid_result(list(stage1, stage2))

  extracted_notice <- extract_stage(workflow, "Notice")
  expect_s3_class(extracted_notice, "bid_stage")
  expect_equal(get_stage(extracted_notice), "Notice")

  extracted_missing <- extract_stage(workflow, "Structure")
  expect_null(extracted_missing)

  # should error with non-bid_result
  expect_error(
    extract_stage(list(stage1, stage2), "Notice"),
    "workflow must be a bid_result object"
  )
})

test_that("is_complete works correctly", {
  # create incomplete workflow
  stage1_data <- tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )
  stage1 <- bid_stage("Interpret", stage1_data)

  incomplete_workflow <- bid_result(list(stage1))
  expect_false(is_complete(incomplete_workflow))

  # create complete workflow
  all_stages <- list()
  stage_names <- c("Interpret", "Notice", "Anticipate", "Structure", "Validate")

  for (stage_name in stage_names) {
    stage_data <- tibble(
      stage = stage_name,
      test_field = paste("Test", stage_name),
      timestamp = Sys.time()
    )
    all_stages[[stage_name]] <- bid_stage(stage_name, stage_data)
  }

  complete_workflow <- bid_result(all_stages)
  expect_true(is_complete(complete_workflow))

  # should return FALSE for non-bid_result
  expect_false(is_complete(list(stage1)))
})

test_that("print.bid_result works correctly", {
  stage1_data <- tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )
  stage1 <- bid_stage("Interpret", stage1_data)

  stage2_data <- tibble(
    stage = "Notice",
    problem = "Test problem",
    timestamp = Sys.time()
  )
  stage2 <- bid_stage("Notice", stage2_data)

  workflow <- bid_result(list(stage1, stage2))

  expect_output(print(workflow), "BID Framework Workflow")
  expect_output(print(workflow), "Stages completed: 2 of 5")
  expect_output(print(workflow), "Progress: 40%")
  expect_output(print(workflow), "Notice")
  expect_output(print(workflow), "Interpret")
})

test_that("summary.bid_result works correctly", {
  stage1_data <- tibble(
    stage = "Interpret",
    central_question = "Test question",
    timestamp = Sys.time()
  )
  stage1 <- bid_stage("Interpret", stage1_data)

  workflow <- bid_result(list(stage1))

  expect_output(summary(workflow), "BID Framework Workflow Summary")
  expect_output(summary(workflow), "Total stages: 1")
  expect_output(summary(workflow), "Complete workflow: No")
  expect_output(summary(workflow), "1. Interpret")
})
