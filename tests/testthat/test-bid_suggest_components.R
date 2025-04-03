test_that("bid_suggest_components returns appropriate shiny suggestions", {
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  # Test shiny suggestions
  suggestions <- bid_suggest_components(notice_result, package = "shiny")
  
  # Check structure
  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(c("stage", "component", "description", "code_example") %in% names(suggestions)))
  
  # Check content
  expect_true(any(suggestions$stage == "Notice"))
  expect_true(any(grepl("radioButtons|select|input", suggestions$component)))
  expect_true(all(nchar(suggestions$code_example) > 0))
  
  # Check that we get a message
  expect_message(
    bid_suggest_components(notice_result, package = "shiny"),
    "Component suggestions provided"
  )
})

test_that("bid_suggest_components returns stage-appropriate suggestions", {
  # Test for Notice stage
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  notice_suggestions <- bid_suggest_components(notice_result, package = "bslib")
  expect_true(any(notice_suggestions$stage == "Notice"))
  
  # Test for Interpret stage
  interpret_result <- bid_interpret(
    notice_result,
    central_question = "How to simplify?",
    data_story = list(
      hook = "Users are confused",
      context = "Dashboard has evolved over time"
    )
  )
  interpret_suggestions <- bid_suggest_components(interpret_result, package = "bslib")
  expect_true(any(interpret_suggestions$stage == "Interpret"))
  
  # Test for Structure stage
  structure_result <- bid_structure(
    interpret_result,
    layout = "dual_process",
    concepts = c("Principle of Proximity", "Default Effect")
  )
  structure_suggestions <- bid_suggest_components(structure_result, package = "bslib")
  expect_true(any(structure_suggestions$stage == "Structure"))
  
  # Test for Anticipate stage
  anticipate_result <- bid_anticipate(
    structure_result,
    bias_mitigations = list(
      anchoring = "Provide reference points",
      framing = "Use consistent positive framing"
    )
  )
  anticipate_suggestions <- bid_suggest_components(anticipate_result, package = "bslib")
  expect_true(any(anticipate_suggestions$stage == "Anticipate"))
  
  # Test for Validate stage
  validate_result <- bid_validate(
    anticipate_result,
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )
  validate_suggestions <- bid_suggest_components(validate_result, package = "bslib")
  expect_true(any(validate_suggestions$stage == "Validate"))
})

test_that("bid_suggest_components returns appropriate suggestions for each package", {
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  # Test bslib suggestions
  bslib_suggestions <- bid_suggest_components(notice_result, package = "bslib")
  expect_true(any(grepl("layout_|card|navset", bslib_suggestions$component)))
  
  # Test reactable suggestions
  reactable_suggestions <- bid_suggest_components(notice_result, package = "reactable")
  expect_true(any(grepl("reactable|colDef|defaultCol", reactable_suggestions$component)))
  
  # Test echarts4r suggestions
  echarts_suggestions <- bid_suggest_components(notice_result, package = "echarts4r")
  expect_true(any(grepl("e_charts|e_|echarts", echarts_suggestions$component)))
})

test_that("bid_suggest_components works with unknown stage", {
  # Create tibble with no stage information
  unknown_stage <- tibble::tibble(
    problem = "Test problem",
    evidence = "Test evidence"
  )
  
  suggestions <- bid_suggest_components(unknown_stage, package = "shiny")
  
  # Should return all suggestions when stage is unknown
  expect_s3_class(suggestions, "tbl_df")
  expect_true(nrow(suggestions) > 0)
  expect_true(length(unique(suggestions$stage)) > 1)
})

test_that("bid_suggest_components validates package argument", {
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  # Should error with invalid package
  expect_error(
    bid_suggest_components(notice_result, package = "invalidpackage"),
    "should be one of"
  )
  
  # Should work with abbreviated argument matching
  expect_s3_class(
    bid_suggest_components(notice_result, package = "sh"),
    "tbl_df"
  )
})
