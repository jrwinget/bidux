test_that("bid_report generates text report", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_interpret(
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
        ),
        layout = "dual_process",
        concepts = c("Principle of Proximity", "Default Effect")
      ),
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use consistent positive framing"
      )
    ),
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )
  
  report <- bid_report(validate_result)
  
  expect_type(report, "character")
  expect_match(report, "BID Framework Implementation Report")
  expect_match(report, "Stage 5: Validate")
  expect_match(report, "Summary Panel")
  expect_match(report, "Recommended Next Steps")
})

test_that("bid_report generates HTML report", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_interpret(
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
        ),
        layout = "dual_process",
        concepts = c("Principle of Proximity", "Default Effect")
      ),
      bias_mitigations = list(
        anchoring = "Provide reference points",
        framing = "Use consistent positive framing"
      )
    ),
    summary_panel = "Dashboard simplified for quicker insights",
    collaboration = "Added team annotation features"
  )
  
  html_report <- bid_report(validate_result, format = "html")
  
  expect_type(html_report, "character")
  expect_match(html_report, "<html>")
  expect_match(html_report, "<h1>BID Framework Implementation Report</h1>")
  expect_match(html_report, "<h2>Stage 5: Validate")
  expect_match(html_report, "</html>")
})

test_that("bid_report fails with incorrect input", {
  expect_error(bid_report(NULL), "must be the result")
  expect_error(bid_report(list()), "must be the result")
  expect_error(bid_report(tibble::tibble(stage = "NotValidate")), "must be the result")
})

test_that("bid_suggest_components returns appropriate suggestions", {
  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  
  # Test shiny suggestions
  shiny_suggestions <- bid_suggest_components(notice_result, package = "shiny")
  expect_s3_class(shiny_suggestions, "tbl_df")
  expect_true("component" %in% names(shiny_suggestions))
  expect_true("description" %in% names(shiny_suggestions))
  expect_true("code_example" %in% names(shiny_suggestions))
  expect_true(any(shiny_suggestions$stage == "Notice"))
  
  # Test bslib suggestions
  bslib_suggestions <- bid_suggest_components(notice_result, package = "bslib")
  expect_true(any(bslib_suggestions$stage == "Notice"))
  
  # Test filtering by stage
  structure_result <- bid_structure(
    bid_interpret(
      notice_result,
      central_question = "How to simplify?",
      data_story = list(
        hook = "Users are confused",
        context = "Dashboard has evolved over time"
      )
    ),
    layout = "dual_process",
    concepts = c("Principle of Proximity", "Default Effect")
  )
  
  structure_suggestions <- bid_suggest_components(structure_result, package = "reactable")
  expect_true(any(structure_suggestions$stage == "Structure"))
})