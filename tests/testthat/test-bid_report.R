test_that("bid_report generates text report with expected content", {
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
  expect_true(grepl("\n", report)) # should have line breaks

  expect_match(report, "BID Framework Implementation Report")
  expect_match(report, "Stage 5: Validate & Empower the User")
  expect_match(report, "Dashboard simplified for quicker insights")
  expect_match(report, "Added team annotation features")
  expect_match(report, "Stage 4: Anticipate User Behavior")
  expect_match(report, "anchoring: Provide reference points")
  expect_match(report, "Recommended Next Steps")
})

test_that("bid_report generates HTML report with correct format", {
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

  # HTML structure
  expect_type(html_report, "character")
  expect_match(html_report, "<html>")
  expect_match(html_report, "<head>")
  expect_match(html_report, "<style>")
  expect_match(html_report, "<body>")
  expect_match(html_report, "</html>")

  # HTML content
  expect_match(html_report, "<h1>BID Framework Implementation Report</h1>")
  expect_match(html_report, "<h2>Stage 5: Validate & Empower the User</h2>")
  expect_match(html_report, "<strong>Summary Panel:</strong>")
  expect_match(html_report, "<strong>Collaboration Features:</strong>")
  expect_match(html_report, "<h2>Recommended Next Steps</h2>")

  # list items
  expect_match(html_report, "<li>Implement key BID principles")
})

test_that("bid_report fails with incorrect input", {
  expect_error(bid_report(NULL), "must be the result")
  expect_error(bid_report(list()), "must be the result")

  expect_error(
    bid_report(tibble::tibble(stage = "NotValidate")),
    "must be the result"
  )

  notice_result <- bid_notice(
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  expect_error(bid_report(notice_result), "must be the result")
})

test_that("bid_report handles minimal validate result", {
  minimal_validate <- tibble::tibble(
    stage = "Validate",
    summary_panel = "Simple summary",
    collaboration = "Simple collaboration",
    previous_bias = NA_character_,
    timestamp = Sys.time()
  )

  report <- bid_report(minimal_validate)

  expect_type(report, "character")
  expect_match(report, "BID Framework Implementation Report")
  expect_match(report, "Stage 5: Validate & Empower the User")
  expect_match(report, "Simple summary")
  expect_match(report, "Simple collaboration")
})

test_that("bid_report includes diagrams when requested", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_interpret(
          bid_notice(
            problem = "Test problem",
            evidence = "Test evidence"
          ),
          central_question = "Test question"
        ),
        layout = "dual_process"
      ),
      bias_mitigations = list(anchoring = "Test")
    ),
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  report_with_diagrams <- bid_report(
    validate_result,
    format = "markdown",
    include_diagrams = TRUE
  )

  report_without_diagrams <- bid_report(
    validate_result,
    format = "markdown",
    include_diagrams = FALSE
  )

  expect_match(report_with_diagrams, "```", fixed = TRUE)
  expect_match(report_with_diagrams, "BID Framework Overview", fixed = TRUE)

  expect_true(nchar(report_without_diagrams) < nchar(report_with_diagrams))
  expect_false(grepl("┌─────────────┐", report_without_diagrams, fixed = TRUE))
})
