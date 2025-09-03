test_that("bid_report generates text report with expected content", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_notice(
          previous_stage = bid_interpret(
            central_question = "How to simplify?",
            data_story = list(
              hook = "Users are confused",
              context = "Dashboard has evolved over time"
            )
          ),
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),

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
  # Changed from "Recommended Next Steps" to "Next Steps"
  expect_match(report, "Next Steps")
})

test_that("bid_report generates HTML report with correct format", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_notice(
          previous_stage = bid_interpret(
            central_question = "How to simplify?",
            data_story = list(
              hook = "Users are confused",
              context = "Dashboard has evolved over time"
            )
          ),
          problem = "Complex interface",
          theory = "Cognitive Load Theory",
          evidence = "User complaints"
        ),

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

  # HTML structure - updated to match actual format
  expect_type(html_report, "character")
  expect_match(html_report, "<!DOCTYPE html>")
  expect_match(html_report, "<html lang=\"en\">") # Updated to match actual output
  expect_match(html_report, "<head>")
  expect_match(html_report, "<style>")
  expect_match(html_report, "<body>")
  expect_match(html_report, "</html>")

  # HTML content
  expect_match(html_report, "<h1>BID Framework Implementation Report</h1>")
  expect_match(html_report, "<h2>Stage 5: Validate & Empower the User</h2>")
  expect_match(html_report, "<strong>Summary Panel:</strong>")
  expect_match(html_report, "<strong>Collaboration Features:</strong>")
  # Updated to match actual output
  expect_match(html_report, "<h2>Next Steps</h2>")

  # List items - updated to match actual output
  expect_match(html_report, "<li>Implement key BID principles")
})

test_that("bid_report fails with incorrect input", {
  # Updated error message to match actual implementation
  expect_error(bid_report(NULL), "Invalid input: validate_stage cannot be NULL")
  expect_error(
    bid_report(list()),
    "Invalid input: validate_stage must be a tibble"
  )

  expect_error(
    bid_report(tibble(stage = "NotValidate")),
    "must be the result"
  )

  interpret_result <- bid_interpret(
    central_question = "How to test error handling?"
  )
  
  notice_result <- bid_notice(
    previous_stage = interpret_result,
    problem = "Complex interface",
    theory = "Cognitive Load Theory",
    evidence = "User complaints"
  )
  expect_error(bid_report(notice_result), "must be the result")
})

test_that("bid_report handles minimal validate result", {
  minimal_validate <- tibble(
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
        bid_notice(
          previous_stage = bid_interpret(
            central_question = "Test question"
          ),
          problem = "Test problem",
          evidence = "Test evidence"
        ),
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

test_that("bid_report handles different formats", {
  validate_result <- bid_validate(
    bid_anticipate(
      bid_structure(
        bid_notice(
          previous_stage = bid_interpret(
            central_question = "Test question"
          ),
          problem = "Test problem",
          evidence = "Test evidence"
        ),
      ),
      bias_mitigations = list(anchoring = "Test")
    ),
    summary_panel = "Test summary",
    collaboration = "Test collaboration"
  )

  # Test text format (default)
  text_report <- bid_report(validate_result, format = "text")
  expect_type(text_report, "character")
  expect_false(grepl("<html>", text_report))

  # Test markdown format
  markdown_report <- bid_report(validate_result, format = "markdown")
  expect_type(markdown_report, "character")
  expect_false(grepl("<html>", markdown_report))

  # Test HTML format
  html_report <- bid_report(validate_result, format = "html")
  expect_type(html_report, "character")
  expect_match(html_report, "<html")
})

test_that("bid_report handles missing fields gracefully", {
  minimal_validate <- tibble(
    stage = "Validate",
    summary_panel = "Test summary",
    collaboration = "Test collaboration",
    suggestions = "Test suggestions",
    timestamp = Sys.time()
  )

  # Should not error even with minimal data
  expect_no_error(report <- bid_report(minimal_validate))
  expect_type(report, "character")
  expect_match(report, "BID Framework Implementation Report")
})
