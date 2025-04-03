#' Generate BID Framework Report
#'
#' @description
#' Creates a comprehensive report from a completed BID framework process
#'
#' @param validate_stage A tibble output from \code{bid_validate()}.
#' @param format Output format: "text" or "html"

#' @return A formatted report summarizing the entire BID process

#' @examples
#' \dontrun{
#' # After completing all 5 stages
#' validation_result <- bid_validate(...)
#' bid_report(validation_result)
#' }

#' @export
bid_report <- function(validate_stage, format = "text") {
  # Extract all previous stages if available
  if (
    !(
      "stage" %in% names(validate_stage) &&
        validate_stage$stage[1] == "Validate"
    )
  ) {
    stop("Input must be the result of a completed bid_validate() call.")
  }

  # Build report content
  report <- character(0)

  # Header
  report <- c(report, "# BID Framework Implementation Report")
  report <- c(report, paste0("Generated: ", Sys.time()))
  report <- c(report, "")

  # Stage 5: Validate
  report <- c(report, "## Stage 5: Validate & Empower the User")

  report <- c(
    report,
    paste0("- **Summary Panel**: ", validate_stage$summary_panel[1])
  )

  report <- c(
    report,
    paste0("- **Collaboration Features**: ", validate_stage$collaboration[1])
  )

  report <- c(
    report,
    paste0("- **Suggestions**: ", validate_stage$suggestions[1])
  )

  report <- c(report, "")

  # Stage 4: Anticipate (if available)
  if ("previous_bias" %in% names(validate_stage)) {
    report <- c(report, "## Stage 4: Anticipate User Behavior")

    report <- c(
      report,
      paste0("- **Bias Mitigations**: ", validate_stage$previous_bias[1])
    )

    report <- c(report, "")
  }

  # Add recommended next steps
  report <- c(report, "## Recommended Next Steps")
  report <- c(report, "1. Implement key BID principles identified in this analysis")
  report <- c(report, "2. Conduct user testing to validate improvements")
  report <- c(report, "3. Iterate based on feedback")
  report <- c(report, "4. Document successful patterns for future projects")

  # Format the report based on the requested format
  if (format == "html") {
    html_report <- paste(
      "<html><head><style>",
      "body { font-family: Arial, sans-serif; line-height: 1.6; max-width: 800px; margin: 0 auto; padding: 20px; }",
      "h1, h2 { color: #1c6d7d; }",
      "ul, ol { margin-bottom: 20px; }",
      "</style></head><body>",
      paste(
        gsub(
          "^# (.*)", "<h1>\\1</h1>",
          gsub(
            "^## (.*)", "<h2>\\1</h2>",
            gsub(
              "^- \\*\\*(.*?)\\*\\*: (.*)", "<p><strong>\\1:</strong> \\2</p>",
              gsub(
                "^\\d+\\. (.*)", "<li>\\1</li>",
                report
              )
            )
          )
        ),
        collapse = "\n"
      ),
      "</body></html>",
      sep = "\n"
    )
    return(html_report)
  } else {
    return(paste(report, collapse = "\n"))
  }
}
