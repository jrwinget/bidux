#' Generate BID Framework Report
#'
#' @description
#' Creates a comprehensive report from a completed BID framework process. This
#' report summarizes all stages and provides recommendations for implementation.
#'
#' @param validate_stage A tibble output from \code{bid_validate()}.
#' @param format Output format: "text", "html", or "markdown"
#' @param include_diagrams Logical, whether to include ASCII diagrams in the
#'        report (default: TRUE)
#'
#' @return A formatted report summarizing the entire BID process
#'
#' @examples
#' \dontrun{
#' # After completing all 5 stages
#' validation_result <- bid_validate(...)
#' 
#' # Generate a text report
#' bid_report(validation_result)
#' 
#' # Generate an HTML report
#' bid_report(validation_result, format = "html")
#' 
#' # Generate a markdown report without diagrams
#' bid_report(validation_result, format = "markdown", include_diagrams = FALSE)
#' }
#'
#' @export
bid_report <- function(
    validate_stage,
    format = c("text", "html", "markdown"),
    include_diagrams = TRUE) {
  format <- match.arg(format)
  
  if (
    !tibble::is_tibble(validate_stage) || 
      !("stage" %in% names(validate_stage)) ||
      validate_stage$stage[1] != "Validate"
  ) {
    stop(
      standard_error_msg(
        "invalid_param",
        "validate_stage",
        "result from bid_validate()",
        "invalid input"
      )
    )
  }

  # build report
  report <- character(0)

  # header
  report <- c(report, "# BID Framework Implementation Report")
  report <- c(report, paste0("Generated: ", Sys.time()))
  report <- c(report, "")

  # BID overview (with diagram if requested)
  if (include_diagrams) {
    report <- c(report, "## BID Framework Overview")
    report <- c(report, "```")
    report <- c(report, "┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐")
    report <- c(report, "│    NOTICE   │  │  INTERPRET  │  │  STRUCTURE  │  │  ANTICIPATE │  │   VALIDATE  │")
    report <- c(report, "│    Stage 1  │->│    Stage 2  │->│    Stage 3  │->│    Stage 4  │->│    Stage 5  │")
    report <- c(report, "└─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘")
    report <- c(report, "```")
    report <- c(report, "")
  }
  
  # stage 5: validate
  report <- c(report, "## Stage 5: Validate & Empower the User")

  report <- c(
    report,
    paste0("- **Summary Panel**: ", validate_stage$summary_panel[1])
  )

  report <- c(
    report,
    paste0("- **Collaboration Features**: ", validate_stage$collaboration[1])
  )
  
  if (!is.na(validate_stage$next_steps[1])) {
    next_steps <- unlist(strsplit(validate_stage$next_steps[1], ";"))
    report <- c(report, "- **Next Steps**:")
    for (step in next_steps) {
      report <- c(report, paste0("  - ", trimws(step)))
    }
  }
  
  report <- c(
    report,
    paste0("- **Suggestions**: ", validate_stage$suggestions[1])
  )

  report <- c(report, "")
  
  # stage 4: anticipate
  if ("previous_bias" %in% names(validate_stage)) {
    report <- c(report, "## Stage 4: Anticipate User Behavior")
    report <- c(
      report,
      paste0("- **Bias Mitigations**: ", validate_stage$previous_bias[1])
    )
    
    if (!is.na(validate_stage$previous_interaction[1])) {
      report <- c(
        report,
        paste0(
          "- **Interaction Principles**: ",
          validate_stage$previous_interaction[1]
        )
      )
    }
    
    report <- c(report, "")
  }
  
  # stage 3: structure (try to reconstruct from available data)
  if ("previous_layout" %in% names(validate_stage)) {
    report <- c(report, "## Stage 3: Structure the Dashboard")
    report <- c(
      report,
      paste0("- **Layout**: ", validate_stage$previous_layout[1])
    )

    if ("previous_concepts" %in% names(validate_stage)) {
      report <- c(
        report,
        paste0(
          "- **Applied Concepts**: ",
          validate_stage$previous_concepts[1]
        )
      )
    }
    
    if (
      "previous_accessibility" %in% names(validate_stage) && 
        !is.na(validate_stage$previous_accessibility[1])
    ) {
      report <- c(
        report,
        paste0(
          "- **Accessibility Considerations**: ", 
          validate_stage$previous_accessibility[1]
        )
      )
    }

    report <- c(report, "")
  }
  
  # recommendations
  report <- c(report, "## Implementation Recommendations")
  report <- c(
    report,
    paste(
      "Based on your application of the BID framework, consider these",
      "implementation tips:"
    )
  )
  report <- c(report, "")
  
  # UI suggestions
  report <- c(report, "### Recommended UI Components")
  
  # {bslib} suggestions
  if (requireNamespace("dplyr", quietly = TRUE)) {
    tryCatch({
      bslib_suggestions <- bid_suggest_components(
        validate_stage,
        package = "bslib"
      )

      top_bslib <- head(bslib_suggestions, 3)

      if (nrow(top_bslib) > 0) {
        report <- c(report, "**bslib Components:**")
        for (i in 1:nrow(top_bslib)) {
          report <- c(
            report,
            paste0(
              "- ", top_bslib$component[i], ": ", 
              top_bslib$description[i]
            )
          )
        }
        report <- c(report, "")
      }
    }, error = function(e) {
      # silently fail
    })
    
    # {shiny} suggestions
    tryCatch({
      shiny_suggestions <- bid_suggest_components(
        validate_stage,
        package = "shiny"
      )

      top_shiny <- head(shiny_suggestions, 3)

      if (nrow(top_shiny) > 0) {
        report <- c(report, "**Shiny Components:**")
        for (i in 1:nrow(top_shiny)) {
          report <- c(
            report,
            paste0(
              "- ", top_shiny$component[i], ": ", 
              top_shiny$description[i]
            )
          )
        }
        report <- c(report, "")
      }
    }, error = function(e) {
      # silently fail
    })
  }
  
  # next steps suggestions
  report <- c(report, "## Next Steps")
  report <- c(report, "1. Implement key BID principles identified in this analysis")
  report <- c(report, "2. Conduct user testing to validate improvements")
  report <- c(report, "3. Iterate based on feedback")
  report <- c(report, "4. Document successful patterns for future projects")
  report <- c(report, "")
  
  # learning resources
  report <- c(report, "## Learning Resources")
  report <- c(report, "To learn more about the BID framework concepts used in this report:")
  report <- c(report, "- Review the {bidux} vignettes with `vignette('introduction-to-bid')`")
  report <- c(report, "- Explore the concept dictionary with `bid_concepts()`")
  report <- c(report, "- Check the package documentation at https://github.com/jrwinget/bidux")
  report <- c(report, "")
  
  # format report
  if (format == "html") {
    html_report <- paste(
      "<html><head><style>",
      "body { font-family: Arial, sans-serif; line-height: 1.6; max-width: 800px; margin: 0 auto; padding: 20px; }",
      "h1, h2, h3 { color: #1c6d7d; }",
      "ul, ol { margin-bottom: 20px; }",
      "pre { background-color: #f5f5f5; padding: 10px; border-radius: 4px; overflow-x: auto; }",
      "code { font-family: monospace; }",
      ".diagram { font-family: monospace; white-space: pre; margin: 20px 0; }",
      "</style></head><body>",
      paste(
        gsub(
          "^# (.*)", "<h1>\\1</h1>",
          gsub(
            "^## (.*)", "<h2>\\1</h2>",
            gsub(
              "^### (.*)", "<h3>\\1</h3>",
              gsub(
                "^- \\*\\*(.*?)\\*\\*: (.*)", "<p><strong>\\1:</strong> \\2</p>",
                gsub(
                  "^\\d+\\. (.*)", "<li>\\1</li>",
                  gsub(
                    "^```(.*)```$", "<pre class=\"diagram\">\\1</pre>",
                    report,
                    perl = TRUE
                  )
                )
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
  } else if (format == "markdown") {
    return(paste(report, collapse = "\n"))
  } else {
    text_report <- gsub("^#+ ", "", report)
    text_report <- gsub("\\*\\*", "", text_report)
    return(paste(text_report, collapse = "\n"))
  }
}
