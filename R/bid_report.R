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
  include_diagrams = TRUE
) {
  format <- match.arg(format)

  tryCatch(
    {
      if (is.null(validate_stage)) {
        cli::cli_abort(c(
          "x" = "Invalid input: validate_stage cannot be NULL",
          "i" = "Please provide a valid output from bid_validate()"
        ))
      }

      if (!tibble::is_tibble(validate_stage)) {
        cli::cli_abort(c(
          "x" = "Invalid input: validate_stage must be a tibble",
          "i" = "Did you provide the output from a BID framework function?",
          "i" = "Current type: {.cls {class(validate_stage)[1]}}"
        ))
      }

      if (!("stage" %in% names(validate_stage))) {
        cli::cli_abort(c(
          "x" = "Invalid input: validate_stage must contain a 'stage' column",
          "i" = "Available columns: {.field {paste(names(validate_stage), collapse = ', ')}}"
        ))
      }

      if (validate_stage$stage[1] != "Validate") {
        cli::cli_abort(c(
          "x" = "Invalid input: validate_stage must be the result from bid_validate()",
          "i" = "Current stage: {.val {validate_stage$stage[1]}}",
          "i" = "Use the output from bid_validate() as input to this function"
        ))
      }
    },
    error = function(e) {
      stop(e$message, call. = FALSE)
    }
  )

  if (format == "html") {
    return(generate_html_report_direct(validate_stage, include_diagrams))
  } else {
    return(generate_text_report(validate_stage, format, include_diagrams))
  }
}

generate_text_report <- function(validate_stage, format, include_diagrams) {
  report <- character(0)

  # header
  if (format == "markdown") {
    report <- c(report, "# BID Framework Implementation Report")
  } else {
    report <- c(report, "BID Framework Implementation Report")
  }
  report <- c(report, paste0("Generated: ", Sys.time()))
  report <- c(report, "")

  # BID overview diagram
  if (include_diagrams) {
    if (format == "markdown") {
      report <- c(report, "## BID Framework Overview")
      report <- c(report, "```")
    } else {
      report <- c(report, "BID Framework Overview")
    }
    report <- c(
      report,
      "+-------------+  +-------------+  +-------------+  +-------------+  +-------------+"
    )
    report <- c(
      report,
      "|    NOTICE   |  |  INTERPRET  |  |  STRUCTURE  |  |  ANTICIPATE |  |   VALIDATE  |"
    )
    report <- c(
      report,
      "|    Stage 1  |->|    Stage 2  |->|    Stage 3  |->|    Stage 4  |->|    Stage 5  |"
    )
    report <- c(
      report,
      "+-------------+  +-------------+  +-------------+  +-------------+  +-------------+"
    )
    if (format == "markdown") {
      report <- c(report, "```")
    }
    report <- c(report, "")
  }

  safe_extract <- function(data, field, default = "Not specified") {
    if (field %in% names(data) && length(data[[field]]) > 0) {
      value <- data[[field]][1]
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(trimws(as.character(value)))
      }
    }
    return(default)
  }

  # stage 5: validate
  stage_header <- if (format == "markdown") {
    "## Stage 5: Validate & Empower the User"
  } else {
    "Stage 5: Validate & Empower the User"
  }
  report <- c(report, stage_header)

  # summary panel
  summary_panel <- safe_extract(validate_stage, "summary_panel")
  prefix <- if (format == "markdown") {
    "- **Summary Panel**: "
  } else {
    "- Summary Panel: "
  }
  report <- c(report, paste0(prefix, summary_panel))

  # collaboration features
  collaboration <- safe_extract(validate_stage, "collaboration")
  prefix <- if (format == "markdown") {
    "- **Collaboration Features**: "
  } else {
    "
    - Collaboration Features: "
  }
  report <- c(report, paste0(prefix, collaboration))

  # next steps
  if (
    "next_steps" %in%
      names(validate_stage) &&
      !is.na(validate_stage$next_steps[1])
  ) {
    next_steps <- tryCatch(
      {
        if (
          is.character(validate_stage$next_steps[1]) &&
            grepl(";", validate_stage$next_steps[1])
        ) {
          unlist(strsplit(validate_stage$next_steps[1], ";"))
        } else {
          validate_stage$next_steps[1]
        }
      },
      error = function(e) {
        "Error parsing next steps"
      }
    )

    prefix <- if (format == "markdown") {
      "- **Next Steps**:"
    } else {
      "- Next Steps:"
    }
    report <- c(report, prefix)

    if (length(next_steps) > 1) {
      for (step in next_steps) {
        step_text <- trimws(step)
        if (nchar(step_text) > 0) {
          report <- c(report, paste0("  - ", step_text))
        }
      }
    } else {
      report <- c(report, paste0("  - ", trimws(next_steps)))
    }
  } else {
    prefix <- if (format == "markdown") {
      "- **Next Steps**: "
    } else {
      "- Next Steps: "
    }
    report <- c(report, paste0(prefix, "Not specified"))
  }

  # Suggestions
  suggestions <- safe_extract(validate_stage, "suggestions")
  prefix <- if (format == "markdown") "- **Suggestions**: " else
    "- Suggestions: "
  report <- c(report, paste0(prefix, suggestions))
  report <- c(report, "")

  # Stage 4: Anticipate
  if ("previous_bias" %in% names(validate_stage)) {
    stage_header <- if (format == "markdown")
      "## Stage 4: Anticipate User Behavior" else
      "Stage 4: Anticipate User Behavior"
    report <- c(report, stage_header)

    bias_mitigations <- safe_extract(validate_stage, "previous_bias")
    prefix <- if (format == "markdown") "- **Bias Mitigations**: " else
      "- Bias Mitigations: "
    report <- c(report, paste0(prefix, bias_mitigations))

    interaction_principles <- safe_extract(
      validate_stage,
      "previous_interaction"
    )
    prefix <- if (format == "markdown") "- **Interaction Principles**: " else
      "- Interaction Principles: "
    report <- c(report, paste0(prefix, interaction_principles))
    report <- c(report, "")
  }

  # Stage 3: Structure
  if ("previous_layout" %in% names(validate_stage)) {
    stage_header <- if (format == "markdown")
      "## Stage 3: Structure the Dashboard" else
      "Stage 3: Structure the Dashboard"
    report <- c(report, stage_header)

    layout <- safe_extract(validate_stage, "previous_layout")
    prefix <- if (format == "markdown") "- **Layout**: " else "- Layout: "
    report <- c(report, paste0(prefix, layout))

    concepts <- safe_extract(validate_stage, "previous_concepts")
    prefix <- if (format == "markdown") "- **Applied Concepts**: " else
      "- Applied Concepts: "
    report <- c(report, paste0(prefix, concepts))

    accessibility <- safe_extract(validate_stage, "previous_accessibility")
    prefix <- if (format == "markdown")
      "- **Accessibility Considerations**: " else
      "- Accessibility Considerations: "
    report <- c(report, paste0(prefix, accessibility))
    report <- c(report, "")
  }

  # Implementation Recommendations
  impl_header <- if (format == "markdown")
    "## Implementation Recommendations" else "Implementation Recommendations"
  report <- c(report, impl_header)
  report <- c(
    report,
    "Based on your application of the BID framework, consider these implementation tips:"
  )
  report <- c(report, "")

  # UI Components
  ui_header <- if (format == "markdown") "### Recommended UI Components" else
    "Recommended UI Components"
  report <- c(report, ui_header)

  # Try to get component suggestions
  tryCatch(
    {
      bslib_suggestions <- bid_suggest_components(
        validate_stage,
        package = "bslib"
      )
      if (nrow(bslib_suggestions) > 0) {
        prefix <- if (format == "markdown") "**bslib Components:**" else
          "bslib Components:"
        report <- c(report, prefix)
        for (i in 1:min(3, nrow(bslib_suggestions))) {
          report <- c(
            report,
            paste0(
              "- ",
              bslib_suggestions$component[i],
              ": ",
              bslib_suggestions$description[i]
            )
          )
        }
        report <- c(report, "")
      }
    },
    error = function(e) {
      report <- c(report, "bslib Components: Error generating suggestions")
      report <- c(report, "")
    }
  )

  tryCatch(
    {
      shiny_suggestions <- bid_suggest_components(
        validate_stage,
        package = "shiny"
      )
      if (nrow(shiny_suggestions) > 0) {
        prefix <- if (format == "markdown") "**Shiny Components:**" else
          "Shiny Components:"
        report <- c(report, prefix)
        for (i in 1:min(3, nrow(shiny_suggestions))) {
          report <- c(
            report,
            paste0(
              "- ",
              shiny_suggestions$component[i],
              ": ",
              shiny_suggestions$description[i]
            )
          )
        }
        report <- c(report, "")
      }
    },
    error = function(e) {
      report <- c(report, "Shiny Components: Error generating suggestions")
      report <- c(report, "")
    }
  )

  # Next Steps
  next_header <- if (format == "markdown") "## Next Steps" else "Next Steps"
  report <- c(report, next_header)
  report <- c(
    report,
    "1. Implement key BID principles identified in this analysis"
  )
  report <- c(report, "2. Conduct user testing to validate improvements")
  report <- c(report, "3. Iterate based on feedback")
  report <- c(report, "4. Document successful patterns for future projects")
  report <- c(report, "")

  # Learning Resources
  learn_header <- if (format == "markdown") "## Learning Resources" else
    "Learning Resources"
  report <- c(report, learn_header)
  report <- c(
    report,
    "To learn more about the BID framework concepts used in this report:"
  )
  report <- c(
    report,
    "- Review the {bidux} vignettes with `vignette('introduction-to-bid')`"
  )
  report <- c(report, "- Explore the concept dictionary with `bid_concepts()`")
  report <- c(
    report,
    "- Check the package documentation at https://github.com/jrwinget/bidux"
  )

  # Format output
  if (format == "text") {
    # Remove markdown formatting for plain text
    text_report <- gsub("^#+\\s*", "", report)
    text_report <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text_report)
    return(paste(text_report, collapse = "\n"))
  } else {
    return(paste(report, collapse = "\n"))
  }
}

# Generate HTML report directly (no markdown conversion)
generate_html_report_direct <- function(validate_stage, include_diagrams) {
  # Helper function to safely extract data
  safe_extract <- function(data, field, default = "Not specified") {
    if (field %in% names(data) && length(data[[field]]) > 0) {
      value <- data[[field]][1]
      if (
        !is.null(value) &&
          !is.na(value) &&
          nchar(trimws(as.character(value))) > 0
      ) {
        return(trimws(as.character(value)))
      }
    }
    return(default)
  }

  # Helper function to generate HTML lists
  generate_html_list <- function(items, default_text = "None specified") {
    if (is.null(items) || length(items) == 0) {
      return(paste0("<ul><li>", default_text, "</li></ul>"))
    }

    if (is.character(items) && length(items) > 0) {
      # Handle semicolon-separated strings
      if (length(items) == 1 && grepl(";", items)) {
        items <- unlist(strsplit(items, ";"))
      }
      # Filter out empty strings
      items <- trimws(items)
      items <- items[items != "" & !is.na(items)]
      if (length(items) == 0) {
        return(paste0("<ul><li>", default_text, "</li></ul>"))
      }
      list_items <- paste0("<li>", items, "</li>", collapse = "\n    ")
    } else {
      return(paste0("<ul><li>", default_text, "</li></ul>"))
    }

    return(paste0("<ul>\n    ", list_items, "\n  </ul>"))
  }

  # CSS
  css <- '
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    line-height: 1.6;
    max-width: 900px;
    margin: 0 auto;
    padding: 2rem;
    color: #333;
    background-color: #fff;
  }

  h1, h2, h3, h4, h5, h6 {
    margin-top: 1.5em;
    margin-bottom: 0.75em;
    color: #2c3e50;
    font-weight: 600;
  }

  h1 {
    font-size: 2.2em;
    border-bottom: 2px solid #eaecef;
    padding-bottom: 0.3em;
  }

  h2 {
    font-size: 1.8em;
    border-bottom: 1px solid #eaecef;
    padding-bottom: 0.3em;
  }

  h3 {
    font-size: 1.5em;
  }

  p {
    margin-bottom: 1em;
  }

  ul, ol {
    margin-bottom: 1.5em;
    padding-left: 2em;
  }

  li {
    margin-bottom: 0.5em;
  }

  pre {
    background-color: #f6f8fa;
    border-radius: 6px;
    padding: 1em;
    overflow: auto;
    font-family: SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
    font-size: 0.9em;
    line-height: 1.45;
  }

  code {
    font-family: SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
    background-color: rgba(27, 31, 35, 0.05);
    border-radius: 3px;
    padding: 0.2em 0.4em;
    font-size: 0.9em;
  }

  .diagram {
    font-family: SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
    white-space: pre;
    background-color: #f6f8fa;
    border-radius: 6px;
    padding: 1em;
    margin: 1.5em 0;
    overflow-x: auto;
  }

  strong {
    font-weight: 600;
    color: #24292e;
  }

  .section {
    margin: 2em 0;
    padding: 1em;
    border-radius: 6px;
    background-color: #f8f9fa;
    border-left: 4px solid #007bff;
  }

  .container {
    display: flex;
    flex-wrap: wrap;
    gap: 1.5em;
  }

  .card {
    flex: 1 1 300px;
    border: 1px solid #e1e4e8;
    border-radius: 6px;
    padding: 1em;
    background-color: #fff;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
  }

  .card h4 {
    margin-top: 0;
    border-bottom: 1px solid #eaecef;
    padding-bottom: 0.5em;
  }

  @media (max-width: 768px) {
    body {
      padding: 1rem;
    }

    h1 {
      font-size: 1.8em;
    }

    h2 {
      font-size: 1.5em;
    }

    h3 {
      font-size: 1.3em;
    }
  }
  '

  # Build HTML content directly
  html_body <- paste0(
    "<h1>BID Framework Implementation Report</h1>",
    "<p>Generated: ",
    Sys.time(),
    "</p>"
  )

  # BID Framework Overview with diagram
  if (include_diagrams) {
    html_body <- paste0(
      html_body,
      '
<div class="section"><h2>BID Framework Overview</h2>
<pre class="diagram">
+-------------+  +-------------+  +-------------+  +-------------+  +-------------+
|    NOTICE   |  |  INTERPRET  |  |  STRUCTURE  |  |  ANTICIPATE |  |   VALIDATE  |
|    Stage 1  |->|    Stage 2  |->|    Stage 3  |->|    Stage 4  |->|    Stage 5  |
+-------------+  +-------------+  +-------------+  +-------------+  +-------------+
</pre>
</div>'
    )
  }

  # Stage 5: Validate
  html_body <- paste0(
    html_body,
    '
<div class="section"><h2>Stage 5: Validate & Empower the User</h2>
  <p><strong>Summary Panel:</strong> ',
    safe_extract(validate_stage, "summary_panel"),
    "</p>
  <p><strong>Collaboration Features:</strong> ",
    safe_extract(validate_stage, "collaboration"),
    "</p>"
  )

  # Next Steps
  if (
    "next_steps" %in%
      names(validate_stage) &&
      !is.na(validate_stage$next_steps[1])
  ) {
    next_steps <- tryCatch(
      {
        steps <- validate_stage$next_steps[1]
        if (is.character(steps) && grepl(";", steps)) {
          unlist(strsplit(steps, ";"))
        } else {
          steps
        }
      },
      error = function(e) {
        "Error parsing next steps"
      }
    )

    html_body <- paste0(
      html_body,
      "
  <h3>Next Steps</h3>
  ",
      generate_html_list(next_steps)
    )
  } else {
    html_body <- paste0(
      html_body,
      "
  <p><strong>Next Steps:</strong> Not specified</p>"
    )
  }

  html_body <- paste0(
    html_body,
    "
  <p><strong>Suggestions:</strong> ",
    safe_extract(validate_stage, "suggestions"),
    "</p>
</div>"
  )

  # Stage 4: Anticipate
  if ("previous_bias" %in% names(validate_stage)) {
    html_body <- paste0(
      html_body,
      '
<div class="section"><h2>Stage 4: Anticipate User Behavior</h2>
  <p><strong>Bias Mitigations:</strong> ',
      safe_extract(validate_stage, "previous_bias"),
      "</p>
  <p><strong>Interaction Principles:</strong> ",
      safe_extract(validate_stage, "previous_interaction"),
      "</p>
</div>"
    )
  }

  # Stage 3: Structure
  if ("previous_layout" %in% names(validate_stage)) {
    html_body <- paste0(
      html_body,
      '
<div class="section"><h2>Stage 3: Structure the Dashboard</h2>
  <p><strong>Layout:</strong> ',
      safe_extract(validate_stage, "previous_layout"),
      "</p>
  <p><strong>Applied Concepts:</strong> ",
      safe_extract(validate_stage, "previous_concepts"),
      "</p>
  <p><strong>Accessibility Considerations:</strong> ",
      safe_extract(validate_stage, "previous_accessibility"),
      "</p>
</div>"
    )
  }

  # Implementation Recommendations
  html_body <- paste0(
    html_body,
    '
<div class="section"><h2>Implementation Recommendations</h2>
<p>Based on your application of the BID framework, consider these implementation tips:</p>

<h3>Recommended UI Components</h3>'
  )

  # bslib Components
  tryCatch(
    {
      bslib_suggestions <- bid_suggest_components(
        validate_stage,
        package = "bslib"
      )
      if (nrow(bslib_suggestions) > 0) {
        html_body <- paste0(
          html_body,
          "
<p><strong>bslib Components:</strong></p>"
        )
        bslib_items <- paste0(
          bslib_suggestions$component[1:min(3, nrow(bslib_suggestions))],
          ": ",
          bslib_suggestions$description[1:min(3, nrow(bslib_suggestions))]
        )
        html_body <- paste0(html_body, generate_html_list(bslib_items))
      }
    },
    error = function(e) {
      html_body <- paste0(
        html_body,
        "
<p><strong>bslib Components:</strong></p>
<ul><li>Error generating suggestions</li></ul>"
      )
    }
  )

  # Shiny Components
  tryCatch(
    {
      shiny_suggestions <- bid_suggest_components(
        validate_stage,
        package = "shiny"
      )
      if (nrow(shiny_suggestions) > 0) {
        html_body <- paste0(
          html_body,
          "
<p><strong>Shiny Components:</strong></p>"
        )
        shiny_items <- paste0(
          shiny_suggestions$component[1:min(3, nrow(shiny_suggestions))],
          ": ",
          shiny_suggestions$description[1:min(3, nrow(shiny_suggestions))]
        )
        html_body <- paste0(html_body, generate_html_list(shiny_items))
      }
    },
    error = function(e) {
      html_body <- paste0(
        html_body,
        "
<p><strong>Shiny Components:</strong></p>
<ul><li>Error generating suggestions</li></ul>"
      )
    }
  )

  html_body <- paste0(
    html_body,
    "
</div>"
  )

  # Next Steps
  html_body <- paste0(
    html_body,
    '
<div class="section"><h2>Next Steps</h2>'
  )

  next_steps_items <- c(
    "Implement key BID principles identified in this analysis",
    "Conduct user testing to validate improvements",
    "Iterate based on feedback",
    "Document successful patterns for future projects"
  )
  html_body <- paste0(html_body, generate_html_list(next_steps_items))

  html_body <- paste0(
    html_body,
    "
</div>"
  )

  # Learning Resources
  html_body <- paste0(
    html_body,
    '
<div class="section"><h2>Learning Resources</h2>
<p>To learn more about the BID framework concepts used in this report:</p>'
  )

  learning_items <- c(
    "Review the {bidux} vignettes with `vignette('introduction-to-bid')`",
    "Explore the concept dictionary with `bid_concepts()`",
    "Check the package documentation at https://github.com/jrwinget/bidux"
  )
  html_body <- paste0(html_body, generate_html_list(learning_items))

  html_body <- paste0(
    html_body,
    "
</div>"
  )

  # Complete HTML document
  html_report <- paste(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"UTF-8\">",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
    "  <title>BID Framework Implementation Report</title>",
    "  <style>",
    css,
    "  </style>",
    "</head>",
    "<body>",
    html_body,
    "</body>",
    "</html>",
    sep = "\n"
  )

  return(html_report)
}
