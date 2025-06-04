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

  cli::cli_h1("BID Framework Report")

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

  cli::cli_alert_info("Generating {.field {toupper(format)}} report")

  # initialize report
  report <- character(0)

  # add header
  report <- c(report, "# BID Framework Implementation Report")
  report <- c(report, paste0("Generated: ", Sys.time()))
  report <- c(report, "")

  # BID overview (with diagram if requested)
  if (include_diagrams) {
    cli::cli_alert_info("Including BID framework diagram")
    report <- c(report, "## BID Framework Overview")
    report <- c(report, "```")
    report <- c(report, "+-------------+  +-------------+  +-------------+  +-------------+  +-------------+")
    report <- c(report, "|    NOTICE   |  |  INTERPRET  |  |  STRUCTURE  |  |  ANTICIPATE |  |   VALIDATE  |")
    report <- c(report, "|    Stage 1  |->|    Stage 2  |->|    Stage 3  |->|    Stage 4  |->|    Stage 5  |")
    report <- c(report, "+-------------+  +-------------+  +-------------+  +-------------+  +-------------+")
    report <- c(report, "```")
    report <- c(report, "")
  }

  # helper function to safely extract and format data
  safe_extract <- function(data, field, default = NA_character_, prefix = "") {
    if (field %in% names(data) && !is.null(data[[field]][1])) {
      if (!is.na(data[[field]][1])) {
        return(paste0(prefix, data[[field]][1]))
      }
    }
    if (is.na(default)) {
      return(NA_character_)
    }
    return(paste0(prefix, default))
  }

  # stage 5: validate
  cli::cli_alert_info("Adding Stage 5: Validate information")
  report <- c(report, "## Stage 5: Validate & Empower the User")

  # add summary panel
  summary_panel <- safe_extract(
    validate_stage,
    "summary_panel",
    "No summary panel provided"
  )
  report <- c(report, paste0("- **Summary Panel**: ", summary_panel))

  # add collaboration features
  collaboration <- safe_extract(
    validate_stage,
    "collaboration",
    "No collaboration features specified"
  )
  report <- c(report, paste0("- **Collaboration Features**: ", collaboration))

  # add next steps
  if (
    "next_steps" %in% names(validate_stage) &&
      !is.na(validate_stage$next_steps[1])
  ) {
    cli::cli_alert_success("Found {.field next_steps} information")
    next_steps <- tryCatch(
      {
        unlist(strsplit(validate_stage$next_steps[1], ";"))
      },
      error = function(e) {
        cli::cli_warn("Could not parse next_steps: {e$message}")
        return(c("No next steps available or could not be parsed"))
      }
    )

    report <- c(report, "- **Next Steps**:")
    for (step in next_steps) {
      step_text <- trimws(step)
      if (nchar(step_text) > 0) {
        report <- c(report, paste0("  - ", step_text))
      }
    }
  } else {
    cli::cli_alert_warning("No next steps found in validate_stage")
    report <- c(report, "- **Next Steps**: None specified")
  }

  # add suggestions
  suggestions <- safe_extract(
    validate_stage,
    "suggestions",
    "No suggestions available"
  )
  report <- c(report, paste0("- **Suggestions**: ", suggestions))
  report <- c(report, "")

  # stage 4: anticipate
  if ("previous_bias" %in% names(validate_stage)) {
    cli::cli_alert_info("Adding Stage 4: Anticipate information")
    report <- c(report, "## Stage 4: Anticipate User Behavior")

    bias_mitigations <- safe_extract(
      validate_stage,
      "previous_bias",
      "No bias mitigations specified"
    )
    report <- c(report, paste0("- **Bias Mitigations**: ", bias_mitigations))

    interaction_principles <- safe_extract(
      validate_stage,
      "previous_interaction",
      "No interaction principles specified"
    )
    if (!is.na(interaction_principles)) {
      report <- c(
        report,
        paste0("- **Interaction Principles**: ", interaction_principles)
      )
    }

    report <- c(report, "")
  } else {
    cli::cli_alert_warning("No Anticipate stage information found")
  }

  # stage 3: structure
  if ("previous_layout" %in% names(validate_stage)) {
    cli::cli_alert_info("Adding Stage 3: Structure information")
    report <- c(report, "## Stage 3: Structure the Dashboard")

    layout <- safe_extract(
      validate_stage,
      "previous_layout",
      "No layout specified"
    )
    report <- c(report, paste0("- **Layout**: ", layout))

    concepts <- safe_extract(
      validate_stage,
      "previous_concepts",
      "No concepts specified"
    )
    if (!is.na(concepts)) {
      report <- c(report, paste0("- **Applied Concepts**: ", concepts))
    }

    accessibility <- safe_extract(
      validate_stage,
      "previous_accessibility",
      "No accessibility considerations specified"
    )
    if (!is.na(accessibility)) {
      report <- c(
        report,
        paste0("- **Accessibility Considerations**: ", accessibility)
      )
    }

    report <- c(report, "")
  } else {
    cli::cli_alert_warning("No Structure stage information found")
  }

  # recommendations
  cli::cli_alert_info("Adding implementation recommendations")
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

  # helper function to safely add component suggestions
  add_component_suggestions <- function(report, validate_stage, package_name, title) {
    tryCatch(
      {
        if (!requireNamespace("dplyr", quietly = TRUE)) {
          cli::cli_alert_warning(
            "Package {.pkg dplyr} not available, skipping {package_name} suggestions"
          )
          return(report)
        }

        suggestions <- bid_suggest_components(
          validate_stage,
          package = package_name
        )

        if (nrow(suggestions) == 0) {
          cli::cli_alert_warning("No {package_name} suggestions available")
          return(report)
        }

        top_suggestions <- head(suggestions, 3)

        cli::cli_alert_success(
          "Found {nrow(top_suggestions)} {package_name} components to recommend"
        )

        report <- c(report, paste0("**", title, "**"))
        for (i in 1:nrow(top_suggestions)) {
          report <- c(
            report,
            paste0(
              "- ", top_suggestions$component[i], ": ",
              top_suggestions$description[i]
            )
          )
        }
        report <- c(report, "")
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Error getting {package_name} suggestions: {e$message}"
        )
        report <- c(report, paste0("**", title, "**"))
        report <- c(
          report,
          paste0("- Could not generate suggestions: ", e$message)
        )
        report <- c(report, "")
      }
    )
    return(report)
  }

  report <- add_component_suggestions(
    report,
    validate_stage,
    "bslib",
    "bslib Components:"
  )

  report <- add_component_suggestions(
    report,
    validate_stage,
    "shiny",
    "Shiny Components:"
  )

  # sext steps suggestions
  cli::cli_alert_info("Adding next steps guidance")
  report <- c(report, "## Next Steps")
  report <- c(report, "1. Implement key BID principles identified in this analysis")
  report <- c(report, "2. Conduct user testing to validate improvements")
  report <- c(report, "3. Iterate based on feedback")
  report <- c(report, "4. Document successful patterns for future projects")
  report <- c(report, "")

  # learning resources
  cli::cli_alert_info("Adding learning resources")
  report <- c(report, "## Learning Resources")
  report <- c(report, "To learn more about the BID framework concepts used in this report:")
  report <- c(report, "- Review the {bidux} vignettes with `vignette('introduction-to-bid')`")
  report <- c(report, "- Explore the concept dictionary with `bid_concepts()`")
  report <- c(report, "- Check the package documentation at https://github.com/jrwinget/bidux")
  report <- c(report, "")

  # format report
  if (format == "html") {
    cli::cli_alert_info("Formatting as HTML")
    html_report <- generate_html_report(report)
    cli::cli_alert_success("HTML report generated successfully")
    return(html_report)
  } else if (format == "markdown") {
    cli::cli_alert_success("Markdown report generated successfully")
    return(paste(report, collapse = "\n"))
  } else {
    cli::cli_alert_info("Formatting as plain text")
    text_report <- gsub("^#+ ", "", report)
    text_report <- gsub("\\*\\*", "", text_report)
    cli::cli_alert_success("Text report generated successfully")
    return(paste(text_report, collapse = "\n"))
  }
}

# helper function to generate HTML report
generate_html_report <- function(report) {
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

  convert_to_html <- function(md_lines) {
    html_lines <- character(length(md_lines))

    for (i in seq_along(md_lines)) {
      line <- md_lines[i]

      # headers
      if (grepl("^# ", line)) {
        html_lines[i] <- gsub("^# (.*?)$", "<h1>\\1</h1>", line)
      } else if (grepl("^## ", line)) {
        html_lines[i] <- gsub("^## (.*?)$", "<h2>\\1</h2>", line)
      } else if (grepl("^### ", line)) {
        html_lines[i] <- gsub("^### (.*?)$", "<h3>\\1</h3>", line)
      }
      # list items with strong elements
      else if (grepl("^- \\*\\*(.*?)\\*\\*: (.*?)$", line)) {
        html_lines[i] <- gsub(
          "^- \\*\\*(.*?)\\*\\*: (.*?)$",
          "<li><strong>\\1:</strong> \\2</li>",
          line
        )
      }
      # regular list items
      else if (grepl("^- (.*?)$", line)) {
        html_lines[i] <- gsub("^- (.*?)$", "<li>\\1</li>", line)
      }
      # numbered list items
      else if (grepl("^\\d+\\. (.*?)$", line)) {
        html_lines[i] <- gsub("^\\d+\\. (.*?)$", "<li>\\1</li>", line)
      }
      # code blocks
      else if (line == "```") {
        # start or end of code block
        if (exists("in_code_block") && in_code_block) {
          html_lines[i] <- "</pre>"
          in_code_block <- FALSE
        } else {
          html_lines[i] <- "<pre class=\"diagram\">"
          in_code_block <- TRUE
        }
      }
      # empty lines to paragraph breaks
      else if (line == "") {
        html_lines[i] <- ""
      }
      # regular text becomes paragraphs
      else {
        if (exists("in_code_block") && in_code_block) {
          # inside code block, preserve as is
          html_lines[i] <- line
        } else {
          # wrap other lines in paragraph tags if not empty
          if (nchar(trimws(line)) > 0) {
            html_lines[i] <- paste0("<p>", line, "</p>")
          } else {
            html_lines[i] <- ""
          }
        }
      }
    }

    # clean up any remaining code block state
    if (exists("in_code_block") && in_code_block) {
      html_lines <- c(html_lines, "</pre>")
    }

    return(html_lines)
  }

  html_content <- convert_to_html(report)

  # group consecutive list items
  html_content <- gsub(
    "(<li>.*?</li>)\\s*(<li>.*?</li>)",
    "\\1\\2",
    paste(html_content, collapse = "\n"),
    perl = TRUE
  )

  html_content <- gsub(
    "(<li>.*?</li>)+",
    "<ul>\\0</ul>",
    html_content,
    perl = TRUE
  )

  html_content <- gsub(
    "<h2>(.*?)</h2>",
    '<div class="section"><h2>\\1</h2>',
    html_content
  )
  html_content <- gsub(
    "<h2>(.*?)</h2>(.+?)(?=<div class=\"section\">|$)",
    '<div class="section"><h2>\\1</h2>\\2</div>',
    html_content,
    perl = TRUE
  )

  # final HTML
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
    html_content,
    "</body>",
    "</html>",
    sep = "\n"
  )

  return(html_report)
}
