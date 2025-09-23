#' Centralized Suggestion Rules for BID Framework
#'
#' @description
#' This file consolidates simple suggestion rules for Interpret, Notice, and
#' Validate stages to reduce duplication and provide consistent messaging
#' across the BID framework.

# ==============================================================================
# SIMPLE SUGGESTION RULES
# ==============================================================================

#' Get consolidated suggestion rules for all BID stages
#'
#' @description
#' Returns a structured list of suggestion rules for each BID stage.
#' These rules help generate consistent, context-aware suggestions
#' based on user input patterns and common issues.
#'
#' @return Named list of suggestion rules by stage
#' @keywords internal
get_consolidated_suggestion_rules <- function() {
  list(
    # interpret stage rules
    interpret = list(
      # data story completeness rules
      list(
        condition = function(ctx) {
          story_fields <- c("hook", "context", "tension", "resolution")
          story_data <- ctx$data_story
          if (is.null(story_data)) {
            return(TRUE)
          }

          complete_fields <- sum(sapply(story_fields, function(field) {
            val <- story_data[[field]]
            !is.null(val) && !is.na(val) && nchar(trimws(as.character(val))) > 0
          }))

          complete_fields < 3 # less than 3 complete fields
        },
        message = "Consider completing your data story with hook, context, tension, and resolution elements"
      ),

      # central question clarity rules
      list(
        condition = function(ctx) {
          question <- ctx$central_question %||% ""
          nchar(trimws(question)) < 10
        },
        message = "Provide a more detailed central question to guide your analysis"
      ),
      list(
        condition = function(ctx) {
          question <- ctx$central_question %||% ""
          nchar(trimws(question)) > 150
        },
        message = "Consider simplifying your central question for better focus"
      ),

      # audience definition rules
      list(
        condition = function(ctx) {
          audience_fields <- c("audience", "target_audience", "personas")
          has_audience <- any(sapply(audience_fields, function(field) {
            val <- ctx[[field]]
            !is.null(val) && !is.na(val) && nchar(trimws(as.character(val))) > 0
          }))
          !has_audience
        },
        message = "Define your target audience or user personas to improve design focus"
      )
    ),

    # notice stage rules
    notice = list(
      # problem description quality rules
      list(
        condition = function(ctx) {
          problem <- ctx$problem %||% ""
          nchar(trimws(problem)) < 15
        },
        message = "Provide more detail in your problem description for better analysis"
      ),

      # evidence strength rules
      list(
        condition = function(ctx) {
          evidence <- ctx$evidence %||% ""
          nchar(trimws(evidence)) < 15
        },
        message = "Add quantitative metrics or specific examples to strengthen your evidence"
      ),
      list(
        condition = function(ctx) {
          evidence <- ctx$evidence %||% ""
          has_numbers <- grepl("\\d+%|\\d+\\.\\d+%|\\d+ out of \\d+|\\d+ users?|\\d+ sessions?", evidence)
          !has_numbers
        },
        message = "Consider adding quantitative data (percentages, counts, metrics) to support your evidence"
      ),

      # theory application rules
      list(
        condition = function(ctx) {
          theory <- ctx$theory
          problem <- ctx$problem %||% ""

          # suggest specific theories based on problem patterns
          if (grepl("too many|overwhelm|choice|complex", tolower(problem))) {
            is.null(theory) || !grepl("choice|cognitive load|miller", tolower(theory))
          } else {
            FALSE
          }
        },
        message = "Consider Choice Architecture or Cognitive Load Theory for complexity issues"
      ),

      # evidence-theory alignment rules
      list(
        condition = function(ctx) {
          theory <- ctx$theory %||% ""
          evidence <- ctx$evidence %||% ""

          # check for misalignment patterns
          if (grepl("cognitive load", tolower(theory)) && !grepl("confus|overwhelm|complex|difficult", tolower(evidence))) {
            return(TRUE)
          }
          if (grepl("visual hierarchy", tolower(theory)) && !grepl("focus|attention|important|priority", tolower(evidence))) {
            return(TRUE)
          }
          FALSE
        },
        message = "Ensure your evidence aligns with the chosen behavioral theory"
      )
    ),

    # validate stage rules
    validate = list(
      # telemetry integration rules
      list(
        condition = function(ctx) {
          has_telemetry_refs <- !is.null(ctx$telemetry_refs) && length(ctx$telemetry_refs) > 0
          include_telemetry <- ctx$include_telemetry %||% FALSE

          include_telemetry && !has_telemetry_refs
        },
        message = "Add specific telemetry tracking points to measure success of your BID implementation"
      ),

      # measurement completeness rules
      list(
        condition = function(ctx) {
          validation_steps <- ctx$validation_steps %||% ""
          has_metrics <- grepl("measur|metric|track|count|rate|time", tolower(validation_steps))
          !has_metrics
        },
        message = "Include specific metrics or measurements in your validation approach"
      ),

      # timeline and iteration rules
      list(
        condition = function(ctx) {
          validation_steps <- ctx$validation_steps %||% ""
          has_timeline <- grepl("week|month|day|timeline|schedule", tolower(validation_steps))
          !has_timeline
        },
        message = "Consider adding timeline estimates for validation activities"
      ),

      # user testing integration rules
      list(
        condition = function(ctx) {
          validation_steps <- ctx$validation_steps %||% ""
          has_user_testing <- grepl("user test|usabilit|interview|survey|feedback", tolower(validation_steps))
          !has_user_testing
        },
        message = "Include user testing or feedback collection in your validation plan"
      )
    )
  )
}

#' Apply suggestion rules to context data
#'
#' @description
#' Evaluates suggestion rules against provided context data and returns
#' applicable suggestions. Used by generate_stage_suggestions() for
#' consistent suggestion generation.
#'
#' @param stage_name Name of the BID stage
#' @param context_data Named list with stage-specific context
#' @param rules_list Optional custom rules list (defaults to consolidated rules)
#' @return Character vector of applicable suggestions
#' @keywords internal
apply_suggestion_rules <- function(stage_name, context_data, rules_list = NULL) {
  if (is.null(rules_list)) {
    rules_list <- get_consolidated_suggestion_rules()
  }

  stage_rules <- rules_list[[stage_name]]
  if (is.null(stage_rules) || length(stage_rules) == 0) {
    return(character(0))
  }

  applicable_suggestions <- character(0)

  for (rule in stage_rules) {
    if (!is.list(rule) || is.null(rule$condition) || is.null(rule$message)) {
      next # skip malformed rules
    }

    # evaluate condition safely
    condition_met <- FALSE
    tryCatch(
      {
        if (is.function(rule$condition)) {
          condition_met <- rule$condition(context_data)
        }
      },
      error = function(e) {
        # silently skip rules with evaluation errors
        condition_met <<- FALSE
      }
    )

    if (isTRUE(condition_met)) {
      applicable_suggestions <- c(applicable_suggestions, rule$message)
    }
  }

  return(applicable_suggestions)
}

#' Remove duplicate warning-suggestion pairs
#'
#' @description
#' Identifies and removes redundant warnings where the warning message
#' duplicates information already provided in suggestions. Helps clean up
#' verbose output by preferring actionable suggestions over warnings.
#'
#' @param warnings Character vector of warning messages
#' @param suggestions Character vector of suggestion messages
#' @param similarity_threshold Similarity threshold for detecting duplicates (0-1)
#' @return List with cleaned warnings and suggestions
#' @keywords internal
deduplicate_warnings_suggestions <- function(warnings, suggestions, similarity_threshold = 0.7) {
  if (length(warnings) == 0 || length(suggestions) == 0) {
    return(list(warnings = warnings, suggestions = suggestions))
  }

  # convert to lowercase for comparison
  warnings_lower <- tolower(warnings)
  suggestions_lower <- tolower(suggestions)

  # identify warnings that are very similar to suggestions
  duplicate_warnings <- logical(length(warnings))

  for (i in seq_along(warnings)) {
    warning_words <- strsplit(warnings_lower[i], "\\W+")[[1]]
    warning_words <- warning_words[nchar(warning_words) > 2] # ignore short words

    for (j in seq_along(suggestions)) {
      suggestion_words <- strsplit(suggestions_lower[j], "\\W+")[[1]]
      suggestion_words <- suggestion_words[nchar(suggestion_words) > 2]

      if (length(warning_words) == 0 || length(suggestion_words) == 0) {
        next
      }

      # calculate word overlap
      common_words <- intersect(warning_words, suggestion_words)
      overlap_ratio <- length(common_words) / max(length(warning_words), length(suggestion_words))

      if (overlap_ratio >= similarity_threshold) {
        duplicate_warnings[i] <- TRUE
        break
      }
    }
  }

  # return cleaned lists
  list(
    warnings = warnings[!duplicate_warnings],
    suggestions = suggestions
  )
}

#' Generate fallback suggestions for stages without specific rules
#'
#' @description
#' Provides generic but helpful suggestions for BID stages that don't have
#' specific rules defined or when no rules match the context.
#'
#' @param stage_name Name of the BID stage
#' @return Character string with fallback suggestion
#' @keywords internal
get_fallback_suggestion <- function(stage_name) {
  fallbacks <- list(
    "Interpret" = "Focus on creating a compelling data story with clear narrative structure",
    "Notice" = "Ensure your problem description is specific and supported by strong evidence",
    "Anticipate" = "Consider how cognitive biases might affect user interpretation of your design",
    "Structure" = "Choose a layout that aligns with user mental models and task flows",
    "Validate" = "Plan specific metrics and user feedback collection to measure success"
  )

  fallbacks[[stage_name]] %||% "Follow BID framework best practices for this stage"
}
