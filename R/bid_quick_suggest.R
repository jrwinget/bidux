#' Quick UX Suggestions for Shiny Developers
#'
#' @description
#' Provides a streamlined, single-step workflow for Shiny developers who need
#' quick UX suggestions without going through the full 5-stage BID framework.
#' This function internally leverages the BID framework stages but presents
#' results in a simple, actionable format.
#'
#' Unlike the full BID workflow (Interpret -> Notice -> Anticipate -> Structure
#' -> Validate), this function provides immediate suggestions based on a problem
#' description. Use this for rapid prototyping or when you need quick guidance.
#' For comprehensive UX redesign projects, use the full BID workflow.
#'
#' @param problem Required. A character string describing the UX problem. Examples:
#'        "Users can't find the download button", "Information overload on
#'        dashboard", "Mobile interface is hard to navigate".
#' @param context Optional. Additional context about the application or users.
#'        This helps refine suggestions to your specific situation.
#' @param package Optional. Filter suggestions to specific package ("bslib",
#'        "shiny", "reactable", "DT", etc.). If NULL, returns suggestions for
#'        all relevant packages.
#' @param limit Optional. Maximum number of suggestions to return (default: 10).
#'        Set to Inf to return all suggestions.
#' @param min_score Optional. Minimum relevance score 0-1 (default: 0.7).
#'        Higher values return only the most relevant suggestions.
#' @param quiet Optional. Logical indicating whether to suppress informational
#'        messages. If NULL, uses getOption("bidux.quiet", FALSE).
#'
#' @return A tibble with columns:
#'   \item{title}{Brief actionable description of the suggestion}
#'   \item{details}{Specific implementation guidance}
#'   \item{components}{Shiny/bslib component recommendations (character vector)}
#'   \item{concept}{UX concept the suggestion is based on}
#'   \item{score}{Relevance score (0-1, higher is more relevant)}
#'   \item{difficulty}{Implementation difficulty (easy/moderate/advanced)}
#'   \item{rationale}{1-2 sentence explanation of why this helps}
#'
#' @details
#' **How it works:**
#'
#' The function analyzes your problem description using keyword matching and
#' semantic analysis to:
#' 1. Identify relevant UX concepts (cognitive load, navigation, visual hierarchy, etc.)
#' 2. Detect appropriate layout patterns (grid, card, breathable, etc.)
#' 3. Generate ranked suggestions with specific component recommendations
#' 4. Filter and sort by relevance score
#'
#' **Problem Analysis Keywords:**
#' - "overload", "overwhelm", "too many" -> Cognitive Load Theory
#' - "find", "search", "navigate" -> Information Scent
#' - "cluttered", "messy", "disorganized" -> Visual Hierarchy
#' - "mobile", "touch", "responsive" -> Fitts's Law
#' - "confusing", "unclear", "complex" -> Progressive Disclosure
#'
#' **When to use this vs full BID workflow:**
#' - Use `bid_quick_suggest()`: Quick fixes, prototyping, single issues
#' - Use full workflow: Comprehensive redesigns, complex projects, team collaboration
#'
#' @examples
#' # Basic usage
#' suggestions <- bid_quick_suggest(
#'   problem = "Users can't find the download button"
#' )
#' print(suggestions)
#'
#' # With additional context
#' suggestions <- bid_quick_suggest(
#'   problem = "Dashboard has too many charts and metrics",
#'   context = "Financial analysts need quick insights but get overwhelmed",
#'   limit = 5
#' )
#'
#' # Filter to specific package
#' bslib_suggestions <- bid_quick_suggest(
#'   problem = "Mobile interface is hard to use",
#'   package = "bslib",
#'   min_score = 0.8
#' )
#'
#' # Navigation issues
#' nav_suggestions <- bid_quick_suggest(
#'   problem = "Users get lost in multi-tab interface",
#'   context = "Application has 10+ tabs with nested content"
#' )
#'
#' # Information overload
#' overload_suggestions <- bid_quick_suggest(
#'   problem = "Too many filters and options on the sidebar",
#'   context = "Beginners find the interface overwhelming"
#' )
#'
#' @export
bid_quick_suggest <- function(
    problem,
    context = NULL,
    package = NULL,
    limit = 10,
    min_score = 0.7,
    quiet = NULL) {
  # ============================================================================
  # parameter validation
  # ============================================================================

  # validate problem (required)
  validate_character_param(problem, "problem", required = TRUE, min_length = 5)

  # validate context (optional)
  if (!is.null(context)) {
    validate_character_param(context, "context", required = FALSE, min_length = 1)
  }

  # validate package (optional)
  valid_packages <- c(
    "bslib", "shiny", "reactable", "DT", "plotly",
    "leaflet", "shinyWidgets", "shinydashboard"
  )
  if (!is.null(package)) {
    if (!is.character(package) || length(package) != 1) {
      cli::cli_abort(standard_error_msg(
        "Parameter 'package' must be a single character string",
        context = glue::glue("You provided: {class(package)[1]} of length {length(package)}"),
        suggestions = c(
          glue::glue("Valid packages: {paste(valid_packages, collapse = ', ')}"),
          "Or leave NULL to include all packages"
        )
      ))
    }
    package_clean <- tolower(trimws(package))
    if (!package_clean %in% valid_packages) {
      cli::cli_warn(c(
        "!" = glue::glue("Package '{package}' is not in the standard list"),
        "i" = glue::glue("Standard packages: {paste(valid_packages, collapse = ', ')}"),
        "i" = "Suggestions may be limited"
      ))
    }
  }

  # validate limit (optional)
  if (!is.numeric(limit) || length(limit) != 1 || limit < 1) {
    cli::cli_abort(standard_error_msg(
      "Parameter 'limit' must be a positive number",
      context = glue::glue("You provided: {limit}"),
      suggestions = "Use a positive integer (e.g., 10) or Inf for all results"
    ))
  }

  # validate min_score (optional)
  if (!is.numeric(min_score) || length(min_score) != 1 ||
    min_score < 0 || min_score > 1) {
    cli::cli_abort(standard_error_msg(
      "Parameter 'min_score' must be a number between 0 and 1",
      context = glue::glue("You provided: {min_score}"),
      suggestions = "Use values like 0.7 (default), 0.8 (strict), 0.5 (lenient)"
    ))
  }

  # ============================================================================
  # analyze problem and build context
  # ============================================================================

  bid_alert_info("Analyzing your UX problem", quiet = quiet)

  # clean inputs
  problem_clean <- trimws(problem)
  context_clean <- if (!is.null(context)) trimws(context) else NULL

  # combine problem and context for analysis
  combined_text <- paste(
    problem_clean,
    if (!is.null(context_clean)) context_clean else "",
    sep = " "
  )

  # detect relevant concepts from problem description
  detected_concepts <- .detect_concepts_from_problem(combined_text)

  # detect layout needs
  suggested_layout <- .detect_layout_from_problem(combined_text)

  bid_alert_info(
    glue::glue("Detected {length(detected_concepts)} relevant UX concepts"),
    quiet = quiet
  )
  bid_alert_info(
    glue::glue("Suggested layout pattern: {suggested_layout}"),
    quiet = quiet
  )

  # ============================================================================
  # create minimal BID stages internally
  # ============================================================================

  # create minimal data story from problem
  data_story_auto <- new_data_story(
    hook = .extract_hook_from_problem(problem_clean),
    context = if (!is.null(context_clean)) {
      context_clean
    } else {
      "User experience issue identified"
    },
    tension = problem_clean,
    resolution = "Apply UX best practices to address the problem"
  )

  # create minimal interpret stage (quietly)
  interpret_result <- bid_with_quiet({
    bid_interpret(
      central_question = paste0("How can we address: ", problem_clean, "?"),
      data_story = data_story_auto
    )
  })

  # detect or suggest theory from problem
  theory_result <- .suggest_theory_from_text(
    problem_clean,
    context_clean,
    mappings = NULL,
    show_message = FALSE
  )

  # create minimal notice stage (quietly)
  notice_result <- bid_with_quiet({
    bid_notice(
      previous_stage = interpret_result,
      problem = problem_clean,
      theory = theory_result$theory,
      evidence = if (!is.null(context_clean)) {
        context_clean
      } else {
        "Problem identified through UX analysis"
      }
    )
  })

  # create structure stage to get suggestions (quietly)
  structure_result <- suppressWarnings(bid_with_quiet({
    bid_structure(
      previous_stage = notice_result,
      concepts = detected_concepts,
      quiet = TRUE
    )
  }))

  # ============================================================================
  # extract and process suggestions
  # ============================================================================

  bid_alert_info("Generating actionable suggestions", quiet = quiet)

  # extract suggestion groups from structure stage
  # suggestions is stored as a list in the tibble
  # structure_result$suggestions directly gives us the list of groups
  suggestion_groups <- structure_result$suggestions

  # flatten suggestions into tibble format
  all_suggestions <- .flatten_suggestions_to_tibble(
    suggestion_groups,
    suggested_layout,
    combined_text
  )

  # apply filters
  filtered_suggestions <- all_suggestions

  # filter by min_score
  if (!is.null(min_score) && nrow(filtered_suggestions) > 0) {
    filtered_suggestions <- filtered_suggestions[
      filtered_suggestions$score >= min_score,
    ]
  }

  # filter by package if specified
  if (!is.null(package) && nrow(filtered_suggestions) > 0) {
    package_clean <- tolower(trimws(package))
    package_match <- sapply(filtered_suggestions$components, function(comp_vec) {
      any(grepl(package_clean, tolower(comp_vec), fixed = TRUE))
    })
    filtered_suggestions <- filtered_suggestions[package_match, ]
  }

  # apply limit
  if (!is.null(limit) && !is.infinite(limit) && nrow(filtered_suggestions) > limit) {
    filtered_suggestions <- filtered_suggestions[1:limit, ]
  }

  # ============================================================================
  # return results with summary message
  # ============================================================================

  n_suggestions <- nrow(filtered_suggestions)

  if (n_suggestions == 0) {
    bid_message(
      "No suggestions found",
      "Try adjusting parameters:",
      "  - Lower min_score (current: {min_score})",
      "  - Remove package filter",
      "  - Provide more context about the problem",
      quiet = quiet
    )
  } else {
    summary_msg <- glue::glue(
      "Found {n_suggestions} suggestion{if (n_suggestions > 1) 's' else ''}"
    )

    if (!is.null(package)) {
      summary_msg <- paste0(
        summary_msg,
        glue::glue(" for package '{package}'")
      )
    }

    avg_score <- round(mean(filtered_suggestions$score, na.rm = TRUE), 2)
    summary_msg <- paste0(
      summary_msg,
      glue::glue(" (avg relevance: {avg_score})")
    )

    bid_message(
      "Quick suggestions ready",
      summary_msg,
      glue::glue("Top concept: {filtered_suggestions$concept[1]}"),
      "Use bid_concept() to learn more about any concept",
      quiet = quiet
    )
  }

  return(filtered_suggestions)
}

# ==============================================================================
# internal helper functions
# ==============================================================================

#' Detect relevant UX concepts from problem text
#'
#' @param text Combined problem and context text
#' @return Character vector of concept names
#' @keywords internal
#' @noRd
.detect_concepts_from_problem <- function(text) {
  text_lower <- tolower(text)
  detected <- character(0)

  # concept detection patterns
  concept_patterns <- list(
    "Cognitive Load Theory" = c(
      "overload", "overwhelm", "too many", "too much",
      "complex", "confusing", "mental load", "difficult"
    ),
    "Progressive Disclosure" = c(
      "hide", "show", "reveal", "collapse", "expand",
      "step", "gradually", "progressive", "accordion"
    ),
    "Visual Hierarchy" = c(
      "hierarchy", "priority", "important", "focus",
      "attention", "prominence", "clutter", "messy", "disorganized"
    ),
    "Information Scent" = c(
      "find", "search", "locate", "discover",
      "navigation", "navigate", "lost", "wayfinding"
    ),
    "Fitts's Law" = c(
      "mobile", "touch", "tap", "responsive",
      "small", "target", "button", "click"
    ),
    "Hick's Law" = c(
      "choice", "option", "dropdown", "select",
      "decide", "decision", "many option"
    ),
    "Principle of Proximity" = c(
      "group", "related", "together", "proximity",
      "close", "associate", "spacing"
    ),
    "User Onboarding" = c(
      "first time", "new user", "beginner", "novice",
      "getting started", "tutorial", "help", "guidance"
    )
  )

  for (concept_name in names(concept_patterns)) {
    keywords <- concept_patterns[[concept_name]]
    if (any(sapply(keywords, function(k) grepl(k, text_lower, fixed = TRUE)))) {
      detected <- c(detected, concept_name)
    }
  }

  # ensure at least some core concepts if none detected
  if (length(detected) == 0) {
    detected <- c("Cognitive Load Theory", "Visual Hierarchy")
  }

  return(unique(detected))
}

#' Detect appropriate layout from problem description
#'
#' @param text Combined problem and context text
#' @return Character string with layout type
#' @keywords internal
#' @noRd
.detect_layout_from_problem <- function(text) {
  text_lower <- tolower(text)

  # layout detection heuristics (similar to suggest_layout_from_previous)
  if (grepl("overload|overwhelm|too many|confus|clutter|busy", text_lower)) {
    return("breathable")
  }

  if (grepl("summary.*detail|overview.*detail|quick.*deep", text_lower)) {
    return("dual_process")
  }

  if (grepl("group|cluster|compare|related metric|visual hierarchy", text_lower)) {
    return("grid")
  }

  if (grepl("card|tile|modular|chunk", text_lower)) {
    return("card")
  }

  if (grepl("section|categor|tab|navigation", text_lower)) {
    return("tabs")
  }

  # default fallback
  return("breathable")
}

#' Extract hook from problem description
#'
#' @param problem Problem text
#' @return Character string for data story hook
#' @keywords internal
#' @noRd
.extract_hook_from_problem <- function(problem) {
  # create attention-grabbing hook from problem
  problem_lower <- tolower(problem)

  if (grepl("can't|cannot|unable|difficult|hard|struggle", problem_lower)) {
    return("Users are facing obstacles")
  } else if (grepl("confus|unclear|lost", problem_lower)) {
    return("Users are getting confused")
  } else if (grepl("overload|overwhelm|too many", problem_lower)) {
    return("Users are feeling overwhelmed")
  } else if (grepl("mobile|touch|responsive", problem_lower)) {
    return("Mobile experience needs improvement")
  } else if (grepl("find|search|navigate|locate", problem_lower)) {
    return("Users can't find what they need")
  } else {
    return("User experience issue detected")
  }
}

#' Flatten suggestion groups to tibble format
#'
#' @param suggestion_groups List of concept groups from structure stage
#' @param layout Layout type for context
#' @param problem_text Original problem text for scoring adjustments
#' @return Tibble with flattened suggestions
#' @keywords internal
#' @noRd
.flatten_suggestions_to_tibble <- function(
    suggestion_groups,
    layout,
    problem_text) {
  all_rows <- list()

  # handle different input formats
  # suggestion_groups can be:
  # 1. A list of concept groups (each with $concept and $suggestions)
  # 2. Already a flat list of suggestions

  # check if this is a list of concept groups or already flattened
  is_grouped <- FALSE
  if (length(suggestion_groups) > 0) {
    first_item <- suggestion_groups[[1]]
    if (is.list(first_item) && !is.null(first_item$concept) && !is.null(first_item$suggestions)) {
      is_grouped <- TRUE
    }
  }

  if (is_grouped) {
    # process grouped suggestions
    for (group in suggestion_groups) {
      concept <- group$concept
      suggestions <- group$suggestions

      for (sug in suggestions) {
        # determine difficulty based on components
        difficulty <- .estimate_difficulty(sug$components)

        # adjust score based on problem relevance
        adjusted_score <- .adjust_score_for_problem(
          sug$score,
          sug$title,
          sug$details,
          problem_text
        )

        row <- list(
          title = sug$title,
          details = sug$details,
          components = list(sug$components), # store as list column
          concept = concept,
          score = adjusted_score,
          difficulty = difficulty,
          rationale = sug$rationale
        )

        all_rows <- append(all_rows, list(row))
      }
    }
  } else {
    # this shouldn't happen with current implementation but handle anyway
    return(tibble::tibble(
      title = character(0),
      details = character(0),
      components = list(),
      concept = character(0),
      score = numeric(0),
      difficulty = character(0),
      rationale = character(0)
    ))
  }

  # convert to tibble
  if (length(all_rows) == 0) {
    return(tibble::tibble(
      title = character(0),
      details = character(0),
      components = list(),
      concept = character(0),
      score = numeric(0),
      difficulty = character(0),
      rationale = character(0)
    ))
  }

  result <- tibble::tibble(
    title = sapply(all_rows, function(x) x$title),
    details = sapply(all_rows, function(x) x$details),
    components = lapply(all_rows, function(x) x$components[[1]]),
    concept = sapply(all_rows, function(x) x$concept),
    score = sapply(all_rows, function(x) x$score),
    difficulty = sapply(all_rows, function(x) x$difficulty),
    rationale = sapply(all_rows, function(x) x$rationale)
  )

  # sort by score descending
  result <- result[order(result$score, decreasing = TRUE), ]

  return(result)
}

#' Estimate implementation difficulty from components
#'
#' @param components Character vector of component names
#' @return Character string: "easy", "moderate", or "advanced"
#' @keywords internal
#' @noRd
.estimate_difficulty <- function(components) {
  if (length(components) == 0) {
    return("moderate")
  }

  components_str <- paste(tolower(components), collapse = " ")

  # easy: basic shiny/bslib components
  easy_patterns <- c(
    "shiny::h1", "shiny::h2", "shiny::h3",
    "shiny::helptext", "bslib::card_header",
    "bslib::value_box", "shiny::actionbutton"
  )
  if (any(sapply(easy_patterns, function(p) grepl(p, components_str, fixed = TRUE)))) {
    return("easy")
  }

  # advanced: complex components or multiple integrations
  advanced_patterns <- c(
    "dt::datatable", "reactable::reactable",
    "shiny::modaldialog", "shinyjs", "plotly",
    "shiny::observeevent"
  )
  if (any(sapply(advanced_patterns, function(p) grepl(p, components_str, fixed = TRUE)))) {
    return("advanced")
  }

  # default to moderate
  return("moderate")
}

#' Adjust suggestion score based on problem relevance
#'
#' @param base_score Base score from structure stage
#' @param title Suggestion title
#' @param details Suggestion details
#' @param problem_text Original problem text
#' @return Adjusted numeric score
#' @keywords internal
#' @noRd
.adjust_score_for_problem <- function(
    base_score,
    title,
    details,
    problem_text) {
  score <- base_score
  problem_lower <- tolower(problem_text)
  suggestion_text <- tolower(paste(title, details))

  # boost score if suggestion keywords match problem keywords
  # extract key terms from problem
  problem_keywords <- c()
  if (grepl("find|search|navigate|locate", problem_lower)) {
    problem_keywords <- c(problem_keywords, "navigation", "label", "scent")
  }
  if (grepl("overload|overwhelm|too many", problem_lower)) {
    problem_keywords <- c(problem_keywords, "limit", "progressive", "default")
  }
  if (grepl("mobile|touch|responsive", problem_lower)) {
    problem_keywords <- c(problem_keywords, "touch", "target", "responsive")
  }
  if (grepl("clutter|messy|disorganized", problem_lower)) {
    problem_keywords <- c(problem_keywords, "hierarchy", "group", "spacing")
  }

  # boost if keywords match
  if (length(problem_keywords) > 0) {
    matches <- sum(sapply(
      problem_keywords,
      function(k) grepl(k, suggestion_text, fixed = TRUE)
    ))
    if (matches > 0) {
      score <- score + (0.03 * matches)
    }
  }

  # ensure score stays within bounds
  score <- max(0, min(1, score))

  return(score)
}
