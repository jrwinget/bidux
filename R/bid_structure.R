#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard with automatic layout
#' selection and generates ranked, concept-grouped actionable UI/UX suggestions.
#' Layout is intelligently chosen based on content analysis of previous stages
#' using deterministic heuristics. Returns structured recommendations with
#' specific component pointers and implementation rationales.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#'        function.
#' @param concepts A character vector of additional BID concepts to include.
#'        Concepts can be provided in natural language (e.g., "Principle of
#'        Proximity") or with underscores (e.g., "principle_of_proximity"). The
#'        function uses fuzzy matching to identify the concepts. If NULL, will
#'        detect relevant concepts from previous stages automatically.
#' @param telemetry_flags Optional named list of telemetry flags from bid_flags().
#'        Used to adjust layout choice and suggestion scoring based on observed
#'        user behavior patterns.
#' @param quiet Logical indicating whether to suppress informational messages.
#'        If NULL, uses getOption("bidux.quiet", FALSE).
#' @param ... Additional parameters. If `layout` is provided via `...`, the
#'        function will abort with a helpful error message.
#'
#' @return A bid_stage object containing:
#'   \item{stage}{"Structure"}
#'   \item{layout}{Auto-selected layout type}
#'   \item{suggestions}{List of concept groups with ranked suggestions (nested format)}
#'   \item{suggestions_tbl}{Flattened tibble with all suggestions, includes columns:
#'     concept, title, details, components, rationale, score, difficulty, category}
#'   \item{concepts}{Comma-separated string of all concepts used}
#'
#' @details
#' **Layout Auto-Selection**: For backwards compatibility with versions < 0.3.0;
#' to be removed in 0.4.0. Uses deterministic heuristics to analyze content
#' from previous stages and select the most appropriate layout:
#' - **breathable**: For information overload/confusion patterns
#' - **dual_process**: For overview vs detail needs
#' - **grid**: For grouping/comparison requirements
#' - **card**: For modular/chunked content
#' - **tabs**: For categorical organization (unless telemetry shows issues)
#'
#' **Suggestion Engine**: Generates ranked, actionable recommendations grouped
#' by UX concepts. Each suggestion includes specific R dashboard components
#' (Shiny, bslib, DT, plotly, etc.), implementation details, and rationale.
#' Suggestions are scored based on relevance, layout appropriateness, and
#' contextual factors. Component suggestions work with both Shiny applications
#' and Quarto dashboards, with shiny-prefixed components (i.e., `shiny::`)
#' requiring Shiny runtime.
#'
#' @examples
#' notice_result <- bid_interpret(
#'   central_question = "How can we simplify data presentation?",
#'   data_story = list(
#'     hook = "Data is too complex",
#'     context = "Overloaded with charts",
#'     tension = "Confusing layout",
#'     resolution = "Introduce clear grouping"
#'   )
#' ) |>
#'   bid_notice(
#'     problem = "Users struggle with information overload",
#'     evidence = "Survey results indicate delays"
#'   )
#'
#' # Auto-selected layout with concept-grouped suggestions
#' structure_result <- bid_structure(previous_stage = notice_result)
#' print(structure_result$layout) # Auto-selected layout
#' print(structure_result$suggestions) # Ranked suggestions by concept (nested)
#'
#' # Access flattened tibble format for easier manipulation
#' suggestions_flat <- structure_result$suggestions_tbl[[1]]
#' print(suggestions_flat)
#'
#' # Filter by difficulty
#' easy_suggestions <- suggestions_flat[suggestions_flat$difficulty == "Easy", ]
#'
#' # Filter by category
#' layout_suggestions <- suggestions_flat[suggestions_flat$category == "Layout", ]
#'
#' summary(structure_result)
#'
#' @export
bid_structure <- function(
    previous_stage,
    concepts = NULL,
    telemetry_flags = NULL,
    quiet = NULL,
    ...) {
  # check for deprecated layout parameter
  dots <- list(...)
  if ("layout" %in% names(dots)) {
    cli::cli_abort(c(
      "x" = "`layout` parameter was removed in bidux 0.2.0.",
      "i" = "Layout is now auto-selected based on previous stage content.",
      "i" = "Remove the `layout` argument to use automatic selection."
    ))
  }

  validate_required_params(previous_stage = previous_stage)
  validate_previous_stage(previous_stage, "Structure")

  chosen_layout <- suggest_layout_from_previous(previous_stage, telemetry_flags)

  bid_alert_info(glue::glue("Auto-selected layout: {chosen_layout}"), quiet = quiet)
  bid_alert_info(layout_rationale(previous_stage, chosen_layout), quiet = quiet)

  # issue deprecation warning once per session (skip in tests to reduce noise)
  # use package namespace instead of global environment for CRAN compliance
  pkg_env <- asNamespace("bidux")
  if (
    !exists(".bid_layout_selection_warned", envir = pkg_env) &&
      !identical(Sys.getenv("TESTTHAT"), "true")
  ) {
    warning(
      "Layout auto-selection is deprecated and will be removed in bidux 0.4.0. ",
      "The BID framework will focus on concept-based suggestions instead. ",
      "Existing code will continue to work until 0.4.0.",
      call. = FALSE
    )
    try(
      assign(".bid_layout_selection_warned", TRUE, envir = pkg_env),
      silent = TRUE
    )
  }

  # generate ranked, concept-grouped suggestions
  suggestion_groups <- structure_suggestions(
    previous_stage,
    chosen_layout,
    concepts,
    quiet = quiet
  )

  concepts_detected <- sapply(suggestion_groups, function(g) g$concept)
  if (length(concepts_detected) == 0) {
    concepts_detected <- character(0)
  }

  # create flattened tibble version of suggestions
  suggestions_tbl <- flatten_suggestions_to_tibble(suggestion_groups)

  normalized_previous <- normalize_previous_stage(previous_stage)

  # prepare result data
  result_data <- tibble::tibble(
    stage = "Structure",
    layout = chosen_layout,
    concepts = paste(concepts_detected, collapse = ", "),
    previous_central_question = safe_column_access(
      normalized_previous,
      "central_question"
    ),
    previous_hook = safe_column_access(normalized_previous, "hook"),
    previous_problem = safe_column_access(normalized_previous, "problem"),
    previous_theory = safe_column_access(normalized_previous, "theory"),
    previous_audience = get_audience_from_previous(normalized_previous),
    previous_personas = get_personas_from_previous(normalized_previous),
    previous_bias = safe_column_access(normalized_previous, "bias_mitigations"),
    suggestions = suggestion_groups,
    timestamp = .now()
  )

  # add suggestions_tbl as a list column (after tibble creation to avoid issues)
  result_data$suggestions_tbl <- list(suggestions_tbl)

  metadata <- list(
    layout_type = chosen_layout,
    auto_selected_layout = TRUE,
    concepts_count = length(concepts_detected),
    suggestion_groups_count = length(suggestion_groups),
    stage_number = 4,
    stage_number_previous = 3, # migration support for 0.3.1
    total_stages = 5
  )

  result <- bid_stage("Structure", result_data, metadata)

  # add session-level migration notice (once per session)
  .show_stage_numbering_notice()

  bid_message(
    "Stage 4 (Structure) completed.",
    glue::glue("Auto-selected layout: {chosen_layout}"),
    glue::glue("Concept groups generated: {length(suggestion_groups)}"),
    glue::glue("Total concepts: {length(concepts_detected)}"),
    quiet = quiet
  )

  return(result)
}
