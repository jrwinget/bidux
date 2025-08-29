# bidux 0.3.0 (2025-08-29)
==========================

### BREAKING CHANGES

* **BID stage order updated to: Interpret → Notice → Anticipate → Structure → Validate.** This reflects the most natural workflow while maintaining flexibility for iterative usage.

* **`bid_notice()` no longer accepts `target_audience` parameter.** Audience information should now be managed through the `bid_interpret()` stage using the `data_story` or `user_personas` parameters. The function will warn if the deprecated parameter is provided.

* **`bid_anticipate()` no longer accepts `interaction_principles` parameter.** This parameter has been removed in favor of the new `include_accessibility` parameter (default: TRUE). The function will warn if the deprecated parameter is provided.

* **Field name changes for consistency:**
  - `previous_question` → `previous_central_question` 
  - `previous_story_hook` → `previous_hook`
  - `user_personas` → `personas` (in bid_interpret output)

* **`bid_structure()` no longer accepts a `layout` parameter.** Layout is now automatically selected based on content analysis of previous stages using deterministic heuristics. The function will abort with a helpful error message if the deprecated `layout` parameter is provided.

### NEW FEATURES

* **Enhanced `bid_validate()` with experiment/telemetry/empowerment flags.** New optional parameters: `include_exp_design` (default: TRUE), `include_telemetry` (default: TRUE), and `include_empowerment_tools` (default: TRUE) for context-aware suggestions.

* **Accessibility-focused bias mitigation in `bid_anticipate()`.** New `include_accessibility` parameter adds layout-specific accessibility recommendations to bias mitigation strategies.

* **Improved context propagation.** All `previous_problem`, `previous_theory`, `previous_audience`, and `previous_personas` now properly propagate through the entire pipeline to the Validate stage.

* **Automatic layout inference for `bid_structure()`.** Uses sophisticated heuristics to analyze content from previous BID stages (problem, evidence, data story elements) and automatically select the most appropriate layout (breathable, dual_process, grid, card, or tabs).

* **Ranked, concept-grouped actionable suggestions.** `bid_structure()` now returns structured UI/UX recommendations organized by UX concepts (e.g., Cognitive Load Theory, Progressive Disclosure) with specific Shiny/bslib component pointers, implementation rationales, and relevance scores (0-1).

* **Telemetry-aware suggestion scoring.** Layout selection and suggestion ranking now consider telemetry data, automatically avoiding problematic patterns (e.g., avoiding tabs if `nav_dropoff_tabs` is detected).

* **Enhanced CLI feedback.** Clear messages show the auto-selected layout with rationale, plus a helpful tip about using `bid_concept("<concept>")` to learn more about any detected concepts.

### IMPROVEMENTS

* **Field name normalization.** Added `normalize_previous_stage()` helper function to handle legacy field names and ensure consistent data flow between stages.

* **Enhanced helper functions.** Updated utility functions with time stubbing support (`.now()`) and improved safe column access patterns.

* **Comprehensive test coverage.** Added extensive tests for all heuristic branches, suggestion structure validation, CLI message verification, telemetry integration, and error handling for the deprecated `layout` parameter.

### BUG FIXES

* None.

### MIGRATION GUIDE

To update existing code for bidux 0.3.0:

```r
# OLD (0.2.x)
notice <- bid_notice(
  problem = "Users confused", 
  evidence = "High error rate",
  target_audience = "Analysts"
)

# NEW (0.3.0)
notice <- bid_notice(
  problem = "Users confused", 
  evidence = "High error rate"
)

interpret <- bid_interpret(
  previous_stage = notice,
  data_story = list(
    audience = "Analysts"  # Move audience here
  )
)

# OLD (0.2.x)
anticipate <- bid_anticipate(
  previous_stage = structure_result,
  interaction_principles = list(hover = "Show details on hover")
)

# NEW (0.3.0)
anticipate <- bid_anticipate(
  previous_stage = structure_result,
  include_accessibility = TRUE  # New accessibility focus
)

# Field name updates in downstream code:
# Access previous_central_question instead of previous_question
# Access previous_hook instead of previous_story_hook
```

### DEPRECATED AND DEFUNCT

* `target_audience` parameter in `bid_notice()` (deprecated, will be removed in 0.4.0)
* `interaction_principles` parameter in `bid_anticipate()` (deprecated, will be removed in 0.4.0)

### DOCUMENTATION FIXES

* Updated function documentation to reflect parameter changes and new stage ordering
* Updated examples to use new recommended workflow: Interpret → Notice → Anticipate → Structure → Validate

# bidux 0.2.0 (2025-08-05)
==========================

### NEW FEATURES

* **Telemetry-driven UX friction detection.** New `bid_ingest_telemetry()` ingests `{shiny.telemetry}` data (SQLite or JSON), applies configurable thresholds, detects friction patterns (e.g., unused inputs, delayed interactions, error clusters, navigation drop-offs, confusion), and generates BID notices and reports. Includes robust input validation, format auto-detection, and clearer CLI summaries. (#18)

* **Telemetry integration vignette.** A new vignette shows how to set up `{shiny.telemetry}`, run `bid_ingest_telemetry()`, interpret indicators, and customize thresholds within the BID workflow. (#18)

### MINOR IMPROVEMENTS

* **Modular printing and display rules.** `print.bid_stage()` refactored into `print_stage_header()`, `print_stage_content()`, and `print_stage_footer()` with reusable field printing and stage-specific display rules for clearer, more consistent output.

* **Consistent data loading.** Introduced `load_external_data()`; concept, theory, bias, layout, and accessibility loaders now share one code path with sensible fallbacks.

* **Suggestion generation & validation utilities.** Consolidated helpers: `generate_stage_suggestions()`, `get_default_suggestion_rules()`, and `evaluate_suggestion_condition()`; standardized parameter checks with `validate_character_param()`, `validate_list_param()`, and `validate_bid_stage_params()`.

* **Stronger parsing & normalization.** Telemetry JSON handling now supports both JSON arrays and JSON-lines; column normalization validates and standardizes inputs; percentage/label formatting made more consistent.

* **Safety & style passes.** Safer subsetting (`drop = FALSE`), safer indexing (`seq_len()`), tightened conditionals/bracing and indentation, aligned function signatures and lists, and general formatting cleanup for readability and maintainability.

### BUG FIXES

* None.

### DEPRECATED AND DEFUNCT

* None.

### DOCUMENTATION FIXES

* **Docs & guides.** Added **CONTRIBUTING** guidelines and a **Code of Conduct**; standardized GitHub issue templates for clear, consistent reports. (#16)

* **README overhaul.** Reorganized overview, features, installation, and a concise quick start; added a telemetry section; improved examples; suppressed noisy messages during README generation; added a CRAN downloads badge. (#15, #16)

* **Reference updates.** Exported and documented `bid_ingest_telemetry()`; refreshed `man/` pages and package index to reflect new modules and function locations. (#18)

### REFACTORING & INTERNALS (no user-facing API changes)

* **File organization.** Standardized dash-separated filenames (e.g., `bid-classes.R`, `bid-concepts.R`); centralized telemetry ingestion/analysis/notice creation into `telemetry.R`; moved shared helpers into `utils.R`.

* **Namespace & dependencies.** Added `DBI` and `RSQLite` to `Imports`, and `importFrom(stats, complete.cases)`; relaxed version constraints for `cli`, `dplyr`, and `jsonlite`; expanded `NAMESPACE` with explicit imports.

# bidux 0.1.0 (2025-06-16)
==========================

* Initial CRAN submission.
* Complete implementation of the Behavior Insight Design (BID) framework with 5 sequential stages: Notice, Interpret, Structure, Anticipate, and Validate.
* Added initial concept dictionary with 41+ behavioral science principles.
* Added UI component suggestions for major R packages: `{shiny}`, `{bslib}`, `{DT}`, `{plotly}`, `{reactable}`, `{htmlwidgets}`.
* Added multi-format reporting capabilities (text, HTML, markdown) with `bid_report()`.
* Added intelligent theory auto-suggestion system in `bid_notice()`.
* Added comprehensive validation and error handling across all functions.
* Added three detailed vignettes: "Getting Started", "Introduction to BID", and "Concepts Reference".
