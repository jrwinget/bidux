# bidux 0.3.1 (2025-01-XX)
==========================

### NEW FEATURES

* **Hybrid telemetry objects for backward compatibility.** `bid_ingest_telemetry()` now returns a hybrid `bid_issues` object that behaves as a list (maintaining legacy compatibility) while providing enhanced functionality through new methods: `print.bid_issues()`, `as_tibble.bid_issues()`, and `bid_flags()`.

* **Modern tidy telemetry API.** New `bid_telemetry()` function provides a clean, tidy approach that returns organized issues tibbles (`bid_issues_tbl` class) for better integration with dplyr workflows and data analysis pipelines.

* **Bridge functions for telemetry-to-BID integration.** New functions seamlessly connect telemetry issues to BID stages:
  - `bid_notice_issue()` - Convert individual issue to Notice stage
  - `bid_notices()` - Batch process multiple issues to Notice stages
  - `bid_address()` - Sugar function for quick issue addressing
  - `bid_pipeline()` - Process first N issues with limits

* **Telemetry-informed structure selection.** `bid_structure()` now accepts `telemetry_flags` parameter to adjust layout selection and suggestion scoring based on real user behavior patterns (e.g., avoiding tabs layout when navigation issues are detected).

* **Enhanced validation with telemetry references.** `bid_validate()` now includes optional `telemetry_refs` parameter to append provenance information linking improvements back to specific telemetry findings.

### MAJOR FIXES

* **Corrected stage numbering.** Fixed stage progression to proper BID framework order: Interpret (1) → Notice (2) → Anticipate (3) → Structure (4) → Validate (5). Added temporary migration support with `stage_number_previous` attributes and session-level migration notices.

* **Consolidated suggestion system.** Unified suggestion rules for Interpret, Notice, and Validate stages in `R/suggest_rules.R`, reducing code duplication and ensuring consistent messaging across the framework.

* **Removed duplicate warning-suggestion pairs.** Cleaned up verbose output by preferring actionable suggestions over redundant warnings where content overlapped.

### IMPROVEMENTS

* **Enhanced Structure suggestions organization.** Consolidated structure-specific suggestion functions into `R/structure_suggest.R` with documented schema and improved concept-based grouping.

* **Improved telemetry flag extraction.** New `.flags_from_issues()` helper function creates comprehensive boolean flags from telemetry patterns for better layout and suggestion customization.

* **Better suggestion theory integration.** Factored out `.suggest_theory_from_text()` for reuse across Notice stage functions, improving consistency in theory recommendations.

* **Migration support utilities.** Added `.show_stage_numbering_notice()` and `.format_telemetry_refs_for_validation()` helper functions for smooth version migration.

### BREAKING CHANGES

* None

### DOCUMENTATION UPDATES

* **Updated Getting Started vignette** with new telemetry workflow examples and corrected stage numbering throughout.

* **Enhanced Telemetry Integration vignette** showcasing the new API with hybrid objects, bridge functions, and tidy workflow examples.

* **Comprehensive test coverage** for new bid_issues class methods, bridge functions, and edge cases.

### MIGRATION NOTES

For users upgrading from 0.3.0:

```r
# Legacy telemetry code continues to work
legacy_notices <- bid_ingest_telemetry("telemetry.sqlite")
length(legacy_notices)  # Still works as before

# But now also provides enhanced features
as_tibble(legacy_notices)  # New: get tidy issues view
bid_flags(legacy_notices)  # New: extract telemetry flags

# New modern API for tidy workflows
issues <- bid_telemetry("telemetry.sqlite")
critical <- issues |> filter(severity == "critical")
notices <- bid_notices(critical, previous_stage = interpret_result)
```

**Note:** `bid_ingest_telemetry()` will be soft-deprecated in 0.4.0 in favor of `bid_telemetry()`. Current usage remains fully supported.

### BUG FIXES

* **Fixed concept-based bias mitigation generation.** `bid_anticipate()` now correctly generates concept-specific biases (e.g., "attention bias" and "belief perseverance" for Visual Hierarchy) instead of falling back to generic biases.

* **Improved stage flow validation.** Updated validation logic to support the flexible BID workflow where inner stages (Notice, Anticipate, Structure) can occur in any order, while maintaining proper validation for entry and exit points.

* **Enhanced previous_stage handling.** Fixed validation errors when passing data.frame objects to BID functions by improving stage name extraction logic.

* Fixed stage progression validation warnings in tests by correcting test sequences to follow proper BID framework order
* Updated suggestion pattern matching in tests after DRY refactor consolidation

### DEPRECATIONS

* **Layout auto-selection is deprecated.** The layout selection feature in `bid_structure()` will be removed in bidux 0.4.0 to reduce complexity and focus on concept-based suggestions. Existing code continues to work with deprecation warnings.

* **Layout-specific bias mitigations are deprecated.** The layout-dependent bias mappings in `bid_anticipate()` will be removed in bidux 0.4.0 in favor of concept-driven bias mitigations. Existing code continues to work with deprecation warnings.

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

* **Enhanced `bid_validate()` with experiment/telemetry/empowerment flags.** New optional parameters: `include_exp_design` (default: TRUE), `include_telemetry` (default: TRUE), and `include_empower_tools` (default: TRUE) for context-aware suggestions.

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
* Complete implementation of the Behavioral Insight Design (BID) framework with 5 sequential stages: Notice, Interpret, Structure, Anticipate, and Validate.
* Added initial concept dictionary with 41+ behavioral science principles.
* Added UI component suggestions for major R packages: `{shiny}`, `{bslib}`, `{DT}`, `{plotly}`, `{reactable}`, `{htmlwidgets}`.
* Added multi-format reporting capabilities (text, HTML, markdown) with `bid_report()`.
* Added intelligent theory auto-suggestion system in `bid_notice()`.
* Added comprehensive validation and error handling across all functions.
* Added three detailed vignettes: "Getting Started", "Introduction to BID", and "Concepts Reference".
