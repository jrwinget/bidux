# Changelog

## \# bidux 0.3.2.9000 (development version)

#### NEW FEATURES

- **Flattened tibble format for Structure stage suggestions.**
  [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
  now returns both the nested suggestion format (`$suggestions`) and a
  new flattened tibble format (`$suggestions_tbl`). The tibble includes
  columns for `concept`, `title`, `details`, `components`, `rationale`,
  `score`, `difficulty`, and `category`, making it easy to filter and
  analyze suggestions using standard dplyr operations. Examples include
  filtering by difficulty (“Easy”, “Medium”, “Hard”), category
  (“Layout”, “Navigation”, “Content”, etc.), or score thresholds.

#### IMPROVEMENTS

- **Enhanced concept citations and accuracy in bid_concepts_data.csv:**
  - Updated Beautiful-Is-Good Stereotype with more comprehensive
    description and added aesthetic-usability reference (Tractinsky et
    al., 2000) alongside the original social psychology reference (Dion
    et al., 1972)
  - Improved Data Storytelling Framework with clearer description and
    updated citations to data visualization authorities (Knaflic, 2015;
    Dykes, 2020)
  - Added clarifying note to Gherkin Method explaining that while not a
    behavioral science concept, it supports clear information processing
    and scenario-based thinking

#### DOCUMENTATION UPDATES

- **Quarto dashboard compatibility.** Updated package documentation to
  clarify that bidux works with both Shiny applications and Quarto
  dashboards. Core BID framework stages, layout suggestions, and most
  component recommendations (`bslib`, `DT`, `plotly`, `reactable`) work
  in both frameworks. Added notes about which features require Shiny
  runtime when using Quarto (`server: shiny`).

- **Getting Started vignette enhanced** with comprehensive examples
  showing how to work with both nested and flattened suggestion formats,
  including practical filtering patterns for finding easy-to-implement,
  high-impact suggestions

## \# bidux 0.3.2 (2025-10-28)

CRAN release: 2025-10-28

#### NEW FEATURES

- **Flattened data_story API for improved ergonomics.**
  [`new_data_story()`](https://jrwinget.github.io/bidux/reference/new_data_story.md)
  now accepts flat arguments (`hook`, `context`, `tension`,
  `resolution`) making it more intuitive for the 80% use case. The
  nested format (`variables`, `relationships`) is still supported with
  deprecation warnings and will be removed in 0.4.0.

- **Telemetry sensitivity presets.** New
  [`bid_telemetry_presets()`](https://jrwinget.github.io/bidux/reference/bid_telemetry_presets.md)
  function provides pre-configured threshold sets (`strict`, `moderate`,
  `relaxed`) for easier telemetry analysis configuration without manual
  threshold tuning.

- **Quiet mode for cleaner output.** New `quiet` parameter added to all
  BID stage functions to suppress informational messages. Includes
  global configuration via
  [`bid_set_quiet()`](https://jrwinget.github.io/bidux/reference/bid_set_quiet.md),
  [`bid_get_quiet()`](https://jrwinget.github.io/bidux/reference/bid_get_quiet.md),
  and
  [`bid_with_quiet()`](https://jrwinget.github.io/bidux/reference/bid_with_quiet.md)
  for controlling message verbosity across entire workflows.

- **New S3 constructors for modern API.** Added
  [`new_user_personas()`](https://jrwinget.github.io/bidux/reference/new_user_personas.md)
  and
  [`new_bias_mitigations()`](https://jrwinget.github.io/bidux/reference/new_bias_mitigations.md)
  constructors to complement the S3 class system, providing type-safe
  alternatives to legacy list-based formats.

- **Enhanced print methods for S3 classes.** Improved
  [`print()`](https://rdrr.io/r/base/print.html) methods for
  `bid_data_story`, `bid_user_personas`, and `bid_bias_mitigations`
  classes provide clearer, more informative console output.

#### IMPROVEMENTS

- **Reduced package footprint.** Removed unused dependencies `purrr` and
  `stringr`, reducing install size and dependency overhead. Base R
  equivalents are used instead (e.g.,
  [`nchar()`](https://rdrr.io/r/base/nchar.html) instead of
  [`stringr::str_length()`](https://stringr.tidyverse.org/reference/str_length.html)).

- **Enhanced error handling with glue dependency.** Added `glue` package
  to dependencies for improved error message formatting. Migrated from
  [`stop()`](https://rdrr.io/r/base/stop.html) to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  with `standard_error_msg()` helper for more informative, context-rich
  error messages.

- **Text normalization improvements.** Enhanced text processing
  utilities with better handling of whitespace, special characters, and
  edge cases in suggestion generation and matching.

- **Code organization and modularity improvements.** Refactored
  utilities into domain-specific files (`utils_messaging.R`,
  `utils_validation.R`, `utils_stage.R`, `utils_safe_access.R`) for
  better maintainability and testability.

- **Cleaner telemetry constants.** Extracted all magic numbers from
  telemetry analysis functions to named constants at the top of
  `telemetry_analysis.R`, improving maintainability and making
  thresholds self-documenting.

- **Improved data.frame API documentation.** Updated examples and
  vignettes to show `data.frame` as the recommended API for
  `user_personas` and `bias_mitigations`, with clearer migration paths
  from legacy list format.

- **Backward-compatible enhancements.** All API changes maintain full
  backward compatibility through migration functions with helpful
  deprecation warnings guiding users to modern patterns.

#### BUG FIXES

- Removed duplicate utility functions (`%||%` operator and
  `validate_required_params()`) that were defined in multiple files.

#### DOCUMENTATION UPDATES

- **New API Modernization vignette** providing comprehensive guidance on
  migrating from legacy list-based APIs to modern S3 class system,
  including examples for `data_story`, `user_personas`, and
  `bias_mitigations`.

- **Simplified Advanced Workflows vignette** with clearer examples of
  complex BID pipeline patterns, quiet mode usage, and telemetry
  integration workflows.

- **Updated Getting Started vignette** to show recommended flat
  [`new_data_story()`](https://jrwinget.github.io/bidux/reference/new_data_story.md)
  API and `data.frame` format for personas.

- **Enhanced function examples** in
  [`bid_interpret()`](https://jrwinget.github.io/bidux/reference/bid_interpret.md)
  and other stage functions to demonstrate both modern and legacy
  formats.

- **Comprehensive test coverage** for new flat data_story API, backward
  compatibility with nested format, telemetry presets, safe accessor
  functions, and quiet mode functionality.

#### DEPRECATIONS

- **Nested data_story format (variables/relationships)** is deprecated
  and will be removed in bidux 0.4.0. Use the flat API instead:
  `new_data_story(hook, context, tension, resolution)`.

#### MIGRATION NOTES

For users upgrading from 0.3.1:

``` r
# OLD: Nested format (deprecated)
story <- new_data_story(
  context = "Dashboard usage dropped",
  variables = list(hook = "User engagement declining"),
  relationships = list(resolution = "Analyze telemetry")
)

# NEW: Flat format (recommended)
story <- new_data_story(
  hook = "User engagement declining",
  context = "Dashboard usage dropped 30%",
  tension = "Don't know if UX or user needs",
  resolution = "Analyze telemetry"
)

# Telemetry presets for easier configuration
issues <- bid_ingest_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("strict")  # or "moderate", "relaxed"
)
```

**Note:** All legacy formats continue to work with deprecation warnings.
See function documentation for detailed migration guidance.

## \# bidux 0.3.1 (2025-09-07)

CRAN release: 2025-09-07

#### NEW FEATURES

- **Hybrid telemetry objects for backward compatibility.**
  [`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
  now returns a hybrid `bid_issues` object that behaves as a list
  (maintaining legacy compatibility) while providing enhanced
  functionality through new methods:
  [`print.bid_issues()`](https://jrwinget.github.io/bidux/reference/print.bid_issues.md),
  [`as_tibble.bid_issues()`](https://jrwinget.github.io/bidux/reference/as_tibble.bid_issues.md),
  and
  [`bid_flags()`](https://jrwinget.github.io/bidux/reference/bid_flags.md).

- **Modern tidy telemetry API.** New
  [`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
  function provides a clean, tidy approach that returns organized issues
  tibbles (`bid_issues_tbl` class) for better integration with dplyr
  workflows and data analysis pipelines.

- **Bridge functions for telemetry-to-BID integration.** New functions
  seamlessly connect telemetry issues to BID stages:

  - [`bid_notice_issue()`](https://jrwinget.github.io/bidux/reference/bid_notice_issue.md) -
    Convert individual issue to Notice stage
  - [`bid_notices()`](https://jrwinget.github.io/bidux/reference/bid_notices.md) -
    Batch process multiple issues to Notice stages
  - [`bid_address()`](https://jrwinget.github.io/bidux/reference/bid_address.md) -
    Sugar function for quick issue addressing
  - [`bid_pipeline()`](https://jrwinget.github.io/bidux/reference/bid_pipeline.md) -
    Process first N issues with limits

- **Telemetry-informed structure selection.**
  [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
  now accepts `telemetry_flags` parameter to adjust layout selection and
  suggestion scoring based on real user behavior patterns (e.g.,
  avoiding tabs layout when navigation issues are detected).

- **Enhanced validation with telemetry references.**
  [`bid_validate()`](https://jrwinget.github.io/bidux/reference/bid_validate.md)
  now includes optional `telemetry_refs` parameter to append provenance
  information linking improvements back to specific telemetry findings.

#### MAJOR FIXES

- **Corrected stage numbering.** Fixed stage progression to proper BID
  framework order: Interpret (1) → Notice (2) → Anticipate (3) →
  Structure (4) → Validate (5). Added temporary migration support with
  `stage_number_previous` attributes and session-level migration
  notices.

- **Consolidated suggestion system.** Unified suggestion rules for
  Interpret, Notice, and Validate stages in `R/suggest_rules.R`,
  reducing code duplication and ensuring consistent messaging across the
  framework.

- **Removed duplicate warning-suggestion pairs.** Cleaned up verbose
  output by preferring actionable suggestions over redundant warnings
  where content overlapped.

#### IMPROVEMENTS

- **Enhanced Structure suggestions organization.** Consolidated
  structure-specific suggestion functions into `R/structure_suggest.R`
  with documented schema and improved concept-based grouping.

- **Improved telemetry flag extraction.** New
  [`.flags_from_issues()`](https://jrwinget.github.io/bidux/reference/dot-flags_from_issues.md)
  helper function creates comprehensive boolean flags from telemetry
  patterns for better layout and suggestion customization.

- **Better suggestion theory integration.** Factored out
  [`.suggest_theory_from_text()`](https://jrwinget.github.io/bidux/reference/dot-suggest_theory_from_text.md)
  for reuse across Notice stage functions, improving consistency in
  theory recommendations.

- **Migration support utilities.** Added
  `.show_stage_numbering_notice()` and
  `.format_telemetry_refs_for_validation()` helper functions for smooth
  version migration.

#### BREAKING CHANGES

- None

#### DOCUMENTATION UPDATES

- **Updated Getting Started vignette** with new telemetry workflow
  examples and corrected stage numbering throughout.

- **Enhanced Telemetry Integration vignette** showcasing the new API
  with hybrid objects, bridge functions, and tidy workflow examples.

- **Comprehensive test coverage** for new bid_issues class methods,
  bridge functions, and edge cases. Also, expanded coverage for
  previously missed lines across bid_anticipate, bid_concepts,
  suggest_rules, and utility functions.

#### MIGRATION NOTES

For users upgrading from 0.3.0:

``` r
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

**Note:**
[`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
will be soft-deprecated in 0.4.0 in favor of
[`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md).
Current usage remains fully supported.

#### BUG FIXES

- **Fixed concept-based bias mitigation generation.**
  [`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md)
  now correctly generates concept-specific biases (e.g., “attention
  bias” and “belief perseverance” for Visual Hierarchy) instead of
  falling back to generic biases.

- **Improved stage flow validation.** Updated validation logic to
  support the flexible BID workflow where inner stages (Notice,
  Anticipate, Structure) can occur in any order, while maintaining
  proper validation for entry and exit points.

- **Enhanced previous_stage handling.** Fixed validation errors when
  passing data.frame objects to BID functions by improving stage name
  extraction logic.

- Fixed stage progression validation warnings in tests by correcting
  test sequences to follow proper BID framework order

- Updated suggestion pattern matching in tests after DRY refactor
  consolidation

#### DEPRECATIONS

- **Layout auto-selection is deprecated.** The layout selection feature
  in
  [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
  will be removed in bidux 0.4.0 to reduce complexity and focus on
  concept-based suggestions. Existing code continues to work with
  deprecation warnings.

- **Layout-specific bias mitigations are deprecated.** The
  layout-dependent bias mappings in
  [`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md)
  will be removed in bidux 0.4.0 in favor of concept-driven bias
  mitigations. Existing code continues to work with deprecation
  warnings.

## \# bidux 0.3.0 (2025-08-29)

CRAN release: 2025-08-29

#### BREAKING CHANGES

- **BID stage order updated to: Interpret → Notice → Anticipate →
  Structure → Validate.** This reflects the most natural workflow while
  maintaining flexibility for iterative usage.

- **[`bid_notice()`](https://jrwinget.github.io/bidux/reference/bid_notice.md)
  no longer accepts `target_audience` parameter.** Audience information
  should now be managed through the
  [`bid_interpret()`](https://jrwinget.github.io/bidux/reference/bid_interpret.md)
  stage using the `data_story` or `user_personas` parameters. The
  function will warn if the deprecated parameter is provided.

- **[`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md)
  no longer accepts `interaction_principles` parameter.** This parameter
  has been removed in favor of the new `include_accessibility` parameter
  (default: TRUE). The function will warn if the deprecated parameter is
  provided.

- **Field name changes for consistency:**

  - `previous_question` → `previous_central_question`
  - `previous_story_hook` → `previous_hook`
  - `user_personas` → `personas` (in bid_interpret output)

- **[`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
  no longer accepts a `layout` parameter.** Layout is now automatically
  selected based on content analysis of previous stages using
  deterministic heuristics. The function will abort with a helpful error
  message if the deprecated `layout` parameter is provided.

#### NEW FEATURES

- **Enhanced
  [`bid_validate()`](https://jrwinget.github.io/bidux/reference/bid_validate.md)
  with experiment/telemetry/empowerment flags.** New optional
  parameters: `include_exp_design` (default: TRUE), `include_telemetry`
  (default: TRUE), and `include_empower_tools` (default: TRUE) for
  context-aware suggestions.

- **Accessibility-focused bias mitigation in
  [`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md).**
  New `include_accessibility` parameter adds layout-specific
  accessibility recommendations to bias mitigation strategies.

- **Improved context propagation.** All `previous_problem`,
  `previous_theory`, `previous_audience`, and `previous_personas` now
  properly propagate through the entire pipeline to the Validate stage.

- **Automatic layout inference for
  [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md).**
  Uses sophisticated heuristics to analyze content from previous BID
  stages (problem, evidence, data story elements) and automatically
  select the most appropriate layout (breathable, dual_process, grid,
  card, or tabs).

- **Ranked, concept-grouped actionable suggestions.**
  [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
  now returns structured UI/UX recommendations organized by UX concepts
  (e.g., Cognitive Load Theory, Progressive Disclosure) with specific
  Shiny/bslib component pointers, implementation rationales, and
  relevance scores (0-1).

- **Telemetry-aware suggestion scoring.** Layout selection and
  suggestion ranking now consider telemetry data, automatically avoiding
  problematic patterns (e.g., avoiding tabs if `nav_dropoff_tabs` is
  detected).

- **Enhanced CLI feedback.** Clear messages show the auto-selected
  layout with rationale, plus a helpful tip about using
  `bid_concept("<concept>")` to learn more about any detected concepts.

#### IMPROVEMENTS

- **Field name normalization.** Added `normalize_previous_stage()`
  helper function to handle legacy field names and ensure consistent
  data flow between stages.

- **Enhanced helper functions.** Updated utility functions with time
  stubbing support (`.now()`) and improved safe column access patterns.

- **Comprehensive test coverage.** Added extensive tests for all
  heuristic branches, suggestion structure validation, CLI message
  verification, telemetry integration, and error handling for the
  deprecated `layout` parameter.

#### BUG FIXES

- None.

#### MIGRATION GUIDE

To update existing code for bidux 0.3.0:

``` r
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

#### DEPRECATED AND DEFUNCT

- `target_audience` parameter in
  [`bid_notice()`](https://jrwinget.github.io/bidux/reference/bid_notice.md)
  (deprecated, will be removed in 0.4.0)
- `interaction_principles` parameter in
  [`bid_anticipate()`](https://jrwinget.github.io/bidux/reference/bid_anticipate.md)
  (deprecated, will be removed in 0.4.0)

#### DOCUMENTATION FIXES

- Updated function documentation to reflect parameter changes and new
  stage ordering
- Updated examples to use new recommended workflow: Interpret → Notice →
  Anticipate → Structure → Validate

## \# bidux 0.2.0 (2025-08-05)

#### NEW FEATURES

- **Telemetry-driven UX friction detection.** New
  [`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
  ingests [shiny.telemetry](https://appsilon.github.io/shiny.telemetry/)
  data (SQLite or JSON), applies configurable thresholds, detects
  friction patterns (e.g., unused inputs, delayed interactions, error
  clusters, navigation drop-offs, confusion), and generates BID notices
  and reports. Includes robust input validation, format auto-detection,
  and clearer CLI summaries. (#18)

- **Telemetry integration vignette.** A new vignette shows how to set up
  [shiny.telemetry](https://appsilon.github.io/shiny.telemetry/), run
  [`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md),
  interpret indicators, and customize thresholds within the BID
  workflow. (#18)

#### MINOR IMPROVEMENTS

- **Modular printing and display rules.**
  [`print.bid_stage()`](https://jrwinget.github.io/bidux/reference/print.bid_stage.md)
  refactored into `print_stage_header()`, `print_stage_content()`, and
  `print_stage_footer()` with reusable field printing and stage-specific
  display rules for clearer, more consistent output.

- **Consistent data loading.** Introduced `load_external_data()`;
  concept, theory, bias, layout, and accessibility loaders now share one
  code path with sensible fallbacks.

- **Suggestion generation & validation utilities.** Consolidated
  helpers: `generate_stage_suggestions()`,
  `get_default_suggestion_rules()`, and
  `evaluate_suggestion_condition()`; standardized parameter checks with
  `validate_character_param()`, `validate_list_param()`, and
  `validate_bid_stage_params()`.

- **Stronger parsing & normalization.** Telemetry JSON handling now
  supports both JSON arrays and JSON-lines; column normalization
  validates and standardizes inputs; percentage/label formatting made
  more consistent.

- **Safety & style passes.** Safer subsetting (`drop = FALSE`), safer
  indexing ([`seq_len()`](https://rdrr.io/r/base/seq.html)), tightened
  conditionals/bracing and indentation, aligned function signatures and
  lists, and general formatting cleanup for readability and
  maintainability.

#### BUG FIXES

- None.

#### DEPRECATED AND DEFUNCT

- None.

#### DOCUMENTATION FIXES

- **Docs & guides.** Added **CONTRIBUTING** guidelines and a **Code of
  Conduct**; standardized GitHub issue templates for clear, consistent
  reports. (#16)

- **README overhaul.** Reorganized overview, features, installation, and
  a concise quick start; added a telemetry section; improved examples;
  suppressed noisy messages during README generation; added a CRAN
  downloads badge. (#15, \#16)

- **Reference updates.** Exported and documented
  [`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md);
  refreshed `man/` pages and package index to reflect new modules and
  function locations. (#18)

#### REFACTORING & INTERNALS (no user-facing API changes)

- **File organization.** Standardized dash-separated filenames (e.g.,
  `bid-classes.R`, `bid-concepts.R`); centralized telemetry
  ingestion/analysis/notice creation into `telemetry.R`; moved shared
  helpers into `utils.R`.

- **Namespace & dependencies.** Added `DBI` and `RSQLite` to `Imports`,
  and `importFrom(stats, complete.cases)`; relaxed version constraints
  for `cli`, `dplyr`, and `jsonlite`; expanded `NAMESPACE` with explicit
  imports.

## \# bidux 0.1.0 (2025-06-16)

CRAN release: 2025-06-13

- Initial CRAN submission.
- Complete implementation of the Behavioral Insight Design (BID)
  framework with 5 sequential stages: Notice, Interpret, Structure,
  Anticipate, and Validate.
- Added initial concept dictionary with 41+ behavioral science
  principles.
- Added UI component suggestions for major R packages:
  [shiny](https://shiny.posit.co/),
  [bslib](https://rstudio.github.io/bslib/),
  [DT](https://github.com/rstudio/DT), [plotly](https://plotly-r.com),
  [reactable](https://glin.github.io/reactable/),
  [htmlwidgets](https://github.com/ramnathv/htmlwidgets).
- Added multi-format reporting capabilities (text, HTML, markdown) with
  [`bid_report()`](https://jrwinget.github.io/bidux/reference/bid_report.md).
- Added intelligent theory auto-suggestion system in
  [`bid_notice()`](https://jrwinget.github.io/bidux/reference/bid_notice.md).
- Added comprehensive validation and error handling across all
  functions.
- Added three detailed vignettes: “Getting Started”, “Introduction to
  BID”, and “Concepts Reference”.
