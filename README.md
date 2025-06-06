
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bidux <a href="https://github.com/jrwinget/bid-framework"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bidux)](https://cran.r-project.org/package=bidux)
[![R-CMD-check](https://github.com/jrwinget/bidux/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrwinget/bidux/actions/workflows/R-CMD-check.yaml)
[![code-size](https://img.shields.io/github/languages/code-size/jrwinget/bidux)](https://github.com/jrwinget/bidux)
[![Codecov test
coverage](https://codecov.io/gh/jrwinget/bidux/graph/badge.svg)](https://app.codecov.io/gh/jrwinget/bidux)
<!-- badges: end -->

The `{bidux}` package helps Shiny developers implement the Behavior
Insight Design (BID) framework in their workflow. BID is a 5-stage
process that incorporates psychological principles into UI/UX design:

1.  **Notice** the Problem - Identify friction points using principles
    of cognitive load and visual hierarchies
2.  **Interpret** the User’s Need - Create compelling data stories and
    define user personas
3.  **Structure** the Dashboard - Apply layout patterns and
    accessibility considerations
4.  **Anticipate** User Behavior - Mitigate cognitive biases and
    implement effective interaction hints
5.  **Validate** & Empower the User - Provide summary insights and
    collaborative features

This structured approach helps developers create more intuitive,
user-friendly dashboards by systematically applying behavioral
psychology concepts to their design process.

## Installation

You can install the development version of {bidux} from GitHub:

``` r
# install.packages("pak")
pak::pak("jrwinget/bidux")
```

## Core Functions

The package provides documentation functions for each stage of the BID
process:

``` r
library(bidux)

# Stage 1: Notice the Problem
notice_result <- bid_notice(
  problem = "Users can't find the most important metrics",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics",
  theory = "Visual Hierarchies",  # Optional - will be suggested if omitted
  target_audience = "Data analysts with varying technical skills"
)
#> Stage 1 (Notice) completed. (20% complete)
#>   - Problem: Users can't find the most important metrics
#>   - Theory: Visual Hierarchies
#>   - Evidence: User testing showed 70% of users spending >30s looking fo...
#>   - Next: Use bid_interpret() for Stage 2

# Stage 2: Interpret the User's Need
interpret_result <- bid_interpret(
  previous_stage = notice_result,
  central_question = "How are sales trending against targets?",
  data_story = list(
    hook = "Sales are trending below target in Q2",
    context = "Previous quarters exceeded targets",
    tension = "What's causing the Q2 decline?",
    resolution = "Identify underperforming product categories",
    audience = "Marketing team",
    metrics = c("YTD Sales", "Quarter Growth Rate", "Regional Performance"),
    visual_approach = "Comparative visualization with clear color coding"
  ),
  user_personas = list(
    list(
      name = "Sales Manager",
      goals = "Track team performance",
      pain_points = "Too much data to sift through",
      technical_level = "Intermediate"
    )
  )
)
#> Stage 2 (Interpret) completed.
#>   - Central question: How are sales trending against targets?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - User personas: 1 defined

# Stage 3: Structure the Dashboard
structure_result <- bid_structure(
  previous_stage = interpret_result,
  layout = "dual_process",  # Options: dual_process, grid, card, tabs, breathable
  concepts = c("principle_of_proximity", "default_effect", "breathable_layouts"),
  accessibility = list(
    color_contrast = "WCAG AA compliant",
    keyboard_navigation = "All elements focusable",
    screen_reader = "Charts include descriptive alt text"
  )
)
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Returning all 41 concepts
#> 
#> ℹ Consider adding these common accessibility parameters:- text_size- alternative_text- focus_indicators- semantic_markup- aria_labels
#> 
#> 
#> 
#> ── Implementation tips for selected concepts: 
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> • Principle of Proximity: Place related controls and visualizations in
#> proximity to each other.
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> • Default Effect: Pre-select the most useful timeframe or metrics for initial
#> view.
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> • Breathable Layouts: Use bslib::card_body(padding = 4) to add consistent
#> padding and whitespace.
#> Stage 3 (Structure) completed.
#>   - Layout: dual_process
#>   - Concepts: Principle of Proximity, Default Effect, Breathable Layouts
#>   - Accessibility considerations included: color_contrast, keyboard_navigation, screen_reader

# Stage 4: Anticipate User Behavior
anticipate_result <- bid_anticipate(
  previous_stage = structure_result,
  bias_mitigations = list(
    anchoring = "Using context-aware reference points",
    framing = "Providing both positive and negative framings",
    confirmation_bias = "Including alternative scenarios"
  ),
  interaction_principles = list(
    hover_effects = "Show details on hover",
    selection_feedback = "Highlight active filters",
    progressive_disclosure = "Reveal advanced options progressively"
  )
)
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found 7 concept(s) matching 'bias|anchor|fram|confirm'
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found partial match: Anchoring Effect
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found partial match: Framing & Loss Aversion
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Concept 'confirmation_bias' not found
#> Stage 4 (Anticipate) completed.
#>   - Bias mitigations: 3 defined
#>   - Interaction principles: 3 defined
#>   - Key suggestions: anchoring mitigation: Always show reference points like previous period, budget, or industry average., framing mitigation: Toggle between progress (65% complete) and gap (35% remaining) framing., confirmation_bias mitigation: Consider how this bias affects user decisions.

# Stage 5: Validate & Empower
validate_result <- bid_validate(
  previous_stage = anticipate_result,
  summary_panel = "Key insights panel with actionable takeaways",
  collaboration = "Team annotation and sharing capabilities",
  next_steps = c(
    "Review underperforming categories",
    "Schedule team discussion",
    "Update forecast models"
  )
)
#> Stage 5 (Validate) completed.
#>   - Summary panel: Key insights panel with actionable takeaways
#>   - Collaboration: Team annotation and sharing capabilities
#>   - Next steps: 3 items defined
#>   - Include user testing in your next steps
```

## Concept Dictionary

The package includes a comprehensive dictionary of behavioral psychology
concepts:

``` r
# List all concepts
all_concepts <- bid_concepts()
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Returning all 41 concepts

# Search for specific concepts
cognitive_concepts <- bid_concepts("cognitive")
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found 4 concept(s) matching 'cognitive'
visual_concepts <- bid_concepts("visual, hierarchy") # Multiple search terms
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found 11 concept(s) matching 'visual, hierarchy'

# Get detailed information about a specific concept
anchoring_info <- bid_concept("anchoring effect")
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## UI Component Suggestions

Get concrete implementation ideas for various UI packages:

``` r
# Get bslib component suggestions
bslib_components <- bid_suggest_components(structure_result, package = "bslib")
#> ✔ Found 10 bslib component suggestion for BID Structure stage

# Get shiny component suggestions
shiny_components <- bid_suggest_components(notice_result, package = "shiny")
#> ✔ Found 10 shiny component suggestion for BID Notice stage

# Get reactable component suggestions
reactable_components <- bid_suggest_components(anticipate_result, package = "reactable")
#> ✔ Found 1 reactable component suggestion for BID Anticipate stage

# Get echarts4r component suggestions
echarts_components <- bid_suggest_components(validate_result, package = "echarts4r")
#> Warning: No components found for package: echarts4r

# Get suggestions from all supported packages
all_suggestions <- bid_suggest_components(validate_result)
#> ✔ Found 12 component suggestions across all packages for BID Validate stage
#> ℹ Showing components ordered by relevance to your BID analysis
```

## Comprehensive Reporting

Generate documentation for your BID implementation:

``` r
# Generate a report in various formats
text_report <- bid_report(validate_result)
#> ✔ Found 4 bslib component suggestion for BID Validate stage
#> ✔ Found 7 shiny component suggestion for BID Validate stage
html_report <- bid_report(validate_result, format = "html")
#> ✔ Found 4 bslib component suggestion for BID Validate stage
#> ✔ Found 7 shiny component suggestion for BID Validate stage
md_report <- bid_report(validate_result, format = "markdown", include_diagrams = TRUE)
#> ✔ Found 4 bslib component suggestion for BID Validate stage
#> ✔ Found 7 shiny component suggestion for BID Validate stage
```

## Example Workflow

``` r
library(shiny)
library(bidux)

# Document the entire BID process
bid_process <- bid_notice(
  problem = "Users can't find the most important metrics",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics"
) |>
  bid_interpret(
    central_question = "How are sales trending against targets?",
    data_story = list(
      hook = "Sales are trending below target in Q2",
      context = "Previous quarters exceeded targets",
      tension = "What's causing the Q2 decline?",
      resolution = "Identify underperforming product categories"
    )
  ) |>
  bid_structure(
    layout = "dual_process",
    concepts = c("principle_of_proximity", "default_effect")
  ) |>
  bid_anticipate(
    bias_mitigations = list(
      anchoring = "Using context-aware reference points",
      framing = "Providing both positive and negative framings"
    )
  ) |>
  bid_validate(
    summary_panel = "Key insights summary",
    collaboration = "Team annotation features"
  )
#> Auto-suggested theory: Cognitive Load Theory (confidence: 90%)
#> Stage 1 (Notice) completed. (20% complete)
#>   - Problem: Users can't find the most important metrics
#>   - Theory: Cognitive Load Theory (auto-suggested)
#>   - Evidence: User testing showed 70% of users spending >30s looking fo...
#>   - Theory confidence: 90%
#>   - Next: Use bid_interpret() for Stage 2 
#> Stage 2 (Interpret) completed.
#>   - Central question: How are sales trending against targets?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Returning all 41 concepts
#> 
#> 
#> 
#> ── Implementation tips for selected concepts: 
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> • Principle of Proximity: Place related controls and visualizations in
#> proximity to each other.
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> • Default Effect: Pre-select the most useful timeframe or metrics for initial
#> view.
#> Stage 3 (Structure) completed.
#>   - Layout: dual_process
#>   - Concepts: Principle of Proximity, Default Effect
#>   - No accessibility considerations specified
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found 7 concept(s) matching 'bias|anchor|fram|confirm'
#> 
#> Automatically suggested interaction principles: progressive_disclosure, hover_effects.
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found partial match: Anchoring Effect
#> 
#> Rows: 41 Columns: 7
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (7): concept, description, category, reference, example, implementation_...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Found partial match: Framing & Loss Aversion
#> Stage 4 (Anticipate) completed.
#>   - Bias mitigations: 2 defined
#>   - Interaction principles: 2 defined
#>   - Key suggestions: anchoring mitigation: Always show reference points like previous period, budget, or industry average., framing mitigation: Toggle between progress (65% complete) and gap (35% remaining) framing., Consider also addressing these common biases: confirmation
#> ℹ Suggested next steps:
#> • Conduct user testing with target audience to validate design decisions
#> • Implement bias mitigation strategies identified in the Anticipate stage
#> • Monitor user behavior patterns to validate bias assumptions
#> • Document successful patterns and lessons learned for future projects
#> • Plan iterative improvements based on user feedback and analytics
#> Stage 5 (Validate) completed.
#>   - Summary panel: Key insights summary
#>   - Collaboration: Team annotation features
#>   - Next steps: 5 items defined
#>   - Validation stage is well-defined. Focus on implementation and user testing.

# Generate implementation suggestions
ui_suggestions <- bid_suggest_components(bid_process, "bslib")
#> ✔ Found 4 bslib component suggestion for BID Validate stage

# Create a report
report <- bid_report(bid_process, format = "html")
#> ✔ Found 4 bslib component suggestion for BID Validate stage
#> ✔ Found 7 shiny component suggestion for BID Validate stage
```

## Learn More

Check out the vignettes for more information:

- `vignette("introduction-to-bid")` - Overview of the BID framework
- `vignette("getting-started")` - Quick start guide with implementation
  examples
- `vignette("concepts-reference")` - Detailed guide to implementing key
  concepts
