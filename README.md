# bidux: Behavior Insight Design for Shiny UI/UX

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/bidux)](https://CRAN.r-status.org/package=bidux)
<!-- badges: end -->

## Overview

The `{bidux}` package helps Shiny developers implement the Behavior Insight Design (BID) framework in their workflow. BID is a 5-stage process that incorporates psychological principles into UI/UX design:

1. **Notice** the Problem - Identify friction points using principles of cognitive load and visual hierarchies
2. **Interpret** the User's Need - Create compelling data stories and define user personas
3. **Structure** the Dashboard - Apply layout patterns and accessibility considerations
4. **Anticipate** User Behavior - Mitigate cognitive biases and implement effective interaction hints
5. **Validate** & Empower the User - Provide summary insights and collaborative features

This structured approach helps developers create more intuitive, user-friendly dashboards by systematically applying behavioral psychology concepts to their design process.

## Installation

You can install the development version of {bidux} from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jrwinget/bidux")
```

## Core Functions

The package provides documentation functions for each stage of the BID process:

```r
# Stage 1: Notice the Problem
notice_result <- bid_notice(
  problem = "Users can't find the most important metrics",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics",
  theory = "Visual Hierarchies",  # Optional - will be suggested if omitted
  target_audience = "Data analysts with varying technical skills"
)

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
```

## Concept Dictionary

The package includes a comprehensive dictionary of behavioral psychology concepts:

```r
# List all concepts
all_concepts <- bid_concepts()

# Search for specific concepts
cognitive_concepts <- bid_concepts("cognitive")
visual_concepts <- bid_concepts("visual, hierarchy") # Multiple search terms

# Get detailed information about a specific concept
anchoring_info <- bid_concept("anchoring effect")
```

## UI Component Suggestions

Get concrete implementation ideas for various UI packages:

```r
# Get bslib component suggestions
bslib_components <- bid_suggest_components(structure_result, package = "bslib")

# Get shiny component suggestions
shiny_components <- bid_suggest_components(notice_result, package = "shiny")

# Get reactable component suggestions
reactable_components <- bid_suggest_components(anticipate_result, package = "reactable")

# Get echarts4r component suggestions
echarts_components <- bid_suggest_components(validate_result, package = "echarts4r")

# Get suggestions from all supported packages
all_suggestions <- bid_suggest_components(validate_result, package = "all")
```

## Comprehensive Reporting

Generate documentation for your BID implementation:

```r
# Generate a report in various formats
text_report <- bid_report(validate_result)
html_report <- bid_report(validate_result, format = "html")
md_report <- bid_report(validate_result, format = "markdown", include_diagrams = TRUE)
```

## Example Workflow

```r
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

# Generate implementation suggestions
ui_suggestions <- bid_suggest_components(bid_process, "bslib")

# Create a report
report <- bid_report(bid_process, format = "html")
```

## Learn More

Check out the vignettes for more information:

* `vignette("introduction-to-bid")` - Overview of the BID framework
* `vignette("getting-started")` - Quick start guide with implementation examples
* `vignette("concepts-reference")` - Detailed guide to implementing key concepts

## License

This project is licensed under the MIT License.
