# bidux: Behavior Insight Design for Shiny UI/UX

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/bidux)](https://CRAN.r-status.org/package=bidux)
<!-- badges: end -->

## Overview

The `{bidux}` package helps Shiny developers implement the Behavior Insight Design (BID) framework in their workflow. BID is a 5-stage process that incorporates psychological principles into UI/UX design:

1. **Notice** the Problem
2. **Interpret** the User's Need
3. **Structure** the Dashboard
4. **Anticipate** User Behavior
5. **Validate** & Empower the User

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
bid_notice(
  problem = "Users can't find the most important metrics",
  theory = "visual_hierarchies",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics",
  target_audience = "Data analysts with varying technical skills"
)

# Stage 2: Interpret the User's Need
bid_interpret(
  previous_stage = notice_result,
  central_question = "How are sales trending against targets?",
  data_story = list(
    hook = "Sales are trending below target in Q2",
    context = "Previous quarters exceeded targets",
    tension = "What's causing the Q2 decline?",
    resolution = "Identify underperforming product categories"
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
bid_structure(
  previous_stage = interpret_result,
  layout = "dual_process",
  concepts = c("principle_of_proximity", "default_effect", "breathable_layouts"),
  accessibility = list(
    color_contrast = "WCAG AA compliant",
    keyboard_navigation = "All elements focusable"
  )
)

# Stage 4: Anticipate User Behavior
bid_anticipate(
  previous_stage = structure_result,
  bias_mitigations = list(
    anchoring = "Using context-aware reference points",
    framing = "Providing both positive and negative framings",
    confirmation = "Including alternative scenarios"
  ),
  interaction_principles = list(
    hover_effects = "Show details on hover",
    selection_feedback = "Highlight active filters"
  )
)

# Stage 5: Validate & Empower
bid_validate(
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

## UI Component Suggestions

Get implementation ideas for various UI packages:

```r
# Get bslib component suggestions
bid_suggest_components(structure_result, package = "bslib")

# Get shiny component suggestions
bid_suggest_components(notice_result, package = "shiny")

# Get suggestions from all supported packages
bid_suggest_components(validate_result, package = "all")
```

## Comprehensive Reporting

Generate documentation for your BID implementation:

```r
# Generate a report in various formats
text_report <- bid_report(validate_result)
html_report <- bid_report(validate_result, format = "html")
md_report <- bid_report(validate_result, format = "markdown")
```

## Concept Dictionary

The package includes a comprehensive dictionary of behavioral psychology concepts:

```r
# List all concepts
bid_concepts()

# Search for specific concepts
bid_concepts("cognitive")

# Get detailed information about a specific concept
bid_concept("anchoring effect")
```

## Example Workflow

```r
library(shiny)
library(bidux)

# Document the entire BID process
bid_process <- bid_notice(
  problem = "Users can't find the most important metrics",
  theory = "Information Hierarchy",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics"
) |>
  bid_interpret(
    previous_stage = _,
    central_question = "How are sales trending against targets?",
    data_story = list(
      hook = "Sales are trending below target in Q2",
      context = "Previous quarters exceeded targets",
      tension = "What's causing the Q2 decline?",
      resolution = "Identify underperforming product categories"
    )
  ) |>
  bid_structure(
    previous_stage = _,
    layout = "dual_process",
    concepts = c("principle_of_proximity", "default_effect")
  ) |>
  bid_anticipate(
    previous_stage = _,
    bias_mitigations = list(
      anchoring = "Using context-aware reference points",
      framing = "Providing both positive and negative framings",
      confirmation = "Including alternative scenarios"
    )
  ) |>
  bid_validate(
    previous_stage = _,
    summary_panel = "Key insights summary",
    collaboration = "Team annotation features"
  )
```

## Learn More

Check out the vignettes for more information:

* `vignette("introduction-to-bid")` - Overview of the BID framework
* `vignette("getting-started")` - Quick start guide with implementation examples

## License

This project is licensed under the MIT License.
