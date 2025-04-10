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
  evidence = "User testing showed 70% of users spending >30s looking for key metrics"
)

# Stage 2: Interpret the User's Need
bid_interpret(
  central_question = "How are sales trending against targets?",
  data_story = list(
    hook = "Sales are trending below target in Q2",
    context = "Previous quarters exceeded targets",
    tension = "What's causing the Q2 decline?",
    resolution = "Identify underperforming product categories"
  )
)

# Stage 3: Structure the Dashboard
bid_structure(
  layout = "dual process",
  concepts = c("principle of proximity", "default effect")
)

# Stage 4: Anticipate User Behavior
bid_anticipate(
  bias_mitigations = list(
    anchoring = "Using context-aware reference points",
    framing = "Providing both positive and negative framings",
    confirmation = "Including alternative scenarios"
  )
)

# Stage 5: Validate & Empower
bid_validate(
  validation_methods = c("peak end summary", "team annotations")
)
```

## Concept Dictionary

The package includes a comprehensive dictionary of behavioral psychology concepts:

```r
# List all concepts
bid_concepts()

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
    central_question = "How are sales trending against targets?",
    data_story = list(
      hook = "Sales are trending below target in Q2",
      context = "Previous quarters exceeded targets",
      tension = "What's causing the Q2 decline?",
      resolution = "Identify underperforming product categories"
    )
  ) |>
  bid_structure(
    layout = "dual process",
    concepts = c("principle_of_proximity", "default_effect")
  ) |>
  bid_anticipate(
    bias_mitigations = list(
      anchoring = "Using context-aware reference points",
      framing = "Providing both positive and negative framings",
      confirmation = "Including alternative scenarios"
    )
  ) |>
  bid_validate(
    validation_methods = c("peak end summary", "team annotations")
  )
```

## Learn More

Check out the vignettes for more information:

* `vignette("introduction-to-bid")` - Overview of the BID framework
* `vignette("getting-started")` - Quick start guide

## License

This project is licensed under the MIT License.
