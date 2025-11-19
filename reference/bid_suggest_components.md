# Suggest UI Components Based on BID Framework Analysis

This function analyzes the results from BID framework stages and
suggests appropriate UI components from popular R dashboard packages
like shiny, bslib, DT, plotly, reactable, and htmlwidgets. The
suggestions are based on the design principles and user needs identified
in the BID process. Components work with both Shiny applications and
Quarto dashboards (shiny-prefixed components require Shiny runtime).

## Usage

``` r
bid_suggest_components(bid_stage, package = NULL)
```

## Arguments

- bid_stage:

  A tibble output from any BID framework stage function

- package:

  Optional character string specifying which package to focus
  suggestions on. Options include "shiny", "bslib", "DT", "plotly",
  "reactable", "htmlwidgets". If NULL, suggestions from all packages are
  provided.

## Value

A tibble containing component suggestions with relevance scores

## Examples

``` r
if (interactive()) {
  # After completing BID stages
  notice_result <- bid_notice(
    problem = "Users struggle with complex data",
    theory = "Cognitive Load Theory"
  )

  # Get all component suggestions
  bid_suggest_components(notice_result)

  # Get only bslib suggestions
  bid_suggest_components(notice_result, package = "bslib")

  # Get shiny-specific suggestions
  bid_suggest_components(notice_result, package = "shiny")
}
```
