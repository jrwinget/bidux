# Generate BID Framework Report

Creates a comprehensive report from a completed BID framework process.
This report summarizes all stages and provides recommendations for
implementation.

## Usage

``` r
bid_report(
  validate_stage,
  format = c("text", "html", "markdown"),
  include_diagrams = TRUE
)
```

## Arguments

- validate_stage:

  A tibble output from
  [`bid_validate()`](https://jrwinget.github.io/bidux/reference/bid_validate.md).

- format:

  Output format: "text", "html", or "markdown"

- include_diagrams:

  Logical, whether to include ASCII diagrams in the report (default:
  TRUE)

## Value

A formatted report summarizing the entire BID process

## Examples

``` r
if (interactive()) {
  # After completing all 5 stages
  validation_result <- bid_validate(...)

  # Generate a text report
  bid_report(validation_result)

  # Generate an HTML report
  bid_report(validation_result, format = "html")

  # Generate a markdown report without diagrams
  bid_report(
    validation_result,
    format = "markdown",
    include_diagrams = FALSE
  )
}
```
