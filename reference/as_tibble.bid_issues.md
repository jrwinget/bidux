# Convert bid_issues object to tibble

Extracts the tidy issues tibble from a bid_issues object for analysis
and visualization. This provides a structured view of all telemetry
issues with metadata for prioritization and reporting.

## Usage

``` r
# S3 method for class 'bid_issues'
as_tibble(x, ...)
```

## Arguments

- x:

  A bid_issues object from bid_ingest_telemetry()

- ...:

  Additional arguments (unused)

## Value

A tibble with issue metadata including severity, impact, and
descriptions
