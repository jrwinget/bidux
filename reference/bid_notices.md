# Create multiple Notice stages from telemetry issues

Bridge function that converts multiple telemetry issues into Notice
stages. Provides filtering and limiting options for managing large issue
sets.

## Usage

``` r
bid_notices(issues, filter = NULL, previous_stage = NULL, max_issues = 5, ...)
```

## Arguments

- issues:

  A tibble from bid_telemetry() output

- filter:

  Optional filter expression for subsetting issues (e.g., severity ==
  "critical")

- previous_stage:

  Optional previous BID stage (typically from bid_interpret)

- max_issues:

  Maximum number of issues to convert (default: 5)

- ...:

  Additional arguments passed to bid_notice_issue()

## Value

A named list of bid_stage objects in the Notice stage

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- bid_telemetry("data.sqlite")
interpret <- bid_interpret("How can we reduce user friction?")

# Convert all critical issues
notices <- bid_notices(issues, filter = severity == "critical", interpret)

# Convert top 3 issues by impact
top_issues <- issues[order(-issues$impact_rate), ][1:3, ]
notices <- bid_notices(top_issues, previous_stage = interpret)
} # }
```
