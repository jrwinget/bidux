# Create pipeline of Notice stages from top telemetry issues (sugar)

Convenience function that creates a pipeline of Notice stages from the
highest priority telemetry issues. Useful for systematic issue
resolution workflows.

## Usage

``` r
bid_pipeline(issues, previous_stage, max = 3, ...)
```

## Arguments

- issues:

  A tibble from bid_telemetry() output

- previous_stage:

  Previous BID stage (typically from bid_interpret)

- max:

  Maximum number of issues to include in pipeline (default: 3)

- ...:

  Additional arguments passed to bid_notices()

## Value

A named list of bid_stage objects in the Notice stage

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- bid_telemetry("data.sqlite")
interpret <- bid_interpret("How can we systematically improve UX?")

# Create pipeline for top 3 issues
notice_pipeline <- bid_pipeline(issues, interpret, max = 3)

# Continue with first issue in pipeline
anticipate <- bid_anticipate(previous_stage = notice_pipeline[[1]])
} # }
```
