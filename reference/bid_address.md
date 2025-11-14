# Create Notice stage from single telemetry issue (sugar)

Convenience function that combines issue selection and Notice creation
in one step. Useful for quick workflows where you want to address a
specific issue immediately.

## Usage

``` r
bid_address(issue, previous_stage, ...)
```

## Arguments

- issue:

  A single row from bid_telemetry() output

- previous_stage:

  Previous BID stage (typically from bid_interpret)

- ...:

  Additional arguments passed to bid_notice_issue()

## Value

A bid_stage object in the Notice stage

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- bid_telemetry("data.sqlite")
interpret <- bid_interpret("How can we improve user experience?")

# Address the highest impact issue
top_issue <- issues[which.max(issues$impact_rate), ]
notice <- bid_address(top_issue, interpret)
} # }
```
