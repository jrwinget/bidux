# Create Notice stage from individual telemetry issue

Bridge function that converts a single telemetry issue row into a BID
Notice stage. This allows seamless integration between telemetry
analysis and the BID framework.

## Usage

``` r
bid_notice_issue(issue, previous_stage = NULL, override = list())
```

## Arguments

- issue:

  A single row from bid_telemetry() output or issues tibble

- previous_stage:

  Optional previous BID stage (typically from bid_interpret)

- override:

  List of values to override from the issue (problem, evidence, theory)

## Value

A bid_stage object in the Notice stage

## Examples

``` r
if (FALSE) { # \dontrun{
issues <- bid_telemetry("data.sqlite")
interpret <- bid_interpret("How can we reduce user friction?")

# Convert first issue to Notice stage
notice <- bid_notice_issue(issues[1, ], previous_stage = interpret)

# Override problem description
notice <- bid_notice_issue(
  issues[1, ],
  previous_stage = interpret,
  override = list(problem = "Custom problem description")
)
} # }
```
