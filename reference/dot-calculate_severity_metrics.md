# Calculate severity metrics for an issue

Calculate severity metrics for an issue

## Usage

``` r
.calculate_severity_metrics(issue_key, notice, events, total_sessions)
```

## Arguments

- issue_key:

  String identifier for the issue

- notice:

  Bid_stage notice object containing problem description

- events:

  Raw events data frame

- total_sessions:

  Total number of sessions

## Value

List with severity, affected_sessions, and impact_rate
