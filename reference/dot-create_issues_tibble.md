# Create tidy issues tibble from notice issues list

Create tidy issues tibble from notice issues list

## Usage

``` r
.create_issues_tibble(notice_issues, total_sessions, events)
```

## Arguments

- notice_issues:

  List of bid_stage objects from telemetry analysis

- total_sessions:

  Total number of sessions analyzed

- events:

  Raw events data frame

## Value

Tibble with structured issue metadata
