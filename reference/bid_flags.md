# Extract telemetry flags from bid_issues object

Extracts global telemetry flags and metadata from a bid_issues object.
These flags provide boolean indicators for different types of issues and
can be used for conditional logic in downstream BID stages.

## Usage

``` r
bid_flags(x)

# S3 method for class 'bid_issues'
bid_flags(x)

# Default S3 method
bid_flags(x)
```

## Arguments

- x:

  A bid_issues object from bid_ingest_telemetry() or any object with a
  flags attribute

## Value

A named list of boolean flags and metadata
