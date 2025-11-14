# Find confusion patterns (rapid repeated changes)

Find confusion patterns (rapid repeated changes)

## Usage

``` r
find_confusion_patterns(
  events,
  window_seconds = confusion_window_secs,
  min_changes = confusion_min_changes
)
```

## Arguments

- events:

  Telemetry events data frame

- window_seconds:

  Time window in seconds

- min_changes:

  Minimum changes to flag as confusion

## Value

List of confusion patterns
