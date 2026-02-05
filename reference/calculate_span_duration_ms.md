# Calculate span duration in milliseconds

Computes the duration between span start and end times in milliseconds.

## Usage

``` r
calculate_span_duration_ms(start_time, end_time)
```

## Arguments

- start_time:

  POSIXct start timestamp

- end_time:

  POSIXct end timestamp

## Value

Numeric duration in milliseconds, or NA if either time is missing

## Examples

``` r
if (FALSE) { # \dontrun{
start <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
end <- as.POSIXct("2024-01-01 12:00:01.5", tz = "UTC")
calculate_span_duration_ms(start, end) # returns 1500
} # }
```
