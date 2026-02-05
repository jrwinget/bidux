# Parse Unix nanosecond timestamp to POSIXct

Converts OTLP Unix nanosecond timestamps to R POSIXct objects.

## Usage

``` r
parse_span_timestamp(span_time_unix_nano)
```

## Arguments

- span_time_unix_nano:

  Character string or numeric Unix timestamp in nanoseconds

## Value

POSIXct timestamp in UTC timezone, or NA if parsing fails

## Examples

``` r
if (FALSE) { # \dontrun{
# parse otlp timestamp
parse_span_timestamp("1234567890123456789")
} # }
```
