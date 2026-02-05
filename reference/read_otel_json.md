# Read OpenTelemetry JSON (OTLP) file

Reads OpenTelemetry Protocol (OTLP) JSON files containing span data from
Shiny 1.12+ applications. Extracts spans from the nested OTLP structure
and converts them to bidux event schema.

## Usage

``` r
read_otel_json(path)
```

## Arguments

- path:

  Path to OTLP JSON file

## Value

Data frame with bidux event schema (converted from spans)

## Examples

``` r
if (FALSE) { # \dontrun{
events <- read_otel_json("otel_spans.json")
names(events)
# [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
# [7] "output_id" "navigation_id"
} # }
```
