# Read OpenTelemetry SQLite database

Reads OpenTelemetry span data from SQLite databases that store OTEL
traces. Looks for standard OTEL table names (spans, span_events,
span_attributes) and joins them to reconstruct the span structure before
converting to bidux events.

## Usage

``` r
read_otel_sqlite(source)
```

## Arguments

- source:

  SQLite database path or DBI connection object

## Value

Data frame with bidux event schema (converted from spans)

## Examples

``` r
if (FALSE) { # \dontrun{
events <- read_otel_sqlite("otel_traces.db")
names(events)
# [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
# [7] "output_id" "navigation_id"
} # }
```
