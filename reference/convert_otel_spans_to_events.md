# Convert OTLP spans to bidux event schema

Converts OpenTelemetry Protocol (OTLP) span data to the bidux telemetry
event schema. This enables transparent compatibility with existing
friction detection algorithms. This function is called automatically by
[`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
and
[`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
when OTLP data is detected - you rarely need to call it directly.

**Automatic Format Detection**: When you pass OTLP JSON or SQLite to
[`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md),
this conversion happens automatically. The same UX friction detection
algorithms work seamlessly on both shiny.telemetry events and
OpenTelemetry spans.

**Span to Event Mapping**:

- session_start -\> login

- output -\> output

- reactive, observe -\> input

- reactive_update -\> synthetic timing events

- Error span events -\> error

## Usage

``` r
convert_otel_spans_to_events(spans_df)
```

## Arguments

- spans_df:

  Data frame of OTLP spans with columns:

  - name: span name (e.g., "session_start", "output:plot1")

  - startTimeUnixNano: start timestamp in Unix nanoseconds

  - endTimeUnixNano: end timestamp in Unix nanoseconds

  - attributes: list column with span attributes

  - events: list column with span events (for errors)

## Value

Tibble with bidux event schema columns:

- timestamp: POSIXct event timestamp

- session_id: character session identifier

- event_type: character event type (login, input, output, error)

- input_id: character input identifier (NA for non-input events)

- value: character/numeric value (NA for most otel spans)

- error_message: character error message (NA for non-error events)

- output_id: character output identifier (NA for non-output events)

- navigation_id: character navigation identifier (NA for otel spans)

## See also

- [`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
  for high-level telemetry analysis (automatic format detection)

- [`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
  for legacy telemetry workflows

- [`vignette("otel-integration")`](https://jrwinget.github.io/bidux/articles/otel-integration.md)
  for complete OTEL setup guide

## Examples

``` r
if (FALSE) { # \dontrun{
# Typically you don't need to call this directly - use bid_telemetry() instead:
issues <- bid_telemetry("otel_spans.json")

# Manual conversion (advanced use case):
# After reading otlp json file
spans <- read_otel_json("spans.json")
events <- convert_otel_spans_to_events(spans)

# Verify schema compatibility
names(events)
# [1] "timestamp" "session_id" "event_type" "input_id" "value" "error_message"
# [7] "output_id" "navigation_id"

# Now use standard friction detection
issues <- detect_telemetry_issues(events)
} # }
```
