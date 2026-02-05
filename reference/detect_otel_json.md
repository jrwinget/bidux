# Detect if JSON file contains OTLP (OpenTelemetry Protocol) data

Checks if a JSON file contains OpenTelemetry Protocol span data by
looking for the characteristic OTLP structure (resourceSpans,
scopeSpans, spans).

## Usage

``` r
detect_otel_json(source_path)
```

## Arguments

- source_path:

  Path to JSON file

## Value

Logical TRUE if OTLP format detected, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
detect_otel_json("spans.json") # returns TRUE for otlp files
detect_otel_json("telemetry.json") # returns FALSE for shiny.telemetry files
} # }
```
