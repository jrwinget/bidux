# Get predefined telemetry sensitivity presets

Returns predefined threshold configurations for telemetry analysis with
different sensitivity levels. Use these presets with
[`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
or
[`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
to easily adjust how aggressively the analysis identifies UX friction
points.

**OpenTelemetry Compatibility**: These presets work with both
shiny.telemetry event data and Shiny 1.12+ OpenTelemetry span data. When
using OTEL data, spans are automatically converted to events for
analysis.

## Usage

``` r
bid_telemetry_presets(preset = c("moderate", "strict", "relaxed"))
```

## Arguments

- preset:

  Character string specifying the sensitivity level:

  strict

  :   Detects even minor issues - use for critical applications or new
      dashboards

  moderate

  :   Balanced default - appropriate for most applications (default)

  relaxed

  :   Only detects major issues - use for mature, stable dashboards

## Value

Named list of threshold parameters suitable for passing to
[`bid_ingest_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_ingest_telemetry.md)
or
[`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
thresholds parameter.

## Examples

``` r
# Get strict sensitivity thresholds
strict_thresholds <- bid_telemetry_presets("strict")

# Use with telemetry analysis (works with both shiny.telemetry and OTEL)
if (FALSE) { # \dontrun{
# Works with shiny.telemetry
issues <- bid_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("strict")
)

# Works with Shiny OpenTelemetry (1.12+)
issues <- bid_telemetry(
  "otel_spans.json",
  thresholds = bid_telemetry_presets("strict")
)
} # }

# Compare different presets
moderate <- bid_telemetry_presets("moderate")
relaxed <- bid_telemetry_presets("relaxed")
```
