# Ingest telemetry data and identify UX friction points

This function ingests telemetry data from multiple sources and
automatically identifies potential UX issues, translating them into BID
framework Notice stages. It returns a hybrid object that is
backward-compatible as a list of Notice stages while also providing
enhanced functionality with tidy tibble access and flags extraction.

**Supported telemetry sources:**

- shiny.telemetry (SQLite or JSON)

- Shiny native OpenTelemetry (Shiny \>= 1.12.0, OTLP JSON or SQLite)

- DBI database connections

Format is automatically detected based on file structure and content.

**OpenTelemetry Support**: For Shiny \>= 1.12.0 applications using
native OpenTelemetry, pass the path to OTLP JSON exports or
OTEL-formatted SQLite databases. Spans are automatically converted to
events for analysis. See
[`vignette("otel-integration")`](https://jrwinget.github.io/bidux/articles/otel-integration.md)
for complete setup guide.

**Note:** For Quarto dashboards, shiny.telemetry only works when using
`server: shiny` in the Quarto YAML. Static Quarto dashboards and
OJS-based dashboards do not support shiny.telemetry. Consider
alternative analytics solutions (e.g., Plausible) for static dashboard
usage tracking.

## Usage

``` r
bid_ingest_telemetry(
  source,
  format = NULL,
  events_table = NULL,
  table_name = NULL,
  thresholds = list()
)
```

## Arguments

- source:

  Either a file path to telemetry data or a DBI connection object.
  Supports:

  - SQLite databases (shiny.telemetry or OTEL format)

  - JSON files (shiny.telemetry logs or OTLP JSON exports)

  - DBI connections to databases with event or span tables When a
    connection is provided, it will not be closed by this function.

- format:

  Optional format specification ("sqlite", "json", "otlp_json",
  "otel_sqlite"). If NULL (default), auto-detected from file extension
  and structure. OTLP formats are automatically detected when file
  contains OpenTelemetry span data.

- events_table:

  Optional data.frame specifying custom events table when reading from
  SQLite. Must have columns: event_id, timestamp, event_type, user_id.
  If NULL, auto-detects standard table names (event_data, events).
  Cannot be used with `table_name`.

- table_name:

  Optional character string specifying the table name to read from the
  database. If NULL (default), auto-detects standard table names
  (event_data, events). Cannot be used with `events_table`.

- thresholds:

  Optional list of threshold parameters: - unused_input_threshold:
  percentage of sessions below which input is considered unused
  (default: 0.05) - delay_threshold_secs: seconds of delay considered
  problematic (default: 30) - error_rate_threshold: percentage of
  sessions with errors considered problematic (default: 0.1) -
  navigation_threshold: percentage of sessions visiting a page below
  which it's considered underused (default: 0.2) - rapid_change_window:
  seconds within which multiple changes indicate confusion
  (default: 10) - rapid_change_count: number of changes within window to
  flag as confusion (default: 5)

## Value

A hybrid object of class c("bid_issues", "list") containing bid_stage
objects for each identified issue in the "Notice" stage. The object
includes:

- Legacy list:

  Named list of bid_stage objects (e.g., "unused_input_region",
  "delayed_interaction")

- issues_tbl:

  Attached tidy tibble with issue metadata

- flags:

  Global telemetry flags as named list

- created_at:

  Timestamp when object was created

        Use as_tibble() to access the tidy issues data, bid_flags() to extract flags,
        and legacy list access for backward compatibility.

## Examples

``` r
if (FALSE) { # \dontrun{
# Works with shiny.telemetry SQLite
issues <- bid_ingest_telemetry("telemetry.sqlite")

# Works with Shiny OpenTelemetry (1.12+)
issues <- bid_ingest_telemetry("otel_spans.json")

# Use sensitivity presets for easier configuration
strict_issues <- bid_ingest_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("strict")
)

# Analyze JSON log with custom thresholds
issues <- bid_ingest_telemetry(
  "telemetry.log",
  format = "json",
  thresholds = list(
    unused_input_threshold = 0.1,
    delay_threshold_secs = 60
  )
)

# Use a DBI connection object directly
con <- DBI::dbConnect(RSQLite::SQLite(), "telemetry.sqlite")
issues <- bid_ingest_telemetry(con)
# Connection remains open for further use
DBI::dbDisconnect(con)

# Specify custom table name
issues <- bid_ingest_telemetry(
  "telemetry.sqlite",
  table_name = "my_custom_events"
)

# Same analysis workflow for both shiny.telemetry and OTEL
if (length(issues) > 0) {
  # Take first issue and continue with BID process
  interpret_result <- bid_interpret(
    previous_stage = issues[[1]],
    central_question = "How can we improve user engagement?"
  )
}
} # }
```
