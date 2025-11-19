# Concise telemetry analysis with tidy output

Preferred modern interface for telemetry analysis. Returns a clean
tibble of identified issues without the legacy list structure. Use this
function for new workflows that don't need backward compatibility.

## Usage

``` r
bid_telemetry(
  source,
  format = NULL,
  events_table = NULL,
  table_name = NULL,
  thresholds = list()
)
```

## Arguments

- source:

  Either a file path to telemetry data (SQLite database or JSON log
  file), or a DBI connection object to an already-open database. When a
  connection is provided, it will not be closed by this function.

- format:

  Optional format specification ("sqlite" or "json"). If NULL,
  auto-detected from file extension (for file paths) or defaults to
  "sqlite" for DBI connections.

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

A tibble of class "bid_issues_tbl" with structured issue metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Modern workflow
issues <- bid_telemetry("telemetry.sqlite")
high_priority <- issues[issues$severity %in% c("critical", "high"), ]

# Use DBI connection directly
con <- DBI::dbConnect(RSQLite::SQLite(), "telemetry.sqlite")
issues <- bid_telemetry(con, table_name = "my_events")
DBI::dbDisconnect(con)

# Use with bridges for BID workflow
top_issue <- issues[1, ]
notice <- bid_notice_issue(top_issue, previous_stage = interpret_stage)
} # }
```
