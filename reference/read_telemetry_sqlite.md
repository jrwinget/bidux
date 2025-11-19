# Read telemetry from SQLite database

Read telemetry from SQLite database

## Usage

``` r
read_telemetry_sqlite(source, events_table = NULL, table_name = NULL)
```

## Arguments

- source:

  SQLite database path or DBI connection object

- events_table:

  Optional custom events table data.frame

- table_name:

  Optional character string specifying table name to read

## Value

Data frame of events
