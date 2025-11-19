# Read telemetry data from file or connection

Read telemetry data from file or connection

## Usage

``` r
read_telemetry_data(source, format, events_table = NULL, table_name = NULL)
```

## Arguments

- source:

  File path or DBI connection object

- format:

  Format ("sqlite" or "json")

- events_table:

  Optional custom events table for SQLite

- table_name:

  Optional table name for SQLite

## Value

Data frame of events
