# Check if OpenTelemetry packages are available

Internal function to check if the required OpenTelemetry packages (otel,
otelsdk) are installed. Provides helpful error message if not available.

## Usage

``` r
check_otel_available()
```

## Value

NULL invisibly if packages are available, otherwise throws error
