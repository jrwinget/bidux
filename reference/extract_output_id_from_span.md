# Extract output ID from span name or attributes

Extracts output identifier from OTLP span. Looks in span name (e.g.,
"output:plot1") and span attributes (e.g., output_id attribute).

## Usage

``` r
extract_output_id_from_span(span_name, span_attributes)
```

## Arguments

- span_name:

  Character span name

- span_attributes:

  List or data frame of span attributes

## Value

Character output ID, or NA if not found

## Examples

``` r
if (FALSE) { # \dontrun{
# extract from span name
extract_output_id_from_span("output:plot1", NULL) # returns "plot1"

# extract from attributes
attrs <- list(list(key = "output_id", value = list(stringValue = "table1")))
extract_output_id_from_span("output", attrs) # returns "table1"
} # }
```
