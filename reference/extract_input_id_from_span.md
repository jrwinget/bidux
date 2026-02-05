# Extract input ID from span name or attributes

Extracts input identifier from OTLP span. Looks in span name (e.g.,
"reactive:input\$slider1") and span attributes (e.g., input_id
attribute).

## Usage

``` r
extract_input_id_from_span(span_name, span_attributes)
```

## Arguments

- span_name:

  Character span name

- span_attributes:

  List or data frame of span attributes

## Value

Character input ID, or NA if not found

## Examples

``` r
if (FALSE) { # \dontrun{
# extract from span name
extract_input_id_from_span("reactive:input$slider1", NULL) # returns "slider1"

# extract from attributes
attrs <- list(list(key = "input_id", value = list(stringValue = "text1")))
extract_input_id_from_span("reactive", attrs) # returns "text1"
} # }
```
