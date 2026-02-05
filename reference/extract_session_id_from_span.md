# Extract session ID from span attributes

Extracts the session.id attribute from OTLP span attributes list.
Handles both nested list and data frame formats.

## Usage

``` r
extract_session_id_from_span(span_attributes)
```

## Arguments

- span_attributes:

  List or data frame of span attributes

## Value

Character session ID, or NA if not found

## Examples

``` r
if (FALSE) { # \dontrun{
# from list format
attrs <- list(
  list(key = "session.id", value = list(stringValue = "abc123"))
)
extract_session_id_from_span(attrs) # returns "abc123"
} # }
```
