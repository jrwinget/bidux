# Extract error message from span events

Extracts error messages from OTLP span events. Error events contain the
error details in their attributes.

## Usage

``` r
extract_error_message_from_span(span_events)
```

## Arguments

- span_events:

  List or data frame of span events

## Value

Character error message, or NA if no error found

## Examples

``` r
if (FALSE) { # \dontrun{
events <- list(
  list(
    name = "error",
    attributes = list(
      list(key = "message", value = list(stringValue = "Division by zero"))
    )
  )
)
extract_error_message_from_span(events) # returns "Division by zero"
} # }
```
