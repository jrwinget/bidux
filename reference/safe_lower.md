# Safely convert text to lowercase with null handling

Helper function that safely converts text to lowercase while handling
NULL, NA, and non-character values gracefully.

## Usage

``` r
safe_lower(x)
```

## Arguments

- x:

  Input value to convert to lowercase string

## Value

Character string in lowercase, or empty string if input is NULL/NA
