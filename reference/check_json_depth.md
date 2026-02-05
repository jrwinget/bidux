# Check JSON nesting depth recursively

Validates that JSON data does not exceed a maximum nesting depth to
prevent stack overflow and resource exhaustion attacks.

## Usage

``` r
check_json_depth(obj, max_depth = 50, current_depth = 1)
```

## Arguments

- obj:

  JSON object (list or other R object from
  [`jsonlite::fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html))

- max_depth:

  Maximum allowed nesting depth (default: 50)

- current_depth:

  Current recursion depth (internal use)

## Value

Logical `TRUE` if depth is acceptable, aborts with error if exceeded
