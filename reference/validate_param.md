# Common validation utility for bidux functions (DRY principle)

Centralized parameter validation to reduce code duplication

## Usage

``` r
validate_param(
  value,
  arg_name,
  type = "character",
  min_length = 1,
  max_length = Inf,
  allow_na = FALSE,
  choices = NULL
)
```

## Arguments

- value:

  The value to validate

- arg_name:

  The argument name for error messages

- type:

  Expected type: "character", "logical", "numeric"

- min_length:

  Minimum length for vectors

- max_length:

  Maximum length for vectors

- allow_na:

  Whether NA values are allowed

- choices:

  Valid choices for character parameters
