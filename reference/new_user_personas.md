# Create user personas tibble

Creates a structured user personas tibble for use in bidux functions.
Replaces nested list structures with a validated data.frame structure.

## Usage

``` r
new_user_personas(personas_df)
```

## Arguments

- personas_df:

  Data.frame with required columns: name, goals, pain_points,
  technical_level

## Value

A bid_user_personas S3 object (inherits from data.frame)

## Examples

``` r
if (FALSE) { # \dontrun{
personas <- new_user_personas(data.frame(
  name = c("data analyst", "product manager"),
  goals = c("quick insights", "strategic overview"),
  pain_points = c("complex tools", "data delays"),
  technical_level = c("intermediate", "beginner")
))
} # }
```
