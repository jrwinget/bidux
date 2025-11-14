# Get detailed information about a specific concept

Returns detailed information about a specific BID framework concept,
including implementation recommendations based on the concept's stage.

## Usage

``` r
bid_concept(concept_name, add_recommendations = TRUE)
```

## Arguments

- concept_name:

  A character string with the exact or partial concept name

- add_recommendations:

  Logical indicating whether to add stage-specific recommendations

## Value

A tibble with detailed concept information
