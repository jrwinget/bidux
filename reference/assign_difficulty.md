# Assign difficulty rating based on components and suggestion complexity

Analyzes suggestion details and component count to assign a difficulty
rating. Considers complexity patterns in the suggestion text and number
of components.

## Usage

``` r
assign_difficulty(suggestion)
```

## Arguments

- suggestion:

  Suggestion object with title, details, and components

## Value

Character string: "Easy", "Medium", or "Hard"
