# Convert nested suggestion groups to flat tibble

Transforms the nested list structure of suggestions (grouped by concept)
into a flat tibble with one row per suggestion. Maintains backward
compatibility by keeping the nested structure available.

## Usage

``` r
flatten_suggestions_to_tibble(suggestion_groups)
```

## Arguments

- suggestion_groups:

  List of concept groups with suggestions

## Value

Tibble with columns: concept, title, details, components, rationale,
score, difficulty, category
