# Search BID Framework Concepts

Search for behavioral science and UX concepts used in the BID framework.
Returns concepts matching the search term along with their descriptions,
categories, and implementation guidance.

## Usage

``` r
bid_concepts(search = NULL, fuzzy_match = TRUE, max_distance = 2)
```

## Arguments

- search:

  A character string to search for. If NULL or empty, returns all
  concepts.

- fuzzy_match:

  Logical indicating whether to use fuzzy string matching (default:
  TRUE)

- max_distance:

  Maximum string distance for fuzzy matching (default: 2)

## Value

A tibble containing matching concepts with their details
