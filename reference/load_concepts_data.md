# Load concepts data from external file with caching

Loads concepts data from CSV file with caching for performance. Uses
memoise to ensure data is only loaded once per session.

## Usage

``` r
load_concepts_data()
```

## Value

A tibble with concepts data
