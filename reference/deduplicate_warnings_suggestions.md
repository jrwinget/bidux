# Remove duplicate warning-suggestion pairs

Identifies and removes redundant warnings where the warning message
duplicates information already provided in suggestions. Helps clean up
verbose output by preferring actionable suggestions over warnings.

## Usage

``` r
deduplicate_warnings_suggestions(
  warnings,
  suggestions,
  similarity_threshold = 0.7
)
```

## Arguments

- warnings:

  Character vector of warning messages

- suggestions:

  Character vector of suggestion messages

- similarity_threshold:

  Similarity threshold for detecting duplicates (0-1)

## Value

List with cleaned warnings and suggestions
