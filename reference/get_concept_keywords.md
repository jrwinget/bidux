# Get the canonical concept keyword map for text-based concept detection

Returns the single authoritative mapping of behavioral science concept
names to keyword vectors. This is the merged union of all keyword sets
previously scattered across
[`infer_concepts_from_story()`](https://jrwinget.github.io/bidux/reference/infer_concepts_from_story.md)
and `detect_concepts_from_text()`.

## Usage

``` r
get_concept_keywords()
```

## Value

Named list mapping concept names to keyword vectors
