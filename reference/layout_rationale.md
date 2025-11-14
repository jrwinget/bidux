# Generate layout selection rationale

Provides a concise explanation for why a particular layout was chosen
based on the content analysis of the previous stage.

## Usage

``` r
layout_rationale(previous_stage, chosen)
```

## Arguments

- previous_stage:

  A tibble or list output from an earlier BID stage

- chosen:

  Character string with the chosen layout type

## Value

Character string with explanation for the layout choice
