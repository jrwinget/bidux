# Create bias mitigations tibble

Creates a structured bias mitigations tibble for use in bidux functions.
Replaces nested list structures with a validated data.frame structure.

## Usage

``` r
new_bias_mitigations(mitigations_df)
```

## Arguments

- mitigations_df:

  Data.frame with required columns: bias_type, mitigation_strategy,
  confidence_level

## Value

A bid_bias_mitigations S3 object (inherits from data.frame)

## Examples

``` r
if (FALSE) { # \dontrun{
mitigations <- new_bias_mitigations(data.frame(
  bias_type = c("confirmation_bias", "selection_bias"),
  mitigation_strategy = c("seek_disconfirming_evidence", "randomize_sample"),
  confidence_level = c(0.8, 0.7)
))
} # }
```
