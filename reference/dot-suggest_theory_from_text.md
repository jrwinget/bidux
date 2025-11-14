# Suggest theory from text with confidence scoring and messaging

Internal helper that wraps suggest_theory_from_mappings with additional
confidence scoring and user messaging. Factored out for reuse across
different BID functions.

## Usage

``` r
.suggest_theory_from_text(
  problem_text,
  evidence_text = NULL,
  mappings = NULL,
  show_message = TRUE
)
```

## Arguments

- problem_text:

  Clean problem description text

- evidence_text:

  Clean evidence description text

- mappings:

  Optional custom theory mappings

- show_message:

  Whether to display auto-suggestion message (default TRUE)

## Value

List with theory, confidence, and auto_suggested flag
