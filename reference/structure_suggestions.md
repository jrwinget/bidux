# Generate ranked, concept-grouped, actionable UI/UX suggestions

Creates structured suggestions organized by UX concepts with specific
component recommendations and rationales. Suggestions are ranked by
relevance and grouped by concept for systematic implementation.

## Usage

``` r
structure_suggestions(
  previous_stage,
  chosen_layout,
  concepts = NULL,
  quiet = NULL
)
```

## Arguments

- previous_stage:

  A tibble or list output from an earlier BID stage

- chosen_layout:

  Character string with the selected layout type

- concepts:

  Optional character vector of additional concepts to include

## Value

List of concept groups with ranked suggestions

## Details

The function combines concepts from multiple sources:

- Stage 1 theory (from Notice)

- Stage 2 inferred concepts (from keywords in story)

- Optional user-provided concepts

Each suggestion includes:

- title: Brief actionable description

- details: Specific implementation guidance

- components: Shiny/bslib component recommendations

- rationale: 1-2 sentence explanation

- score: Relevance ranking (0-1)
