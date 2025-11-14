# Suggest layout based on previous stage content using heuristics

Automatically suggests an appropriate layout type based on content
analysis of previous BID stages. Uses deterministic heuristics to match
keywords in problem descriptions, evidence, data story, and other
contextual information to layout types that best address the identified
issues.

## Usage

``` r
suggest_layout_from_previous(previous_stage, telemetry_flags = NULL)
```

## Arguments

- previous_stage:

  A tibble or list output from an earlier BID stage function containing
  stage data with potential fields like problem, evidence,
  central_question, data_story, etc.

- telemetry_flags:

  Optional named list of telemetry flags from bid_flags() Used to adjust
  layout recommendations based on observed behavior patterns

## Value

Character string indicating the suggested layout type ("breathable",
"dual_process", "grid", "card", "tabs", or fallback)

## Details

The heuristics follow a priority order:

1.  **breathable** - if content suggests information overload,
    confusion, or cognitive load issues

2.  **dual_process** - if content mentions overview vs detail, quick vs
    deep, or two-mode interactions

3.  **grid** - if content focuses on grouping, clustering, visual
    hierarchy, or comparing related metrics

4.  **card** - if content mentions cards, chunks, tiles, modular blocks,
    or per-item summaries

5.  **tabs** - if content suggests sections, categories, progressive
    disclosure, but avoids tabs if telemetry shows tab drop-off

6.  **breathable** - fallback for any unmatched cases
