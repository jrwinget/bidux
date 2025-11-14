# Apply suggestion rules to context data

Evaluates suggestion rules against provided context data and returns
applicable suggestions. Used by generate_stage_suggestions() for
consistent suggestion generation.

## Usage

``` r
apply_suggestion_rules(stage_name, context_data, rules_list = NULL)
```

## Arguments

- stage_name:

  Name of the BID stage

- context_data:

  Named list with stage-specific context

- rules_list:

  Optional custom rules list (defaults to consolidated rules)

## Value

Character vector of applicable suggestions
