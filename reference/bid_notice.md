# Document User Notice Stage in BID Framework

This function documents the observation and problem identification
stage. It represents stage 2 in the BID framework and now returns a
structured bid_stage object with enhanced metadata and external mapping
support.

## Usage

``` r
bid_notice(
  previous_stage,
  problem,
  theory = NULL,
  evidence = NULL,
  quiet = NULL,
  ...
)
```

## Arguments

- previous_stage:

  A tibble or list output from the previous BID stage function
  (typically bid_interpret).

- problem:

  A character string describing the observed user problem.

- theory:

  A character string describing the behavioral theory that might explain
  the problem. If NULL, will be auto-suggested using external theory
  mappings.

- evidence:

  A character string describing evidence supporting the problem.

- quiet:

  Logical indicating whether to suppress informational messages. If
  NULL, uses getOption("bidux.quiet", FALSE).

- ...:

  Additional parameters. Deprecated parameters (e.g., 'target_audience')
  will generate warnings if provided.

## Value

A bid_stage object containing the documented information for the
"Notice" stage with enhanced metadata and validation.

## Examples

``` r
interpret_result <- bid_interpret(
  central_question = "How can we improve user task completion?",
  data_story = list(
    hook = "Users are struggling with complex interfaces",
    resolution = "Simplify key interactions"
  )
)
#> Warning: ! Using deprecated list format for data_story parameter
#> ℹ Please use new_data_story() constructor for new code
#> ℹ Legacy format will be automatically migrated
#> Warning: ! Using deprecated nested format for data_story
#> ℹ The flat API is now recommended: new_data_story(hook, context, tension,
#>   resolution)
#> ℹ Nested format (variables, relationships) will be removed in bidux 0.4.0
#> Stage 1 (Interpret) completed.
#>   - Central question: How can we improve user task completion?
#>   - Your data story is incomplete (25%). Consider adding these missing elements: hook, tension, resolution.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined 

# Auto-suggested theory
bid_notice(
  previous_stage = interpret_result,
  problem = "Users struggling with complex dropdowns and too many options",
  evidence = "User testing shows 65% abandonment rate on filter selection"
)
#> Auto-suggested theory: Hick's Law (confidence: 90%)
#> Stage 2 (Notice) completed. (40% complete)
#>   - Problem: Users struggling with complex dropdowns and too many options
#>   - Theory: Hick's Law (auto-suggested)
#>   - Evidence: User testing shows 65% abandonment rate on filter selection
#>   - Theory confidence: 90%
#>   - Next: Use bid_anticipate() for Stage 3 
#> BID Framework - Notice Stage
#> Generated: 2025-11-19 21:31:15 
#> Progress: 40 % (2/5) 
#> 
#> Problem: Users struggling with complex dropdowns and too many options 
#> Theory: Hick's Law (auto-suggested) 
#> Evidence: User testing shows 65% abandonment rate on filter selection 
#> 
#>  Suggestions: Ensure your problem description is specific and supported by strong evidence 
#> 
#>  Use summary() for detailed information 

# With explicit theory
notice_result <- bid_notice(
  previous_stage = interpret_result,
  problem = "Mobile interface is difficult to navigate",
  theory = "Fitts's Law",
  evidence = "Mobile users report frustration with small touch targets"
)
#> Stage 2 (Notice) completed. (40% complete)
#>   - Problem: Mobile interface is difficult to navigate
#>   - Theory: Fitts's Law
#>   - Evidence: Mobile users report frustration with small touch targets
#>   - Next: Use bid_anticipate() for Stage 3 

summary(notice_result)
#> === BID Framework: Notice Stage Summary ===
#> 
#> Metadata:
#>    stage_number : 2 
#>    total_stages : 5 
#>    validation_status : completed 
#>    auto_suggested_theory : No 
#>    theory_confidence : 1 
#>    problem_length : 41 
#>    evidence_length : 56 
#>    custom_mappings_used : No 
#> 
#> Stage Data:
#>    stage : Notice 
#>    problem : Mobile interface is difficult to navigate 
#>    theory : Fitts's Law 
#>    evidence : Mobile users report frustration with small touch targets 
#>    suggestions : Ensure your problem description is specific and supported by strong evidence 
#> 
#> Generated: 2025-11-19 21:31:15 
```
