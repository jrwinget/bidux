# Document User Behavior Anticipation Stage in BID Framework

This function documents the anticipated user behavior by listing bias
mitigation strategies related to anchoring, framing, confirmation bias,
etc. It also supports adding interaction hints and visual feedback
elements.

## Usage

``` r
bid_anticipate(
  previous_stage,
  bias_mitigations = NULL,
  include_accessibility = TRUE,
  quiet = NULL,
  ...
)
```

## Arguments

- previous_stage:

  A tibble or list output from an earlier BID stage function.

- bias_mitigations:

  A named list of bias mitigation strategies. If NULL, the function will
  suggest bias mitigations based on information from previous stages.

- include_accessibility:

  Logical indicating whether to include accessibility mitigations.
  Default is TRUE.

- quiet:

  Logical indicating whether to suppress informational messages. If
  NULL, uses getOption("bidux.quiet", FALSE).

- ...:

  Additional parameters. If 'interaction_principles' is provided, it
  will be ignored with a warning.

## Value

A tibble containing the documented information for the "Anticipate"
stage.

## Examples

``` r
interpret_stage <- bid_interpret(
  central_question = "How can we improve selection efficiency?",
  data_story = new_data_story(
    hook = "Too many options",
    context = "Excessive choices",
    tension = "User frustration",
    resolution = "Simplify menu"
  )
)
#> Stage 1 (Interpret) completed.
#>   - Central question: How can we improve selection efficiency?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined

notice_stage <- bid_notice(
  previous_stage = interpret_stage,
  problem = "Issue with dropdown menus",
  evidence = "User testing indicated delays"
)
#> Auto-suggested theory: Hick's Law (confidence: 90%)
#> Stage 2 (Notice) completed. (40% complete)
#>   - Problem: Issue with dropdown menus
#>   - Theory: Hick's Law (auto-suggested)
#>   - Evidence: User testing indicated delays
#>   - Theory confidence: 90%
#>   - Next: Use bid_anticipate() for Stage 3

structure_info <- bid_structure(previous_stage = notice_stage)
#> ℹ Tip: Learn more about any concept via bid_concept("<concept>").
#> ℹ Stage numbering has been corrected in bidux 0.3.1:
#>   Anticipate is now Stage 3, Structure is now Stage 4
#>   This change improves logical workflow progression
#>   All existing code remains backward compatible
#> Stage 4 (Structure) completed.
#>   - Concept groups generated: 4
#>   - Total concepts: 4

# Let the function suggest bias mitigations based on previous stages
bid_anticipate(previous_stage = structure_info)
#> ℹ Automatically suggested bias mitigations: attention bias, belief perseverance, accessibility.
#> Concept "attention bias" not found.
#> Found partial match: "Accessibility Contrast".
#> Stage 3 (Anticipate) completed.
#>   - Bias mitigations: 3 defined
#>   - Accessibility considerations included
#>   - Key suggestions: Attention Bias mitigation: Consider how this bias affects user decisions, Belief Perseverance mitigation: Proactively show content that might disprove initial assumptions, Accessibility mitigation: Test color combinations with WebAIM's contrast checker to meet WCAG standards
#> BID Framework - Anticipate Stage
#> Generated: 2026-02-27 22:06:49 
#> Progress: 60 % (3/5) 
#> 
#> Bias Mitigations: 2 strategies defined 
#> 
#>  Suggestions: Attention Bias mitigation: Consider how this bias affects user decisions Belief Perseverance mitigation: Proactively show content that might disprove initial assumptions Accessibility mitigation: Test color combinations with WebAIM's contrast checker to meet WCAG standards Consider also addressing these common biases: anchoring, framing, confirmation Accessibility considerations have been included in bias mitigations 
#> 
#>  Use summary() for detailed information 

# with accessibility included (default) and custom bias mitigations
anticipate_result <- bid_anticipate(
  previous_stage = structure_info,
  bias_mitigations = list(
    anchoring = "Use context-aware references",
    framing = "Toggle between positive and negative framing"
  ),
  include_accessibility = TRUE
)
#> Warning: ! Using deprecated list format for bias_mitigations parameter
#> ℹ Please use new_bias_mitigations() constructor for new code
#> ℹ Legacy format will be automatically migrated
#> ℹ Added accessibility mitigation based on layout context.
#> Concept "bias_type" not found.
#> Concept "mitigation_strategy" not found.
#> Concept "confidence_level" not found.
#> Found partial match: "Accessibility Contrast".
#> Stage 3 (Anticipate) completed.
#>   - Bias mitigations: 4 defined
#>   - Accessibility considerations included
#>   - Key suggestions: Bias_type mitigation: Consider how this bias affects user decisions, Mitigation_strategy mitigation: Consider how this bias affects user decisions, Confidence_level mitigation: Consider how this bias affects user decisions

summary(anticipate_result)
#> === BID Framework: Anticipate Stage Summary ===
#> 
#> Metadata:
#>    stage_number : 3 
#>    total_stages : 5 
#>    validation_status : completed 
#>    bias_count : 4 
#>    include_accessibility : Yes 
#>    layout : NA 
#>    concepts_count : 4 
#>    auto_generated_biases : No 
#>    stage_number_previous : 4 
#> 
#> Stage Data:
#>    stage : Anticipate 
#>    bias_mitigations : anchoring: Use context-aware references; framing: Toggle between positive and... 
#>    accessibility : accessibility mitigation not specified 
#>    previous_concepts : Cognitive Load Theory, Visual Hierarchy, Progressive Disclosure, Hick's Law 
#>    suggestions : Bias_type mitigation: Consider how this bias affects user decisions Mitigatio... 
#> 
#> Generated: 2026-02-27 22:06:49 
```
