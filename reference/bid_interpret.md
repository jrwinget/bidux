# Document User Interpretation Stage in BID Framework

This function documents the interpretation of user needs, capturing the
central question and the data storytelling narrative. It represents
stage 1 in the BID framework.

## Usage

``` r
bid_interpret(
  previous_stage = NULL,
  central_question,
  data_story = NULL,
  user_personas = NULL,
  quiet = NULL
)
```

## Arguments

- previous_stage:

  Optional tibble or list output from an earlier BID stage function.
  Since Interpret is the first stage in the BID framework, this is
  typically NULL but can accept previous stage output in some iteration
  scenarios.

- central_question:

  Required. A character string representing the main question to be
  answered. If NULL, will be suggested based on previous stage
  information.

- data_story:

  A list containing elements such as `hook`, `context`, `tension`,
  `resolution`, and optionally `audience`, `metrics`, and
  `visual_approach`. If NULL, elements will be suggested based on
  previous stage.

- user_personas:

  Optional list of user personas to consider in the design.

- quiet:

  Logical indicating whether to suppress informational messages. If
  NULL, uses getOption("bidux.quiet", FALSE).

## Value

A tibble containing the documented information for the "Interpret"
stage.

## Examples

``` r
# Recommended: use new_data_story() with flat API
interpret_result <- bid_interpret(
  central_question = "What drives the decline in user engagement?",
  data_story = new_data_story(
    hook = "Declining trend in engagement",
    context = "Previous high engagement levels",
    tension = "Unexpected drop",
    resolution = "Investigate new UI changes"
  )
)
#> Stage 1 (Interpret) completed.
#>   - Central question: What drives the decline in user engagement?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined 

# With user personas (using data.frame)
interpret_personas <- bid_interpret(
  central_question = "How can we improve data discovery?",
  data_story = new_data_story(
    hook = "Users are missing key insights",
    context = "Critical data is available but overlooked",
    tension = "Time-sensitive decisions are delayed",
    resolution = "Highlight key metrics more effectively",
    audience = "Data analysts and executives"
  ),
  user_personas = data.frame(
    name = c("Sara, Data Analyst", "Marcus, Executive"),
    goals = c(
      "Needs to quickly find patterns in data",
      "Wants high-level insights at a glance"
    ),
    pain_points = c(
      "Gets overwhelmed by too many visualizations",
      "Limited time to analyze detailed reports"
    ),
    technical_level = c("advanced", "beginner"),
    stringsAsFactors = FALSE
  )
)
#> Stage 1 (Interpret) completed.
#>   - Central question: How can we improve data discovery?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - User personas: 4 defined 

summary(interpret_personas)
#> === BID Framework: Interpret Stage Summary ===
#> 
#> Metadata:
#>    stage_number : 1 
#>    total_stages : 5 
#>    validation_status : completed 
#>    has_central_question : Yes 
#>    story_completeness : 1 
#>    personas_count : 4 
#>    auto_generated_question : No 
#>    auto_generated_story : No 
#> 
#> Stage Data:
#>    stage : Interpret 
#>    central_question : How can we improve data discovery? 
#>    hook : Users are missing key insights 
#>    context : Critical data is available but overlooked 
#>    tension : Time-sensitive decisions are delayed 
#>    resolution : Highlight key metrics more effectively 
#>    audience : Data analysts and executives 
#>    personas : [{"name":"Sara, Data Analyst","goals":"Needs to quickly find patterns in data... 
#>    suggestions : Your data story has all key elements. Focus on making each component compelli... 
#> 
#> Generated: 2025-11-19 21:33:47 

# Legacy list format still works (with deprecation warning)
if (FALSE) { # \dontrun{
interpret_legacy <- bid_interpret(
  central_question = "How can we improve UX?",
  data_story = list(
    hook = "Users struggling",
    context = "Dashboard complexity",
    tension = "High abandonment rate",
    resolution = "Simplify interface"
  )
)
} # }
```
