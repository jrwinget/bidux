# Create a data story object

Creates a structured data story object for use in bid_interpret() and
other bidux functions. Uses a flat API with hook, context, tension, and
resolution fields to structure your data narrative.

## Usage

``` r
new_data_story(
  hook = NULL,
  context = NULL,
  tension = NULL,
  resolution = NULL,
  ...
)
```

## Arguments

- hook:

  Character string for the story hook (attention-grabbing opening)

- context:

  Character string describing the data context

- tension:

  Character string describing the problem or tension

- resolution:

  Character string describing the resolution or next steps

- ...:

  Optional additional fields (audience, metrics, visual_approach, etc.)

## Value

A bid_data_story S3 object

## Examples

``` r
# Basic usage
story <- new_data_story(
  hook = "User engagement is declining",
  context = "Our dashboard usage has dropped 30% this quarter",
  tension = "We don't know if it's UX issues or changing user needs",
  resolution = "Analyze telemetry to identify friction points"
)

# With optional fields
story_detailed <- new_data_story(
  hook = "Revenue dashboards are underutilized",
  context = "Only 40% of sales team uses the new revenue dashboard",
  tension = "Critical metrics are being missed",
  resolution = "Redesign with behavioral science principles",
  audience = "Sales team",
  metrics = "adoption_rate, time_to_insight"
)
```
