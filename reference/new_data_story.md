# Create a data story object

Creates a structured data story object for use in bid_interpret() and
other bidux functions. The flattened API (hook, context, tension,
resolution) is recommended for most users. The nested format (variables,
relationships) is still supported for backward compatibility.

## Usage

``` r
new_data_story(
  hook = NULL,
  context = NULL,
  tension = NULL,
  resolution = NULL,
  variables = NULL,
  relationships = NULL,
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

- variables:

  DEPRECATED. List of variable descriptions (use flat arguments instead)

- relationships:

  DEPRECATED. List describing data relationships (use flat arguments
  instead)

- ...:

  Optional additional fields (audience, metrics, visual_approach, etc.)

## Value

A bid_data_story S3 object

## Examples

``` r
# Recommended: flat API
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
