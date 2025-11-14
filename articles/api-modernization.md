# bidux API Modernization Features

## Introduction

The bidux package has been modernized with enhanced API consistency,
better error handling, and optional tibble support. **This vignette is
the definitive guide to modern API usage in bidux 0.3.2+**.

### What’s New in 0.3.2

| Feature                      | Status             | Details                                                          |
|------------------------------|--------------------|------------------------------------------------------------------|
| **Flat data_story API**      | ✅ **RECOMMENDED** | Use `new_data_story(hook, context, tension, resolution)`         |
| **Nested data_story format** | ⚠️ **DEPRECATED**  | `variables`/`relationships` parameters removed in 0.4.0          |
| **Telemetry presets**        | ✅ **NEW**         | Use `bid_telemetry_presets("moderate")` for easier configuration |
| **S3 classes and methods**   | ✅ **STABLE**      | Enhanced print, summary, and conversion methods                  |
| **Tibble integration**       | ✅ **OPTIONAL**    | Automatic tibble output when package available                   |

### Quick Migration Guide

``` r
# DEPRECATED (will be removed in 0.4.0)
story <- new_data_story(
  context = "Dashboard usage dropped",
  variables = list(hook = "Engagement declining"),
  relationships = list(resolution = "Analyze telemetry")
)

# RECOMMENDED (0.3.2+)
story <- new_data_story(
  hook = "Engagement declining",
  context = "Dashboard usage dropped 30%",
  tension = "Unknown if UX or user needs",
  resolution = "Analyze telemetry to identify friction"
)
```

``` r
library(bidux)
# Optional tibble support
if (requireNamespace("tibble", quietly = TRUE)) {
  library(tibble)
}
```

## Enhanced Data Story Creation

The
[`new_data_story()`](https://jrwinget.github.io/bidux/reference/new_data_story.md)
function provides a structured way to create behavioral insights. **As
of bidux 0.3.2, the flat API is the RECOMMENDED approach** for creating
data stories.

### Recommended: Flat API (0.3.2+)

The flat API accepts four intuitive arguments that map directly to
storytelling elements:

``` r
# RECOMMENDED: Create a structured data story using the modern flat API
story <- new_data_story(
  hook = "Users struggle with complex form layouts leading to abandonment",
  context = "User Interface Cognitive Load Analysis for complex form layouts with 40% improvement potential",
  tension = "Current forms overwhelm users with cognitive load and lack clear guidance",
  resolution = "Implement step-by-step form progression with visual progress indicators"
)

# Enhanced print method
print(story)

# Access story elements directly using the flat API
story$hook
story$context
story$tension
story$resolution

# Story is automatically structured for further BID framework usage
```

### Deprecated: Nested Format (Removal in 0.4.0)

The nested format using `variables` and `relationships` parameters is
**DEPRECATED** and will be removed in bidux 0.4.0.

``` r
# DEPRECATED: Nested format (will be removed in 0.4.0)
# This will trigger a deprecation warning
story_old <- new_data_story(
  context = "Dashboard usage analysis",
  variables = list(
    metric = "engagement",
    trend = "declining"
  ),
  relationships = list(
    correlation = "negative_with_complexity"
  )
)
# Warning: Using deprecated nested format for data_story
# The flat API is now recommended: new_data_story(hook, context, tension, resolution)
# Nested format (variables, relationships) will be removed in bidux 0.4.0
```

### Migration Guide

**Old nested format:**

``` r
# OLD (deprecated)
story <- new_data_story(
  context = "Dashboard usage dropped",
  variables = list(hook = "User engagement declining"),
  relationships = list(resolution = "Analyze telemetry")
)
```

**New flat format:**

``` r
# NEW (recommended)
story <- new_data_story(
  hook = "User engagement declining",
  context = "Dashboard usage dropped 30%",
  tension = "Don't know if UX or user needs",
  resolution = "Analyze telemetry"
)
```

## Modern BID Framework Interpretation

The enhanced
[`bid_interpret()`](https://jrwinget.github.io/bidux/reference/bid_interpret.md)
function captures user needs and central questions with comprehensive
validation and modern S3 methods. **Always use the recommended flat API
for data_story objects.**

``` r
# Basic interpretation with modern flat API (RECOMMENDED)
result <- bid_interpret(
  central_question = "How can we reduce cognitive load in our signup form?",
  data_story = new_data_story(
    hook = "Users abandon signup forms at 60% rate",
    context = "Current form has 15 required fields",
    tension = "Users feel overwhelmed and leave",
    resolution = "Simplify form using progressive disclosure"
  ),
  quiet = FALSE
)

# View results
print(result)

# Enhanced interpretation with user personas
interpretation <- bid_interpret(
  central_question = "What drives user engagement in our dashboard?",
  data_story = new_data_story(
    hook = "Daily active users declining despite new features",
    context = "Rich dashboard with multiple visualizations",
    tension = "Users aren't discovering valuable insights",
    resolution = "Guide attention to high-value content"
  ),
  user_personas = list(
    list(
      name = "Data Analyst",
      goals = "Find patterns quickly",
      pain_points = "Too many visualizations",
      technical_level = "Advanced"
    )
  ),
  quiet = TRUE
)

# Enhanced S3 print method shows summary
print(interpretation)
```

**Note:** While
[`bid_interpret()`](https://jrwinget.github.io/bidux/reference/bid_interpret.md)
still accepts plain list format for `data_story` for backward
compatibility, using
[`new_data_story()`](https://jrwinget.github.io/bidux/reference/new_data_story.md)
with the flat API is recommended for clarity and consistency.

## Structured Dashboard Layout with bid_structure

Transform insights into actionable dashboard structure with the enhanced
`bid_structure` function that automatically selects optimal layout.

``` r
# Follow correct BID framework order: Interpret → Notice → Anticipate → Structure
notice_result <- result |>
  bid_notice(
    problem = "Users struggle with cognitive overload in signup forms",
    evidence = "60% abandonment rate and user feedback surveys",
    quiet = TRUE
  )

# Anticipate potential user interactions and biases
anticipate_result <- notice_result |>
  bid_anticipate(
    interaction_modes = list(
      primary = "Progressive form completion",
      fallback = "Quick signup option"
    ),
    quiet = TRUE
  )

# Structure creates actionable dashboard recommendations
structured <- bid_structure(
  previous_stage = anticipate_result,
  concepts = c("Progressive Disclosure", "Cognitive Load Theory"),
  quiet = TRUE
)

# View auto-selected layout and concept-grouped suggestions
print(paste("Auto-selected layout:", structured$layout))
print("Structured recommendations:")
print(structured$suggestions)

# Summary provides overview of all recommendations
summary(structured)
```

## Complete Workflow Example

Demonstrate the full modernized BID framework workflow with consistent
API patterns using the **RECOMMENDED flat API throughout**.

``` r
# Step 1: Interpret user needs and context (using RECOMMENDED flat API)
ui_analysis <- bid_interpret(
  central_question = "How can we improve landing page conversion without damaging user trust?",
  data_story = new_data_story(
    hook = "High-pressure sales tactics may be reducing user trust",
    context = "Landing page with multiple persuasive elements",
    tension = "Need conversions but not at expense of user experience",
    resolution = "Balance persuasion with trust-building elements"
  ),
  user_personas = list(
    list(
      name = "Potential Customer",
      goals = "Evaluate product value",
      pain_points = "Feels pressured by aggressive sales tactics",
      technical_level = "Basic"
    )
  ),
  quiet = TRUE
)

# Step 2: Notice specific problems
problem_analysis <- ui_analysis |>
  bid_notice(
    problem = "Aggressive persuasion tactics creating user pressure",
    evidence = "Lower conversion rates and negative user feedback on pressure tactics",
    quiet = TRUE
  )

# Step 3: Anticipate user interactions and cognitive biases
interaction_analysis <- problem_analysis |>
  bid_anticipate(
    interaction_modes = list(
      primary = "Trust-based evaluation of product value",
      secondary = "Quick decision under time pressure"
    ),
    quiet = TRUE
  )

# Step 4: Structure actionable recommendations
recommendations <- bid_structure(
  previous_stage = interaction_analysis,
  concepts = c("Social Proof", "Trust Signals", "Progressive Disclosure"),
  quiet = TRUE
)

# Step 5: Validate design decisions
validation_analysis <- bid_validate(
  previous_stage = recommendations,
  validation_method = "expert_review",
  quiet = TRUE
)

# Step 6: Create comprehensive data story from complete workflow using flat API
optimization_story <- new_data_story(
  hook = "High-pressure persuasive elements creating user trust concerns",
  context = "Complete BID Framework Analysis: Landing Page Trust-Based Conversion Optimization across all 5 stages",
  tension = "Need to balance conversion goals with user trust - current approach damages long-term relationships",
  resolution = "Apply validated BID framework recommendations balancing persuasion with trust-building, monitoring both conversion rates and user trust metrics"
)

# View complete analysis results
print("=== Complete BID Framework Workflow Results ===")
print(optimization_story)
print("\n=== Stage Summaries ===")
summary(recommendations)
summary(validation_analysis)
```

## Error Handling and Validation

The modernized API provides clear, descriptive error messages for better
debugging.

``` r
# Clear error messages for invalid inputs
try(bid_interpret()) # Missing required parameter
try(bid_interpret(central_question = 123)) # Wrong type for central_question
try(bid_interpret(central_question = "test", data_story = "invalid")) # Invalid data_story

# Descriptive validation errors for data story creation (flat API)
try(new_data_story()) # Missing required parameter (context)
try(new_data_story(context = "")) # Invalid empty context

# Deprecated format still validates (but triggers warnings)
try(new_data_story(
  context = "Valid context",
  variables = "invalid_type"  # Must be a list
))
```

### Deprecation Warnings

When using the deprecated nested format, you’ll see helpful warnings:

``` r
# Using deprecated nested format triggers warning
story <- new_data_story(
  context = "Dashboard analysis",
  variables = list(metric = "engagement"),
  relationships = list(trend = "declining")
)
# Warning message:
# ! Using deprecated nested format for data_story
# ℹ The flat API is now recommended: new_data_story(hook, context, tension, resolution)
# ℹ Nested format (variables, relationships) will be removed in bidux 0.4.0
```

## Telemetry Sensitivity Presets (New in 0.3.2)

The
[`bid_telemetry_presets()`](https://jrwinget.github.io/bidux/reference/bid_telemetry_presets.md)
function provides pre-configured threshold sets for easier telemetry
analysis configuration, eliminating the need for manual threshold
tuning.

### Available Presets

``` r
# Three sensitivity levels available:
# - "strict": Detects even minor issues (for critical apps or new dashboards)
# - "moderate": Balanced default (appropriate for most applications)
# - "relaxed": Only detects major issues (for mature, stable dashboards)

# Get strict sensitivity thresholds
strict_thresholds <- bid_telemetry_presets("strict")
print(strict_thresholds)

# Get moderate sensitivity (default)
moderate_thresholds <- bid_telemetry_presets("moderate")

# Get relaxed sensitivity
relaxed_thresholds <- bid_telemetry_presets("relaxed")
```

### Using Presets with Telemetry Analysis

``` r
# Use strict preset for critical application
issues <- bid_ingest_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("strict")
)

# Use relaxed preset for mature dashboard
issues <- bid_ingest_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("relaxed")
)

# Override specific thresholds while using a preset as base
custom_thresholds <- bid_telemetry_presets("moderate")
custom_thresholds$unused_input_threshold <- 0.03  # More sensitive to unused inputs
issues <- bid_ingest_telemetry("telemetry.sqlite", thresholds = custom_thresholds)
```

### Threshold Comparison

| Threshold              | Strict | Moderate | Relaxed | Description                               |
|------------------------|--------|----------|---------|-------------------------------------------|
| unused_input_threshold | 0.02   | 0.05     | 0.10    | Min usage rate to flag input              |
| delay_threshold_secs   | 20     | 30       | 60      | Seconds before flagging delay             |
| error_rate_threshold   | 0.05   | 0.10     | 0.20    | Error rate to flag problems               |
| navigation_threshold   | 0.10   | 0.20     | 0.30    | Min visit rate for pages                  |
| rapid_change_window    | 15     | 10       | 5       | Time window for confusion detection (sec) |
| rapid_change_count     | 4      | 5        | 7       | Changes in window to flag confusion       |

**Recommendation:** Start with `"moderate"` preset and adjust based on
your application’s maturity and criticality. Use `"strict"` for new
features or critical workflows, and `"relaxed"` for well-established
interfaces.

## Tibble Integration

When the tibble package is available, all functions can return modern
tibble objects.

``` r
# Check if tibble is available
if (requireNamespace("tibble", quietly = TRUE)) {
  # BID framework functions return tibbles by default when available
  # Using RECOMMENDED flat API
  result_tbl <- bid_interpret(
    central_question = "How can we test tibble integration?",
    data_story = new_data_story(
      hook = "Package supports modern tibble output",
      context = "Enhanced data handling with tibble package",
      tension = "Need to verify integration works correctly",
      resolution = "Test and validate tibble functionality"
    ),
    quiet = TRUE
  )

  cat("Result class:", class(result_tbl), "\n")

  # Works seamlessly with dplyr if available
  if (requireNamespace("dplyr", quietly = TRUE)) {
    library(dplyr)

    # Filter and analyze BID framework results
    interpret_summary <- result_tbl %>%
      select(stage, central_question, hook) %>%
      filter(!is.na(central_question))

    print(interpret_summary)
  }
} else {
  cat("Tibble package not available, using base data.frame\n")
}
```

## API Consistency Features

All core functions now follow consistent patterns for better usability.

``` r
# Consistent parameter patterns across functions
bid_interpret_params <- c("text", "method", "context", "return_tibble", "quiet")
bid_structure_params <- c("insights", "format", "priority", "context", "return_tibble", "quiet")

# Common parameters:
# - return_tibble: Enable modern tibble output
# - quiet: Control informational messages
# - context: Provide analysis context

# All functions return objects with:
# - S3 classes for proper method dispatch
# - Timestamp columns for tracking
# - Consistent attribute structures
# - Modern data types (tibble when available)
```

## Backward Compatibility

All enhancements maintain full backward compatibility - existing code
continues to work with deprecation warnings where appropriate.

``` r
# Legacy nested format still works (with deprecation warning)
old_style_story <- new_data_story(
  context = "Dashboard analysis",
  variables = list(metric = "engagement"),
  relationships = list(trend = "declining")
)
# Triggers deprecation warning but functions correctly

# Modern flat API is recommended
new_style_story <- new_data_story(
  hook = "Engagement metrics show concerning trends",
  context = "Dashboard analysis of Q3 2024 usage patterns",
  tension = "User engagement declining despite new features",
  resolution = "Implement BID framework to identify UX friction"
)

# Both can be used with bid_interpret()
result_old <- bid_interpret(
  central_question = "How to improve engagement?",
  data_story = old_style_story,
  quiet = TRUE
)

result_new <- bid_interpret(
  central_question = "How to improve engagement?",
  data_story = new_style_story,
  quiet = TRUE
)

# Both return compatible structures
identical(names(result_old), names(result_new))
```

### Migration Strategy

For codebases with existing nested data_story usage:

1.  **Immediate (0.3.2)**: Code continues to work with deprecation
    warnings
2.  **Short-term**: Update to flat API following warning guidance
3.  **Before 0.4.0**: Complete migration required (nested format will be
    removed)

Example migration:

``` r
# Step 1: Identify all new_data_story() calls with variables/relationships
# Step 2: Extract nested values and flatten

# OLD CODE
story <- new_data_story(
  context = "User study results",
  variables = list(
    hook = "Users struggle with feature discovery",
    metric = "task_completion_rate",
    value = 0.42
  ),
  relationships = list(
    resolution = "Redesign navigation with progressive disclosure"
  )
)

# NEW CODE (flattened)
story <- new_data_story(
  hook = "Users struggle with feature discovery",
  context = "User study results: 42% task completion rate",
  tension = "Low discoverability preventing user success",
  resolution = "Redesign navigation with progressive disclosure",
  # Optional metadata for extra fields
  metric = "task_completion_rate",
  value = 0.42
)
```

## Summary

The modernized bidux API (0.3.2+) provides:

### Core Modernizations

- **Flat data_story API (RECOMMENDED)**: Intuitive
  `new_data_story(hook, context, tension, resolution)` replaces nested
  format
- **Telemetry sensitivity presets**: Pre-configured threshold sets
  (`strict`, `moderate`, `relaxed`) via
  [`bid_telemetry_presets()`](https://jrwinget.github.io/bidux/reference/bid_telemetry_presets.md)
- **Enhanced S3 classes** with comprehensive method implementations
- **Optional tibble support** for modern data handling
- **Robust parameter validation** with clear error messages
- **Consistent function interfaces** across all core functions

### Deprecation Timeline

- **Nested data_story format** (variables/relationships): Deprecated in
  0.3.2, removal in 0.4.0
- Clear migration warnings guide users to recommended patterns
- All legacy formats continue to work during deprecation period

### Best Practices for 0.3.2+

1.  **Always use flat API for data stories**:
    `new_data_story(hook, context, tension, resolution)`
2.  **Use telemetry presets**: Start with
    `bid_telemetry_presets("moderate")` instead of manual thresholds
3.  **Leverage S3 methods**: Use
    [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html), and
    [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
    for enhanced output
4.  **Follow modern patterns**: Embrace pipe workflows and tidy data
    principles

### Backward Compatibility

- **Full backward compatibility** maintained for all existing code
- Legacy formats trigger helpful deprecation warnings
- Migration examples provided throughout documentation
- Smooth transition path to modern patterns

These improvements make the package more robust, user-friendly, and
suitable for modern R workflows while maintaining all existing
functionality during the transition period.
