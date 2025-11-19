# Quick Start: Improve Your Shiny Dashboard in 15 Minutes

``` r
library(bidux)
library(shiny)
library(bslib)
```

## What This Guide Covers

You’re a Shiny developer. You’ve built dashboards that work, but users
complain they’re “confusing” or “overwhelming.” You know something’s
wrong, but you’re not a UX designer.

**Good news:** You don’t need to be. This 15-minute guide shows you how
to use bidux to get concrete, actionable suggestions for improving your
dashboard using behavioral science principles.

**You’ll learn:**

- How to quickly diagnose common UX problems (5 min)
- How to get instant improvement suggestions (3 min)
- How to analyze telemetry data for real user issues (4 min)
- When to use the full BID framework (3 min)

**Who this is for:** Shiny developers who understand `fluidRow()` and
`selectInput()` but aren’t UX experts.

## The Fast Path: Common Problems, Quick Solutions

Let’s start with the three most common dashboard problems and how bidux
helps you fix them.

### Problem 1: “Users Are Overwhelmed by Too Many Filters”

You’ve added filter after filter because stakeholders asked for them.
Now your sidebar is a mile long and users are paralyzed.

**Quick diagnosis with bidux:**

``` r
# Document what you're observing
filter_problem <- bid_notice(
  previous_stage = bid_interpret(
    central_question = "Why aren't users finding insights in my dashboard?"
  ),
  problem = "Users are overwhelmed by too many filter options",
  evidence = "12 filter controls in sidebar, user testing shows 70% give up within 2 minutes"
)

# bidux automatically suggests the right theory
print(filter_problem$theory) # "Cognitive Load Theory"
```

**Get structure suggestions:**

``` r
# Get layout recommendations
structure_result <- bid_structure(previous_stage = filter_problem)

# bidux recommends "breathable" layout to reduce cognitive load
print(structure_result$layout)
print(structure_result$layout_rationale)

# See concrete suggestions organized by UX concept
View(structure_result$suggestions)
```

**Apply the fix:**

``` r
# BEFORE: All filters visible at once
ui_before <- sidebarPanel(
  dateRangeInput("dates", "Date Range"),
  selectInput("region", "Region", choices = regions),
  selectInput("product", "Product", choices = products),
  selectInput("segment", "Customer Segment", choices = segments),
  selectInput("channel", "Marketing Channel", choices = channels),
  numericInput("min_revenue", "Min Revenue", 0),
  numericInput("max_revenue", "Max Revenue", 1000000),
  checkboxGroupInput("status", "Status", choices = statuses),
  radioButtons("currency", "Currency", choices = c("USD", "EUR")),
  sliderInput("confidence", "Confidence Level", 0, 100, 95),
  selectInput("aggregation", "Aggregation", choices = c("Daily", "Weekly")),
  checkboxInput("include_forecast", "Include Forecast")
)

# AFTER: Progressive disclosure with smart defaults
ui_after <- sidebarPanel(
  # Core filters only (based on bid_structure suggestions)
  h4("Quick Filters"),
  dateRangeInput("dates", "Date Range", start = Sys.Date() - 30, end = Sys.Date()),
  selectInput("region", "Region", choices = c("All", regions), selected = "All"),

  # Progressive disclosure for advanced options
  actionButton("show_advanced", "More Filters...", class = "btn btn-sm btn-outline-secondary"),
  conditionalPanel(
    condition = "input.show_advanced % 2 == 1",
    h5("Advanced Filters", style = "margin-top: 15px;"),
    selectInput("product", "Product", choices = c("All", products)),
    selectInput("segment", "Customer Segment", choices = c("All", segments)),
    selectInput("channel", "Marketing Channel", choices = c("All", channels)),

    # Even more advanced in accordion
    bslib::accordion(
      bslib::accordion_panel(
        "Revenue Range",
        numericInput("min_revenue", "Minimum", 0),
        numericInput("max_revenue", "Maximum", 1000000)
      ),
      bslib::accordion_panel(
        "Display Options",
        radioButtons("currency", "Currency", choices = c("USD", "EUR")),
        selectInput("aggregation", "Aggregation", choices = c("Daily", "Weekly")),
        checkboxInput("include_forecast", "Include Forecast")
      )
    )
  )
)
```

**Result:** Cognitive load reduced, users see 3 filters instead of 12,
advanced options available when needed.

### Problem 2: “Dashboard Takes Too Long to Understand”

Users open your dashboard and stare at it. They don’t know where to look
first or what’s important.

**Quick diagnosis:**

``` r
# Document the navigation confusion
nav_problem <- bid_notice(
  previous_stage = bid_interpret(
    central_question = "What's the most important insight users need?"
  ),
  problem = "Users don't know where to look first - no clear visual hierarchy",
  evidence = "Dashboard shows 8 KPIs, 4 charts, 3 tables simultaneously with equal visual weight"
)

# Get suggestions focused on visual hierarchy
structure_nav <- bid_structure(previous_stage = nav_problem)

# Check which UX concepts bidux identified as relevant
structure_nav$concepts
```

**Apply the fix:**

``` r
# BEFORE: Everything has equal visual weight
ui_flat <- fluidRow(
  column(3, valueBoxOutput("metric1")),
  column(3, valueBoxOutput("metric2")),
  column(3, valueBoxOutput("metric3")),
  column(3, valueBoxOutput("metric4")),
  column(3, valueBoxOutput("metric5")),
  column(3, valueBoxOutput("metric6")),
  column(3, valueBoxOutput("metric7")),
  column(3, valueBoxOutput("metric8"))
)

# AFTER: Clear hierarchy with primary insight first
ui_hierarchy <- layout_columns(
  # Primary insight takes visual priority
  card(
    full_screen = TRUE,
    card_header(
      "Revenue Performance",
      class = "bg-primary text-white"
    ),
    layout_columns(
      col_widths = c(6, 6),
      value_box(
        title = "This Month",
        value = "$1.2M",
        showcase = bs_icon("graph-up", size = "3em"),
        theme = "success",
        p("↑ 22% vs target", style = "font-size: 1.1em;")
      ),
      div(
        style = "padding: 20px;",
        h4("Key Drivers"),
        tags$ul(
          tags$li("Enterprise sales up 35%"),
          tags$li("Customer retention at 94%"),
          tags$li(tags$span(
            style = "color: #dc3545;",
            "⚠ SMB segment declining"
          ))
        )
      )
    )
  ),

  # Secondary metrics with less visual weight
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box("New Customers", "156", theme = "info"),
    value_box("Avg Deal Size", "$7.8K", theme = "info"),
    value_box("Win Rate", "34%", theme = "warning")
  )
)
```

**Result:** Users immediately see what matters, secondary details
available but not competing for attention.

### Problem 3: “Navigation Is Confusing”

You have 6 tabs because you have 6 different analyses. Users get lost
and never find the analysis they need.

**Quick diagnosis with telemetry:**

If you have telemetry data (from `shiny.telemetry` or similar), bidux
can identify this automatically:

``` r
# Analyze real user behavior
issues <- bid_telemetry(
  "path/to/telemetry.sqlite",
  thresholds = bid_telemetry_presets("moderate")
)

# Filter to navigation issues
nav_issues <- issues[issues$issue_type == "navigation_dropoff", ]
print(nav_issues)

# Convert to BID Notice stage
notices <- bid_notices(
  nav_issues,
  previous_stage = bid_interpret(
    central_question = "Why aren't users exploring all dashboard sections?"
  ),
  max_issues = 2
)

# Continue with structure suggestions
structure_nav <- bid_structure(previous_stage = notices[[1]])
```

**Apply the fix:**

``` r
# BEFORE: Generic tab labels, no guidance
ui_confusing <- tabsetPanel(
  tabPanel("Overview", "..."),
  tabPanel("Analysis", "..."),
  tabPanel("Details", "..."),
  tabPanel("Advanced", "..."),
  tabPanel("Reports", "..."),
  tabPanel("Settings", "...")
)

# AFTER: Clear information scent, task-oriented organization
ui_clear <- page_navbar(
  title = "Sales Analytics",

  # Primary task first
  nav_panel(
    "🎯 What Needs Attention",
    "At-risk deals and opportunities requiring immediate action"
  ),

  # Secondary analysis grouped logically
  nav_menu(
    "📊 Performance Analysis",
    nav_panel("By Region", "Regional breakdown"),
    nav_panel("By Product", "Product performance"),
    nav_panel("By Team", "Sales team metrics")
  ),

  # Supporting functions in separate menu
  nav_menu(
    "⚙ Tools",
    nav_panel("Export Reports", "Generate reports"),
    nav_panel("Settings", "Configure dashboard")
  )
)
```

**Result:** Clear task-oriented navigation, users find what they need
without hunting.

## Quick Telemetry Analysis

If you have telemetry data, bidux can automatically identify UX problems
from real user behavior.

### Step 1: Analyze Your Telemetry Data

``` r
# Use moderate sensitivity for most dashboards
issues <- bid_telemetry(
  "telemetry.sqlite",
  thresholds = bid_telemetry_presets("moderate")
)

# Review identified issues
print(issues)

# Output shows:
# - Issue types (unused inputs, delays, errors, navigation, confusion)
# - Severity levels (critical, high, medium, low)
# - Impact rates (% of sessions affected)
```

### Step 2: Focus on High-Impact Issues

``` r
library(dplyr)

# Filter to critical and high severity
priority_issues <- issues |>
  filter(severity %in% c("critical", "high")) |>
  arrange(desc(impact_rate))

# See top 3 issues by impact
head(priority_issues, 3)

# Example output:
# issue_id: unused_input_advanced_filter
# severity: high
# impact_rate: 0.92 (92% of sessions never used it)
# problem: "Users are not interacting with the 'advanced_filter' input control"
```

### Step 3: Convert Issues to BID Workflow

``` r
# Start with interpret stage
interpret_stage <- bid_interpret(
  central_question = "How can we reduce friction points identified in telemetry?"
)

# Convert top issues to Notice stages
notices <- bid_notices(
  priority_issues,
  previous_stage = interpret_stage,
  max_issues = 3
)

# Address the most critical issue
top_notice <- notices[[1]]

# Get structure suggestions for this specific issue
structure_result <- bid_structure(
  previous_stage = top_notice,
  telemetry_flags = bid_flags(issues) # Informs layout selection
)

# Review suggestions
print(structure_result$layout)
print(structure_result$layout_rationale)
```

### Step 4: Use Flags for Decision Making

``` r
# Extract global insights
flags <- bid_flags(issues)

# Use flags to inform your approach
if (flags$has_confusion_patterns) {
  message("Consider simplifying unclear inputs")
}

if (flags$has_navigation_issues) {
  message("Review tab/page organization")
}

if (flags$has_delay_issues) {
  message("Users taking too long to engage - review initial view")
}

# See full flag details
str(flags)
```

## When to Use the Full Framework

The quick approaches above work great for isolated problems. Use the
**full 5-stage BID framework** when:

### You’re Building a New Dashboard

Document your design decisions systematically from the start:

``` r
# Stage 1: Interpret - Understand your users
interpret <- bid_interpret(
  central_question = "How can marketing managers optimize campaign spend?",
  data_story = new_data_story(
    hook = "Campaign ROI varies significantly across channels",
    context = "Marketing team managing $2M annual budget across 6 channels",
    tension = "Current reporting is fragmented, makes optimization difficult",
    resolution = "Unified view with clear recommendations"
  ),
  user_personas = data.frame(
    name = "Marketing Manager",
    goals = "Identify underperforming campaigns, reallocate budget effectively",
    pain_points = "Too much data, not enough actionable insights",
    technical_level = "intermediate"
  )
)

# Stage 2: Notice - Identify specific problems
notice <- bid_notice(
  previous_stage = interpret,
  problem = "Users overwhelmed by comparing 6 channels across 10 metrics",
  evidence = "User interviews show decision paralysis with current 60-cell comparison grid"
)

# Stage 3: Anticipate - Address cognitive biases
anticipate <- bid_anticipate(
  previous_stage = notice,
  bias_mitigations = list(
    anchoring = "Show performance relative to goals, not just absolute numbers",
    recency_bias = "Include trend analysis, not just current snapshot",
    confirmation_bias = "Highlight both successes and failures"
  )
)

# Stage 4: Structure - Get layout and component suggestions
structure <- bid_structure(previous_stage = anticipate)

# Stage 5: Validate - Ensure actionable outcomes
validate <- bid_validate(
  previous_stage = structure,
  summary_panel = "Executive summary with top 3 actions",
  collaboration = "Team can comment on specific campaigns",
  next_steps = c(
    "Review underperforming channels",
    "Implement suggested budget reallocation",
    "Schedule follow-up in 2 weeks"
  )
)

# Generate implementation report
report <- bid_report(validate, format = "markdown")
```

### You’re Redesigning Based on Feedback

Combine telemetry analysis with systematic redesign:

``` r
# Analyze telemetry
issues <- bid_telemetry("telemetry.sqlite")

# Start BID workflow with telemetry insights
interpret <- bid_interpret(
  central_question = "How can we address the top 3 UX friction points?",
  data_story = new_data_story(
    hook = paste("Analysis of", bid_flags(issues)$session_count, "user sessions"),
    tension = "Users struggling with specific UI elements",
    resolution = "Systematic improvements using BID framework"
  )
)

# Convert telemetry issues to notices
notices <- bid_notices(issues, previous_stage = interpret, max_issues = 3)

# Continue through framework for each issue
# (see full example in getting-started vignette)
```

### You Need to Justify Design Decisions

The BID framework creates a documented trail of reasoning:

``` r
# Complete all 5 stages...
validate <- bid_validate(previous_stage = structure, ...)

# Generate report for stakeholders
html_report <- bid_report(validate, format = "html")

# Shows:
# - User needs and personas
# - Identified problems with evidence
# - Bias mitigation strategies
# - Layout selection rationale
# - Specific component recommendations
# - Validation approach
```

## Practical Tips

### Start Small

Pick **one problematic section** of your dashboard:

``` r
# Focus on just your filter panel
filter_notice <- bid_notice(
  previous_stage = bid_interpret(
    central_question = "How can users filter data more easily?"
  ),
  problem = "Filter panel is cluttered and overwhelming",
  evidence = "8 filters visible, users report confusion"
)

# Get specific suggestions for this one area
filter_structure <- bid_structure(previous_stage = filter_notice)
```

### Use Presets Wisely

Telemetry sensitivity matters:

``` r
# During development - catch everything
strict <- bid_telemetry_presets("strict")
dev_issues <- bid_telemetry("telemetry.sqlite", thresholds = strict)

# Production monitoring - major issues only
relaxed <- bid_telemetry_presets("relaxed")
prod_issues <- bid_telemetry("telemetry.sqlite", thresholds = relaxed)

# Most dashboards - balanced approach
moderate <- bid_telemetry_presets("moderate")
issues <- bid_telemetry("telemetry.sqlite", thresholds = moderate)
```

### Iterate Based on Results

Don’t try to fix everything at once:

``` r
# Week 1: Address top issue
top_issue <- issues[1, ]
notice_1 <- bid_notice_issue(top_issue, interpret_stage)
# ... implement suggestions ...

# Week 2: Re-analyze telemetry
issues_v2 <- bid_telemetry("telemetry.sqlite")
# Did the change help? What's the new top issue?
```

### Combine Concepts

Use multiple BID concepts in one component:

``` r
# Get suggestions from bid_structure
structure <- bid_structure(previous_stage = notice)

# Check which concepts were identified
print(structure$concepts)

# Apply multiple concepts to one section:
ui_combined <- card(
  card_header("Key Metrics"), # Visual Hierarchy - clear header

  # Cognitive Load Theory - limit initial choices
  value_box("Primary Metric", "87%", theme = "success"),

  # Progressive Disclosure - details on demand
  actionButton("show_breakdown", "View Breakdown"),
  conditionalPanel(
    condition = "input.show_breakdown",
    # Principle of Proximity - related items grouped
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Metric A", "45%"),
      value_box("Metric B", "23%"),
      value_box("Metric C", "19%")
    )
  )
)
```

## Next Steps

### Learn More

- **Getting Started Vignette**: Full walkthrough of the 5-stage
  framework
- **Practical Examples Vignette**: Before/after dashboard
  transformations
- **Telemetry Integration Vignette**: Deep dive on telemetry analysis
- **Concepts Reference**: Explore behavioral science principles

### Explore the Concept Dictionary

``` r
# Search for concepts related to your problem
bid_concepts("overload|cognitive")

# Get detailed info on a specific concept
bid_concept("Cognitive Load Theory")

# See all available concepts
all_concepts <- bid_concepts()
```

### Get Component Suggestions

``` r
# After running bid_structure...
structure <- bid_structure(previous_stage = notice)

# Get bslib-specific suggestions
bslib_suggestions <- bid_suggest_components(structure, package = "bslib")

# Get reactable suggestions for data display
table_suggestions <- bid_suggest_components(structure, package = "reactable")

# See all available package suggestions
all_suggestions <- bid_suggest_components(structure, package = "all")
```

### Join the Community

- Report issues:
  [github.com/jrwinget/bidux/issues](https://github.com/jrwinget/bidux/issues)
- Contribute examples: Share your before/after transformations
- Suggest features: Help shape the framework’s evolution

## Summary: Your 15-Minute Action Plan

1.  **Minutes 1-5**: Document your biggest UX problem using
    [`bid_notice()`](https://jrwinget.github.io/bidux/reference/bid_notice.md)
2.  **Minutes 6-8**: Get structure suggestions with
    [`bid_structure()`](https://jrwinget.github.io/bidux/reference/bid_structure.md)
3.  **Minutes 9-12**: Review suggestions and pick 1-2 to implement
4.  **Minutes 13-15**: Apply the quick fix to your dashboard

**Remember:**

- You don’t need to be a UX expert
- Start with one problem, not everything
- Use telemetry when you have it
- The framework documents your reasoning
- Iterate based on results

**The difference between a confusing dashboard and an effective one
isn’t adding more features - it’s applying behavioral science to help
users think.**

Now go make your dashboard better. Your users will thank you.
