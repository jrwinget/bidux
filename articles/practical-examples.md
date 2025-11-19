# Practical Examples: Before and After Dashboard Transformations

``` r
library(bidux)
library(shiny)
library(bslib)
library(dplyr)
```

## Introduction: Real Dashboard Problems, Real Solutions

This vignette walks through common dashboard scenarios that data
scientists and Shiny developers encounter, showing how the BID framework
transforms user experience while maintaining analytical rigor.

Each example includes:

- **Before**: The technical-focused approach
- **Problem**: What users actually experience
- **BID Solution**: Systematic application of behavioral science
- **After**: Improved user experience with code examples

## Example 1: The “Everything Dashboard” Problem

### The Scenario

You’re a data scientist at a SaaS company. Your stakeholders asked for
“a dashboard that shows everything about user engagement.” Being
thorough, you built exactly that.

### Before: Technical Excellence, User Confusion

``` r
# The "show everything" approach
ui_before <- navbarPage(
  "User Engagement Analytics",
  tabPanel(
    "Overview",
    fluidRow(
      # 12 KPIs across the top
      column(2, valueBoxOutput("dau")),
      column(2, valueBoxOutput("wau")),
      column(2, valueBoxOutput("mau")),
      column(2, valueBoxOutput("retention")),
      column(2, valueBoxOutput("churn")),
      column(2, valueBoxOutput("ltv"))
    ),
    fluidRow(
      column(2, valueBoxOutput("sessions")),
      column(2, valueBoxOutput("session_duration")),
      column(2, valueBoxOutput("pages_per_session")),
      column(2, valueBoxOutput("bounce_rate")),
      column(2, valueBoxOutput("conversion")),
      column(2, valueBoxOutput("revenue"))
    ),

    # Multiple complex charts
    fluidRow(
      column(6, plotOutput("engagement_trend", height = "400px")),
      column(6, plotOutput("cohort_analysis", height = "400px"))
    ),
    fluidRow(
      column(4, plotOutput("funnel_chart")),
      column(4, plotOutput("retention_curve")),
      column(4, plotOutput("ltv_distribution"))
    )
  ),
  tabPanel("Segments", "More detailed segmentation..."),
  tabPanel("Cohorts", "Cohort analysis details..."),
  tabPanel("Funnels", "Conversion funnel details..."),
  tabPanel("Revenue", "Revenue analytics..."),
  tabPanel("Product", "Product usage analytics...")
)
```

### The Problem: Cognitive Overload

**User feedback:** “I can’t find what I’m looking for” and “It’s
overwhelming”

**What’s happening:** - 12+ metrics compete for attention
simultaneously - No clear hierarchy of importance - Users don’t know
where to look first - Analysis paralysis from too many options

### BID Framework Solution

Let’s apply the systematic BID approach:

``` r
# Stage 1: Interpret - Understand the real user need
interpret_result <- bid_interpret(
  central_question = "How is our user engagement trending, and what needs attention?",
  data_story = new_data_story(
    hook = "User engagement metrics are spread across multiple systems",
    context = "Leadership needs quick insights for weekly business reviews",
    tension = "Current dashboards take too long to interpret",
    resolution = "Provide immediate key insights with drill-down capability"
  ),
  user_personas = data.frame(
    name = c("Sarah (Product Manager)", "Mike (Executive)"),
    goals = c(
      "Quickly spot concerning trends and dive deeper when needed",
      "Understand overall health at a glance"
    ),
    pain_points = c(
      "Too many metrics to process in limited meeting time",
      "Gets lost in details when just needs the big picture"
    ),
    technical_level = c("intermediate", "basic"),
    stringsAsFactors = FALSE
  )
)

# Stage 2: Notice - Identify the specific problem
notice_result <- bid_notice(
  previous_stage = interpret_result,
  problem = "Users experience information overload with 12+ simultaneous metrics",
  evidence = "User interviews show 80% struggle to prioritize information, average time-to-insight is 5+ minutes"
)

# Stage 3: Anticipate - Consider cognitive biases
anticipate_result <- bid_anticipate(
  previous_stage = notice_result,
  bias_mitigations = list(
    attention_bias = "Use size and color to direct focus to most important metrics first",
    choice_overload = "Implement progressive disclosure - show key metrics, hide advanced analytics until requested",
    anchoring = "Lead with the most important business metric to set proper context"
  )
)

# Stage 4: Structure - Organize for cognitive efficiency
structure_result <- bid_structure(previous_stage = anticipate_result)

# Stage 5: Validate - Ensure actionable insights
validate_result <- bid_validate(
  previous_stage = structure_result,
  summary_panel = "Executive summary highlighting key trends and required actions",
  collaboration = "Enable commenting and sharing of specific insights",
  next_steps = c(
    "Focus on the primary engagement metric trend",
    "Investigate any red-flag indicators",
    "Use drill-down for detailed analysis only when needed"
  )
)
```

### After: Cognitively Optimized Dashboard

``` r
# The BID-informed approach: Progressive disclosure with clear hierarchy
ui_after <- page_fillable(
  theme = bs_theme(version = 5),

  # Primary insight first (addresses anchoring bias)
  layout_columns(
    col_widths = c(8, 4),

    # Key insight panel
    card(
      card_header(
        "📈 Engagement Health Score",
        class = "bg-primary text-white"
      ),
      layout_columns(
        value_box(
          title = "Overall Score",
          value = "87/100",
          showcase = bs_icon("speedometer2", size = "2em"),
          theme = "success",
          p(
            "↑ 5 points vs. last month",
            style = "font-size: 0.9em; color: #666;"
          )
        ),
        div(
          h5("Key Drivers", style = "margin-bottom: 10px;"),
          tags$ul(
            tags$li("DAU trending up (+12%)"),
            tags$li("Retention stable (73%)"),
            tags$li("⚠️ Session duration declining (-8%)")
          )
        )
      )
    ),

    # Action panel
    card(
      card_header("🎯 Focus Areas"),
      div(
        tags$div(
          class = "alert alert-warning",
          tags$strong("Attention needed:"),
          br(),
          "Session duration declining. Investigate user experience."
        ),
        actionButton(
          "investigate_sessions",
          "Investigate Session Trends",
          class = "btn btn-warning btn-sm"
        )
      )
    )
  ),

  # Secondary metrics (progressive disclosure)
  card(
    card_header(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span("📊 Detailed Metrics"),
        actionButton(
          "toggle_details",
          "Show Details",
          class = "btn btn-outline-secondary btn-sm"
        )
      )
    ),

    # Hidden by default, shown on demand
    conditionalPanel(
      condition = "input.toggle_details % 2 == 1",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("DAU", "45.2K", icon = "people"),
        value_box("Retention", "73%", icon = "arrow-clockwise"),
        value_box("Sessions", "2.1M", icon = "activity"),
        value_box("Revenue", "$127K", icon = "currency-dollar")
      ),

      # Charts appear only when details are requested
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Engagement Trend"),
          plotOutput("engagement_trend_focused", height = "300px")
        ),
        card(
          card_header("Key Drivers Analysis"),
          plotOutput("drivers_analysis", height = "300px")
        )
      )
    )
  )
)
```

**Key improvements:**

- **Reduced cognitive load**: 1 primary score vs. 12 competing metrics
- **Clear hierarchy**: Most important information first
- **Progressive disclosure**: Details available but not overwhelming
- **Actionable insights**: Clear focus areas and next steps

## Example 2: The “Data Dump” Report Problem

### The Scenario

You’ve built a comprehensive sales dashboard that shows everything the
sales team could possibly need. It’s technically perfect but nobody uses
it.

### Before: All Data, No Insights

``` r
ui_sales_before <- fluidPage(
  titlePanel("Q4 Sales Performance Dashboard"),

  # Massive filter section
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range"),
      selectInput("region", "Region", choices = regions, multiple = TRUE),
      selectInput(
        "product",
        "Product Line",
        choices = products,
        multiple = TRUE
      ),
      selectInput("salesperson", "Sales Rep", choices = reps, multiple = TRUE),
      selectInput(
        "customer_segment",
        "Customer Segment",
        choices = segments,
        multiple = TRUE
      ),
      selectInput(
        "deal_size",
        "Deal Size",
        choices = deal_sizes,
        multiple = TRUE
      ),
      checkboxGroupInput("deal_stage", "Deal Stages", choices = stages),
      numericInput("min_value", "Minimum Deal Value", value = 0),
      numericInput("max_value", "Maximum Deal Value", value = 1000000),
      radioButtons("currency", "Currency", choices = c("USD", "EUR", "GBP")),
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary",
          fluidRow(
            column(3, valueBoxOutput("total_revenue")),
            column(3, valueBoxOutput("deal_count")),
            column(3, valueBoxOutput("avg_deal_size")),
            column(3, valueBoxOutput("win_rate"))
          ),
          plotOutput("revenue_chart", height = "600px")
        ),
        tabPanel("By Region", dataTableOutput("region_table")),
        tabPanel("By Product", dataTableOutput("product_table")),
        tabPanel("By Rep", dataTableOutput("rep_table")),
        tabPanel("Pipeline", dataTableOutput("pipeline_table")),
        tabPanel("Forecasting", plotOutput("forecast_chart"))
      )
    )
  )
)
```

### The Problem: Analysis Paralysis

**User feedback:** “Takes forever to find what I need” and “I just
export to Excel instead”

### BID Solution: User-Centric Design

``` r
# Apply BID framework focusing on sales manager workflow
sales_interpret <- bid_interpret(
  central_question = "What deals need my attention this week?",
  data_story = new_data_story(
    hook = "Sales managers spend 2+ hours weekly creating status reports",
    context = "They need to quickly identify at-risk deals and top opportunities",
    tension = "Current data requires extensive filtering and analysis",
    resolution = "Provide intelligent prioritization with drill-down capability"
  ),
  user_personas = data.frame(
    name = "Jennifer (Regional Sales Manager)",
    goals = "Identify at-risk deals, spot top opportunities, prepare for team meetings",
    pain_points = "Too much filtering required to find actionable insights",
    technical_level = "intermediate",
    stringsAsFactors = FALSE
  )
)

sales_notice <- bid_notice(
  previous_stage = sales_interpret,
  problem = "Sales managers overwhelmed by filter complexity and data volume",
  evidence = "Users spend average 15 minutes per session just setting up filters, 40% abandon before getting insights"
)

sales_anticipate <- bid_anticipate(
  previous_stage = sales_notice,
  bias_mitigations = list(
    recency_bias = "Show deals by urgency, not just recent activity",
    confirmation_bias = "Highlight both positive and concerning trends",
    choice_overload = "Limit initial choices to most common use cases"
  )
)

sales_structure <- bid_structure(previous_stage = sales_anticipate)
```

### After: Insight-Driven Sales Dashboard

``` r
ui_sales_after <- page_navbar(
  title = "Sales Command Center",
  theme = bs_theme(version = 5, preset = "bootstrap"),
  nav_panel(
    "🚨 Needs Attention",
    layout_columns(
      # Immediate action items
      card(
        card_header(
          "🔥 Urgent - Deals at Risk",
          class = "bg-danger text-white"
        ),
        card_body(
          p("3 deals worth $340K need immediate attention"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("MegaCorp Deal - $180K"),
              p(
                "❌ No activity in 14 days",
                style = "color: #dc3545; margin: 0;"
              ),
              p("Owner: Mike Chen", style = "font-size: 0.9em; color: #666;")
            ),
            div(
              actionButton(
                "view_megacorp",
                "View Details",
                class = "btn btn-sm btn-outline-danger"
              ),
              actionButton(
                "contact_mike",
                "Contact Mike",
                class = "btn btn-sm btn-danger"
              )
            )
          )
        )
      ),

      # Opportunities
      card(
        card_header("⭐ Hot Opportunities", class = "bg-success text-white"),
        card_body(
          p("2 deals worth $280K ready to close"),
          actionButton(
            "view_opportunities",
            "Review Opportunities",
            class = "btn btn-success btn-sm"
          )
        )
      )
    ),

    # Smart filters (only show when needed)
    conditionalPanel(
      condition = "input.show_filters",
      card(
        card_header("🔍 Refine Focus"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("quick_region", "Region", choices = c("All", regions)),
          selectInput(
            "quick_timeframe",
            "Timeframe",
            choices = c("This Week", "This Month", "This Quarter")
          ),
          selectInput(
            "quick_value",
            "Deal Size",
            choices = c("All", ">$50K", ">$100K", ">$250K")
          ),
          actionButton(
            "show_all_filters",
            "More Filters...",
            class = "btn btn-outline-secondary btn-sm"
          )
        )
      )
    )
  ),
  nav_panel(
    "📊 Performance",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        "This Month",
        "$1.2M",
        "vs. $980K target (+22%)",
        showcase = bs_icon("graph-up"),
        theme = "success"
      ),
      value_box(
        "Pipeline Health",
        "Strong",
        "3.2x coverage ratio",
        showcase = bs_icon("speedometer2"),
        theme = "info"
      ),
      value_box(
        "Team Status",
        "On Track",
        "8 of 10 reps hitting quota",
        showcase = bs_icon("people"),
        theme = "success"
      )
    ),
    card(
      card_header("📈 Key Trends"),
      plotOutput("performance_trends", height = "400px")
    )
  ),
  nav_panel(
    "🎯 Team Focus",
    # Team-specific insights
    p("Individual rep performance and coaching opportunities...")
  )
)
```

**Key improvements:**

- **Intelligent prioritization**: Shows what needs attention first
- **Reduced choices**: Smart defaults instead of overwhelming filters
- **Action-oriented**: Clear next steps, not just data
- **Progressive complexity**: Simple view first, details on demand

## Example 3: The “Technical Metrics” Challenge

### The Scenario

You’re monitoring application performance with detailed technical
metrics. Your dashboard is perfect for engineers but executives get
lost.

### BID Solution: Audience-Aware Design

``` r
# Interpret stage: Understand different user needs
technical_interpret <- bid_interpret(
  central_question = "How is our application performing and what needs attention?",
  data_story = new_data_story(
    hook = "Application performance directly impacts user satisfaction and revenue",
    context = "Different stakeholders need different views of system health",
    tension = "Technical metrics are critical but overwhelming for non-engineers",
    resolution = "Provide role-appropriate views while maintaining data integrity"
  ),
  user_personas = data.frame(
    name = c("DevOps Engineer", "Engineering Manager", "Executive"),
    goals = c(
      "Identify performance bottlenecks and system issues",
      "Understand overall system health and team priorities",
      "Understand business impact of technical issues"
    ),
    pain_points = c(
      "Needs detailed metrics and historical trends",
      "Needs summary view but ability to drill down",
      "Technical details are overwhelming"
    ),
    technical_level = c("expert", "advanced", "beginner"),
    stringsAsFactors = FALSE
  )
)
```

### After: Multi-Audience Technical Dashboard

``` r
# Adaptive interface based on user role
ui_technical_after <- page_sidebar(
  sidebar = sidebar(
    # Role selector affects entire interface
    radioButtons(
      "user_role",
      "View Mode:",
      choices = c(
        "Executive Summary" = "executive",
        "Management View" = "manager",
        "Technical Details" = "engineer"
      ),
      selected = "executive"
    )
  ),

  # Executive view: Business impact focus
  conditionalPanel(
    condition = "input.user_role == 'executive'",
    h2("🟢 System Health: Good"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Business Impact"),
        value_box(
          "Service Availability",
          "99.8%",
          "Within SLA targets",
          theme = "success"
        ),
        value_box(
          "User Experience",
          "Good",
          "Page loads < 2 seconds",
          theme = "success"
        )
      ),
      card(
        card_header("Action Items"),
        div(
          class = "alert alert-info",
          "✅ No critical issues requiring immediate attention"
        ),
        p("Next scheduled maintenance: Friday 2am")
      )
    )
  ),

  # Manager view: Balance of summary and detail
  conditionalPanel(
    condition = "input.user_role == 'manager'",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Uptime", "99.8%", theme = "success"),
      value_box("Response Time", "1.2s", theme = "success"),
      value_box("Error Rate", "0.02%", theme = "success"),
      value_box("Throughput", "15K/min", theme = "info")
    ),
    card(
      card_header("System Trends"),
      plotOutput("system_trends", height = "300px")
    ),
    card(
      card_header("Team Alerts"),
      p("2 minor alerts resolved this week"),
      actionButton("view_alerts", "View Alert History")
    )
  ),

  # Engineer view: Full technical detail
  conditionalPanel(
    condition = "input.user_role == 'engineer'",
    # Comprehensive technical metrics
    tabsetPanel(
      tabPanel("Performance", "Detailed performance metrics..."),
      tabPanel("Infrastructure", "Server and database metrics..."),
      tabPanel("Alerts", "Full alert history and configuration..."),
      tabPanel("Logs", "System logs and debugging info...")
    )
  )
)
```

**Key insight**: Same data, different presentations based on user
cognitive needs and technical expertise.

## Example 4: The Telemetry-Driven Improvement

### The Scenario

You’ve deployed a dashboard that looks great, but you’re getting vague
complaints about “usability issues.” Rather than guessing, you
instrument your Shiny app with telemetry to understand real user
behavior.

### Discovering Problems with Data

``` r
# Analyze telemetry data to identify real friction points
library(bidux)

# Example telemetry data structure (your actual data would come from shinymetrics, etc.)
telemetry_data <- data.frame(
  session_id = c(rep("s1", 10), rep("s2", 8), rep("s3", 12)),
  input_id = c(
    "date_filter", "region_filter", "product_filter", "date_filter",
    "region_filter", "date_filter", "region_filter", "date_filter",
    "advanced_options", "advanced_options",
    "date_filter", "export_btn", "export_btn", "export_btn",
    "date_filter", "date_filter", "date_filter", "date_filter",
    "date_filter", "region_filter", "date_filter", "date_filter",
    "help_btn", "export_btn", "export_btn", "export_btn",
    "date_filter", "date_filter", "date_filter", "date_filter"
  ),
  timestamp = Sys.time() + 1:30,
  error_occurred = c(rep(FALSE, 8), TRUE, TRUE, rep(FALSE, 20)),
  stringsAsFactors = FALSE
)

# Use bidux telemetry analysis
issues <- bid_telemetry(
  telemetry_data,
  session_col = "session_id",
  input_col = "input_id",
  time_col = "timestamp"
)

# Convert telemetry issues to Notice stage
telemetry_notices <- bid_notices(issues)

print(telemetry_notices)
```

### BID Framework with Data-Driven Insights

``` r
# Start with telemetry-discovered problems
interpret_telemetry <- bid_interpret(
  central_question = "Why are users struggling with the date filter interface?",
  data_story = new_data_story(
    hook = "Telemetry reveals 60% of user interactions involve date filter adjustments",
    context = "Users are repeatedly changing date filters, suggesting confusion or poor defaults",
    tension = "The date filter is creating friction rather than helping users",
    resolution = "Redesign date filtering with smarter defaults and clearer feedback"
  ),
  user_personas = data.frame(
    name = "Data Analyst",
    goals = "Quickly analyze trends for specific time periods",
    pain_points = "Spends too much time adjusting date ranges to see relevant data",
    technical_level = "intermediate",
    stringsAsFactors = FALSE
  )
)

# Use telemetry findings in Notice stage
notice_telemetry <- bid_notice(
  previous_stage = interpret_telemetry,
  problem = "Users make excessive date filter adjustments (avg 8 per session)",
  theory = "Choice Architecture",
  evidence = paste(
    "Telemetry shows 60% of interactions are date-related,",
    "suggesting poor default choices and unclear time period options"
  )
)

# Address cognitive biases revealed by behavior
anticipate_telemetry <- bid_anticipate(
  previous_stage = notice_telemetry,
  bias_mitigations = list(
    status_quo_bias = "Users stick with default settings - provide smarter defaults",
    choice_overload = "Too many date options confuse users - offer common presets",
    analysis_paralysis = "Users repeatedly adjust - provide clear visual feedback on data coverage"
  )
)

# Get layout and UI suggestions
structure_telemetry <- bid_structure(previous_stage = anticipate_telemetry)

# Document validation approach
validate_telemetry <- bid_validate(
  previous_stage = structure_telemetry,
  summary_panel = "Redesigned date filtering with smart defaults and preset options",
  collaboration = "Share telemetry insights with team; A/B test new design",
  next_steps = c(
    "Implement intelligent date defaults based on data recency",
    "Add quick-select presets: 'Last 7 days', 'Last 30 days', 'Year to date'",
    "Provide visual feedback showing data coverage for selected range",
    "Monitor telemetry to validate improvements"
  )
)
```

### After: Data-Driven Date Filter Redesign

``` r
# Before: Generic date picker with no guidance
ui_date_before <- dateRangeInput(
  "date_range",
  "Select Date Range:",
  start = "2024-01-01",
  end = Sys.Date()
)

# After: Smart defaults + quick presets based on telemetry insights
ui_date_after <- div(
  # Quick presets (addresses choice overload from telemetry)
  div(
    style = "margin-bottom: 10px;",
    radioButtons(
      "date_preset",
      "Quick Select:",
      choices = c(
        "Last 7 days" = "7d",
        "Last 30 days" = "30d",
        "Year to date" = "ytd",
        "Custom range" = "custom"
      ),
      selected = "30d", # Smart default based on most common usage
      inline = TRUE
    )
  ),

  # Custom date range (only shown when needed)
  conditionalPanel(
    condition = "input.date_preset == 'custom'",
    dateRangeInput(
      "date_range_custom",
      NULL,
      start = Sys.Date() - 30,
      end = Sys.Date()
    )
  ),

  # Data coverage indicator (provides feedback on selection)
  uiOutput("data_coverage_info")
)

# Server logic for data coverage feedback
server_date_feedback <- function(input, output, session) {
  output$data_coverage_info <- renderUI({
    # Calculate based on selected date range
    coverage_pct <- 95 # Example: 95% of data falls in selected range

    div(
      class = "alert alert-info",
      style = "margin-top: 10px; padding: 8px;",
      icon("info-circle"),
      " This date range covers ",
      strong(paste0(coverage_pct, "%")),
      " of your available data"
    )
  })
}
```

**Key improvements based on telemetry:**

- **Smart defaults**: Most users want recent data, so default to “Last
  30 days”
- **Quick presets**: Reduce 60% of date interactions by providing common
  choices
- **Progressive disclosure**: Hide complex date picker unless custom
  range needed
- **Clear feedback**: Show data coverage to prevent empty-result
  confusion
- **Measurable impact**: Can validate improvements with follow-up
  telemetry

**The telemetry-driven workflow:**

1.  **Measure**: Instrument dashboard to capture real user behavior
2.  **Analyze**: Use
    [`bid_telemetry()`](https://jrwinget.github.io/bidux/reference/bid_telemetry.md)
    to identify friction patterns
3.  **Understand**: Apply BID framework to interpret findings
    systematically
4.  **Redesign**: Make evidence-based improvements targeting real
    problems
5.  **Validate**: Monitor telemetry to confirm improvements worked

## Best Practices Summary

### 1. Start with User Intent, Not Data Structure

``` r
# ❌ Data-structure driven
ui_wrong <- tabPanel(
  "Database Tables",
  tabPanel("Users Table", dataTableOutput("users")),
  tabPanel("Orders Table", dataTableOutput("orders")),
  tabPanel("Products Table", dataTableOutput("products"))
)

# ✅ User-intent driven
ui_right <- tabPanel(
  "Customer Insights",
  card_body(
    h4("What customers need your attention?"),
    # Show actionable customer insights
  )
)
```

### 2. Progressive Disclosure Over Information Density

``` r
# ❌ Everything visible at once
ui_dense <- fluidRow(
  column(2, metric1),
  column(2, metric2),
  column(2, metric3),
  column(2, metric4),
  column(2, metric5),
  column(2, metric6)
)

# ✅ Key information first, details on demand
ui_progressive <- div(
  value_box("Key Metric", "Primary value"),
  actionButton("show_details", "View Supporting Metrics"),
  conditionalPanel(
    condition = "input.show_details",
    # Additional metrics here
  )
)
```

### 3. Context Over Raw Numbers

``` r
# ❌ Raw number without meaning
valueBox("Revenue", "$127,432")

# ✅ Number with context and meaning
value_box(
  "Revenue Progress",
  "$127K",
  "22% above $104K target",
  showcase = bs_icon("graph-up"),
  theme = "success"
)
```

## Next Steps

1.  **Pick one existing dashboard** and apply the BID framework
2.  **Start with Stage 1 (Interpret)** - really understand your users’
    goals
3.  **Test with real users** - measure time-to-insight and task
    completion
4.  **Iterate systematically** - use the same experimental rigor you
    apply to data analysis

Remember: The goal isn’t to become a UX expert, it’s to apply your
analytical thinking to user experience and create more effective data
products.

Use
[`bid_concepts()`](https://jrwinget.github.io/bidux/reference/bid_concepts.md)
to explore behavioral science principles, and check out the
`behavioral-science-primer` vignette for deeper understanding of the
psychological principles behind these improvements.
