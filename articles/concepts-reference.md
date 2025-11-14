# BID Framework Concepts: Practical Implementation

``` r
library(bidux)
library(shiny)
library(bslib)
library(reactable)
library(echarts4r)
```

## BID Framework Concepts: Practical Implementation

The Behavioral Insight Design (BID) framework integrates behavioral
science into UI/UX design for Shiny dashboards. This guide focuses on
practical implementation of key concepts using modern packages like
{bslib}, {reactable}, and {echarts4r}.

### Framework Overview

The BID framework consists of 5 sequential stages, each incorporating
specific psychological tendencies and design principles:

1.  **Interpret**: Understand user needs (Data Storytelling, User
    Personas)
2.  **Notice**: Identify friction points (Cognitive Load, Visual
    Hierarchy)
3.  **Anticipate**: Mitigate biases (Anchoring, Framing)
4.  **Structure**: Organize information (Layout Patterns, Breathable
    Designs)
5.  **Validate**: Empower the user (Peak-End Rule, Collaboration)

### Stage 1: Interpret - Implementing Data Storytelling

Data storytelling helps users understand the meaning behind data through
a narrative structure with hook, context, tension, and resolution.

#### Using {echarts4r} for Data Storytelling

``` r
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#333333"
  ),
  # Clear section headers
  layout_column_wrap(
    width = 12,
    card(
      card_header(
        h2("Sales Dashboard", class = "fs-4")
      ),
      card_body(
        p("Key sales metrics for Q2 2025")
      )
    )
  ),
  # Limited filters grouped by purpose
  layout_column_wrap(
    width = 12,
    card(
      card_header("Filters"),
      card_body(
        layout_column_wrap(
          width = 1 / 3,
          # Time-related filters grouped together
          card(
            card_body(
              h5("Time Period"),
              dateRangeInput(
                "date_range",
                NULL,
                start = Sys.Date() - 90,
                end = Sys.Date()
              ),
              checkboxInput("compare_prev", "Compare to previous period")
            )
          ),
          # Product-related filters grouped together
          card(
            card_body(
              h5("Products"),
              selectInput(
                "category",
                "Category:",
                choices = c("All", "Electronics", "Clothing", "Home")
              )
            )
          ),
          # Region-related filters grouped together
          card(
            card_body(
              h5("Regions"),
              selectInput(
                "region",
                "Region:",
                choices = c("All", "North", "South", "East", "West")
              )
            )
          )
        )
      )
    )
  ),
  # Main content with clear organization
  layout_column_wrap(
    width = 1 / 3,
    # Key metrics with visual simplicity
    value_box(
      title = "Total Revenue",
      value = "$1.24M",
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = value_box_theme(
        bg = "#f8f9fa",
        fg = "#333333"
      )
    ),
    value_box(
      title = "Orders",
      value = "8,490",
      showcase = bsicons::bs_icon("cart"),
      theme = value_box_theme(
        bg = "#f8f9fa",
        fg = "#333333"
      )
    ),
    value_box(
      title = "Avg Order Value",
      value = "$146",
      showcase = bsicons::bs_icon("tag"),
      theme = value_box_theme(
        bg = "#f8f9fa",
        fg = "#333333"
      )
    )
  ),
  # Main charts with progressive disclosure
  accordion(
    accordion_panel(
      "Sales Performance",
      echarts4rOutput("sales_chart", height = "300px")
    ),
    accordion_panel(
      "Category Breakdown",
      echarts4rOutput("category_chart", height = "300px")
    ),
    accordion_panel(
      "Regional Analysis",
      echarts4rOutput("region_chart", height = "300px")
    )
  )
)
```

#### Key Design Principles for Data Storytelling:

1.  Start with a compelling hook to grab attention
2.  Provide relevant context and background
3.  Highlight the challenge or opportunity (tension)
4.  Offer clear insights and actionable recommendations
5.  Use progressive disclosure to reveal the story

### Stage 2: Notice - Reducing Cognitive Load

Cognitive load is the amount of mental effort required to use your
interface. High cognitive load leads to user frustration and errors.

#### Using {bslib} to Reduce Cognitive Load

``` r
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#333333"
  ),
  # Story Hook: Lead with a compelling insight
  layout_column_wrap(
    width = 12,
    card(
      card_header("Q2 2025 Sales Analysis"),
      card_body(
        layout_column_wrap(
          width = 1,
          # Hook: Draw attention to an interesting pattern
          h3(
            "Online sales exceeded in-store for the first time",
            class = "text-primary fs-4"
          ),
          p(
            paste(
              "Q2 saw a historic shift in our sales channels,",
              "creating both challenges and opportunities."
            )
          )
        )
      )
    )
  ),
  # Story Context: Provide background
  layout_column_wrap(
    width = 1 / 2,
    card(
      card_header("Sales Channel Trend"),
      card_body(
        # Context: Show the historical trend
        echarts4rOutput("channel_trend")
      ),
      card_footer(
        p(
          paste(
            "Historical context: Online sales have been steadily increasing",
            "over the past 8 quarters."
          )
        )
      )
    ),
    # Supporting context visualization
    card(
      card_header("Q2 Channel Breakdown"),
      card_body(
        echarts4rOutput("channel_pie")
      )
    )
  ),
  # Story Tension: Highlight the challenge or opportunity
  layout_column_wrap(
    width = 12,
    card(
      card_header("The Challenge"),
      card_body(
        layout_column_wrap(
          width = 1 / 2,
          div(
            # Tension: Highlight the problem
            h4("Different Customer Behaviors by Channel"),
            p(
              paste(
                "Online customers show distinct shopping patterns compared",
                "to in-store shoppers."
              )
            ),
            p("Key differences:")
          ),
          # Supporting data for tension
          echarts4rOutput("behavior_comparison")
        )
      )
    )
  ),
  # Story Resolution: Provide insights and next steps
  layout_column_wrap(
    width = 12,
    card(
      card_header("Recommended Actions"),
      card_body(
        layout_column_wrap(
          width = 1 / 3,
          card(
            card_body(
              h5("Update Digital Strategy"),
              p("Adapt marketing approach to online-first customer base"),
              p("Timeline: Immediate")
            )
          ),
          card(
            card_body(
              h5("Enhance Online Experience"),
              p("Focus on mobile optimization and checkout flow"),
              p("Timeline: Q3 2025")
            )
          ),
          card(
            card_body(
              h5("Reposition Physical Stores"),
              p("Shift from transaction centers to experience hubs"),
              p("Timeline: Q4 2025")
            )
          )
        )
      ),
      card_footer(
        actionButton(
          "download_report",
          "Download Full Analysis",
          class = "btn-primary"
        )
      )
    )
  )
)

# Server function for data storytelling charts:
server <- function(input, output, session) {
  # Channel trend chart showing the story over time
  output$channel_trend <- renderEcharts4r({
    data.frame(
      quarter = paste0("Q", 1:8, " ", c(rep("2024", 4), rep("2025", 4))),
      online = c(42, 45, 47, 48, 49, 52, 54, 56),
      in_store = c(58, 55, 53, 52, 51, 48, 46, 44)
    ) |>
      e_charts(quarter) |>
      e_line(online, name = "Online") |>
      e_line(in_store, name = "In-store") |>
      e_title("Sales Channel Distribution (%)") |>
      e_x_axis(axisLabel = list(rotate = 45)) |>
      e_tooltip(trigger = "axis") |>
      e_mark_line(
        data = list(xAxis = "Q2 2025"),
        name = "Current Quarter"
      ) |>
      e_color(c("#3498db", "#2ecc71"))
  })

  # Channel gauge chart for current quarter
  output$channel_pie <- renderEcharts4r({
    data.frame(
      channel = c("Online", "In-store"),
      value = c(56, 44)
    ) |>
      e_charts(channel) |>
      e_title("Q2 2025 Sales Distribution") |>
      e_gauge(
        value,
        startAngle = 180,
        endAngle = 0,
        detail = list(formatter = "{value}%"),
        axisLine = list(
          lineStyle = list(
            color = list(
              c(value / 100, "#3498db"),
              c(1, "#e9e9e9")
            ),
            width = 30
          )
        )
      ) |>
      e_tooltip()
  })

  # Behavior comparison for tension
  output$behavior_comparison <- renderEcharts4r({
    data.frame(
      metric = c(
        "Avg Order Value",
        "Items per Order",
        "Return Rate",
        "Repeat Purchase"
      ),
      online = c(120, 2.4, 12, 45),
      in_store = c(150, 1.8, 5, 35)
    ) |>
      e_charts(metric) |>
      e_bar(online, name = "Online") |>
      e_bar(in_store, name = "In-store") |>
      e_tooltip() |>
      e_color(c("#3498db", "#2ecc71"))
  })
}
```

#### Key Elements for Reducing Cognitive Load:

1.  Group related controls and information together
2.  Use consistent layouts and patterns
3.  Implement progressive disclosure to hide complexity
4.  Limit the number of choices presented at once
5.  Use clear visual hierarchy to guide attention

### Stage 3: Anticipate - Mitigating Anchoring Bias

Anchoring bias occurs when users rely too heavily on the first piece of
information they encounter when making decisions.

#### Using {reactable} and {echarts4r} to Mitigate Anchoring

``` r
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#333333"
  ),
  # Page title with ample space
  layout_column_wrap(
    width = 12,
    gap = "2rem", # Add breathing room
    card(
      card_header(
        h1(
          "Customer Analytics Dashboard",
          class = "fs-3 py-2" # Add vertical padding
        )
      )
    )
  ),
  # KPI cards with generous spacing
  layout_column_wrap(
    width = 1 / 3,
    gap = "2rem", # Generous spacing between cards
    card(
      card_body(
        padding = "1.5rem", # Extra padding inside cards
        h3("Customers", class = "fs-4 mb-4"), # Bottom margin for breathing room
        div(class = "display-5 mb-3", "12,486"), # Large text with spacing
        div(class = "text-success", "↑ 8% from last month")
      )
    ),
    card(
      card_body(
        padding = "1.5rem",
        h3("Retention Rate", class = "fs-4 mb-4"),
        div(class = "display-5 mb-3", "86%"),
        div(class = "text-success", "↑ 3% from last month")
      )
    ),
    card(
      card_body(
        padding = "1.5rem",
        h3("Satisfaction", class = "fs-4 mb-4"),
        div(class = "display-5 mb-3", "4.7/5"),
        div(class = "text-success", "↑ 0.2 from last month")
      )
    )
  ),
  # Add vertical spacing
  layout_column_wrap(
    width = 12,
    gap = "2rem",
    card(
      card_header(
        h3("Customer Segments", class = "fs-4")
      ),
      card_body(
        padding = "1.5rem", # Extra padding
        echarts4rOutput("segments_chart", height = "300px")
      )
    )
  ),
  # Tabbed interface with inner spacing
  layout_column_wrap(
    width = 12,
    gap = "2rem",
    nav_panel_tabset(
      nav_panel(
        "Demographics",
        div(
          class = "py-4", # Top and bottom padding
          layout_column_wrap(
            width = 1 / 2,
            gap = "2rem",
            echarts4rOutput("age_chart", height = "300px"),
            echarts4rOutput("location_chart", height = "300px")
          )
        )
      ),
      nav_panel(
        "Behavior",
        div(
          class = "py-4",
          layout_column_wrap(
            width = 1 / 2,
            gap = "2rem",
            echarts4rOutput("frequency_chart", height = "300px"),
            echarts4rOutput("channels_chart", height = "300px")
          )
        )
      )
    )
  ),
  # Data table with breathing room
  layout_column_wrap(
    width = 12,
    gap = "2rem",
    card(
      card_header(
        h3("Customer Details", class = "fs-4")
      ),
      card_body(
        padding = "1.5rem", # Extra padding
        reactableOutput("customers_table")
      )
    )
  ),
  # Add space at bottom of page
  layout_column_wrap(
    width = 12,
    gap = "1rem",
    div(class = "py-3") # Empty div for padding
  )
)
```

#### Key Elements of Breathable Layouts:

1.  **Consistent spacing**: Use consistent margins and padding
    throughout
2.  **Whitespace**: Don’t crowd elements; give them room to “breathe”
3.  **Grouping**: Use whitespace to create logical groups
4.  **Content density**: Limit the amount of information in each section
5.  **Visual rhythm**: Create a consistent visual pattern with spacing

#### Using {bslib} for Breathable Layouts

``` r
ui <- page_fillable(
  theme = bs_theme(
    version = 5
  ),
  # Context explanation
  layout_column_wrap(
    width = 12,
    card(
      card_header("Sales Performance Dashboard"),
      card_body(
        p(
          paste(
            "This dashboard provides multiple reference points",
            "to help avoid anchoring bias."
          )
        )
      )
    )
  ),
  # Multiple reference points in KPIs
  layout_column_wrap(
    width = 1 / 3,
    card(
      card_header("Revenue"),
      card_body(
        layout_column_wrap(
          width = 1,
          h3("$1.24M", class = "text-center fs-2"),
          # Multiple reference points to reduce anchoring
          navset_pill(
            nav_panel(
              "vs Target",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+8%"),
                div(class = "text-muted small", "Target: $1.15M")
              )
            ),
            nav_panel(
              "vs Last Year",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+12%"),
                div(class = "text-muted small", "Last Year: $1.11M")
              )
            ),
            nav_panel(
              "vs Industry",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+5%"),
                div(class = "text-muted small", "Industry Avg: $1.18M")
              )
            )
          )
        )
      )
    ),
    # Similar pattern for other metrics
    card(
      card_header("Conversion Rate"),
      card_body(
        layout_column_wrap(
          width = 1,
          h3("3.2%", class = "text-center fs-2"),
          # Multiple reference points
          navset_pill(
            nav_panel(
              "vs Target",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+0.2%"),
                div(class = "text-muted small", "Target: 3.0%")
              )
            ),
            nav_panel(
              "vs Last Year",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+0.5%"),
                div(class = "text-muted small", "Last Year: 2.7%")
              )
            ),
            nav_panel(
              "vs Industry",
              div(
                class = "text-center mt-2",
                div(class = "text-danger fw-bold", "-0.3%"),
                div(class = "text-muted small", "Industry Avg: 3.5%")
              )
            )
          )
        )
      )
    ),
    # Third metric with multiple anchors
    card(
      card_header("Average Order Value"),
      card_body(
        layout_column_wrap(
          width = 1,
          h3("$148", class = "text-center fs-2"),
          # Multiple reference points
          navset_pill(
            nav_panel(
              "vs Target",
              div(
                class = "text-center mt-2",
                div(class = "text-danger fw-bold", "-$7"),
                div(class = "text-muted small", "Target: $155")
              )
            ),
            nav_panel(
              "vs Last Year",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+$10"),
                div(class = "text-muted small", "Last Year: $138")
              )
            ),
            nav_panel(
              "vs Industry",
              div(
                class = "text-center mt-2",
                div(class = "text-success fw-bold", "+$8"),
                div(class = "text-muted small", "Industry Avg: $140")
              )
            )
          )
        )
      )
    )
  ),
  # Chart with multiple reference lines
  layout_column_wrap(
    width = 12,
    card(
      card_header("Sales Performance with Multiple References"),
      card_body(
        echarts4rOutput("sales_references")
      )
    )
  ),
  # Data table with multiple reference columns
  layout_column_wrap(
    width = 12,
    card(
      card_header("Performance Metrics with Multiple References"),
      card_body(
        reactableOutput("metrics_table")
      )
    )
  )
)

# Server logic for anchoring bias mitigation
server <- function(input, output, session) {
  # Chart with multiple reference points
  output$sales_references <- renderEcharts4r({
    data.frame(
      month = month.name[1:6],
      actual = c(980, 1020, 1150, 1240, 1310, 1280),
      target = c(1000, 1050, 1100, 1150, 1200, 1250),
      prev_year = c(920, 950, 990, 1110, 1160, 1200),
      industry = c(990, 1040, 1090, 1130, 1170, 1210)
    ) |>
      e_charts(month) |>
      e_line(
        actual,
        name = "Actual",
        symbol = "circle",
        symbolSize = 10
      ) |>
      e_line(
        target,
        name = "Target",
        symbol = "none",
        lineStyle = list(type = "dashed")
      ) |>
      e_line(
        prev_year,
        name = "Previous Year",
        symbol = "none",
        lineStyle = list(type = "dotted")
      ) |>
      e_line(
        industry,
        name = "Industry Avg",
        symbol = "none",
        lineStyle = list(type = "dashed")
      ) |>
      e_title("Revenue Performance (Thousands)") |>
      e_tooltip(trigger = "axis") |>
      e_color(c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"))
  })

  # Table with multiple reference columns
  output$metrics_table <- renderReactable({
    data <- data.frame(
      metric = c(
        "Revenue",
        "Conversion Rate",
        "Avg Order Value",
        "Customer Count",
        "Units Sold"
      ),
      actual = c("$1.24M", "3.2%", "$148", "8,378", "12,420"),
      target = c("$1.15M", "3.0%", "$155", "7,500", "11,500"),
      vs_target = c("+8%", "+7%", "-5%", "+12%", "+8%"),
      prev_year = c("$1.11M", "2.7%", "$138", "7,825", "11,830"),
      vs_prev = c("+12%", "+19%", "+7%", "+7%", "+5%"),
      industry = c("$1.18M", "3.5%", "$140", "8,100", "12,000"),
      vs_industry = c("+5%", "-9%", "+6%", "+3%", "+4%")
    )

    # Custom cell renderer for comparisons
    compare_cell <- function(value) {
      if (grepl("^\\+", value)) {
        div(value, style = list(color = "#2ecc71", fontWeight = "bold"))
      } else if (grepl("^\\-", value)) {
        div(value, style = list(color = "#e74c3c", fontWeight = "bold"))
      } else {
        value
      }
    }

    reactable(
      data,
      columns = list(
        metric = colDef(name = "Metric"),
        actual = colDef(name = "Actual"),
        target = colDef(name = "Target"),
        vs_target = colDef(name = "vs Target", cell = compare_cell),
        prev_year = colDef(name = "Previous Year"),
        vs_prev = colDef(name = "vs Prev Year", cell = compare_cell),
        industry = colDef(name = "Industry Avg"),
        vs_industry = colDef(name = "vs Industry", cell = compare_cell)
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE
    )
  })
}
```

#### Key Techniques for Mitigating Anchoring Bias:

1.  **Multiple reference points**: Show different benchmarks (target,
    previous, industry)
2.  **Alternative perspectives**: Enable switching between different
    views
3.  **Contextual information**: Provide additional context for values
4.  **Visual references**: Use reference lines in charts
5.  **Relative comparisons**: Show percentage changes rather than just
    absolute values

### Stage 5: Validate - Implementing the Peak-End Rule

The Peak-End Rule states that people judge experiences largely based on
how they felt at the most intense point (peak) and at the end, rather
than based on the sum or average of the experience.

#### Using {bslib} to Implement the Peak-End Rule

``` r
ui <- page_fillable(
  theme = bs_theme(
    version = 5
  ),

  # Dashboard content would go here...

  # End with a strong summary and clear next steps
  layout_column_wrap(
    width = 12,
    card(
      card_header(
        h2("Key Insights & Recommended Actions", class = "fs-4")
      ),
      card_body(
        layout_column_wrap(
          width = 1 / 2,
          # Summary insights at the end of the dashboard
          card(
            card_header("Key Takeaways"),
            card_body(
              tags$ul(
                tags$li(
                  span("Revenue is trending ", class = "fw-bold"),
                  span("8% above target", class = "text-success fw-bold"),
                  span(
                    paste(
                      " for the quarter, primarily driven by",
                      "the Electronics category."
                    )
                  )
                ),
                tags$li(
                  span("Customer acquisition cost has ", class = "fw-bold"),
                  span("decreased by 12%", class = "text-success fw-bold"),
                  span(" following the new digital marketing strategy.")
                ),
                tags$li(
                  span("The West region continues to ", class = "fw-bold"),
                  span("underperform by 5%", class = "text-danger fw-bold"),
                  span(" compared to other regions.")
                )
              )
            )
          ),
          # Clear next steps to end on a positive action-oriented note
          card(
            card_header("Recommended Next Steps"),
            card_body(
              layout_column_wrap(
                width = 1,
                # Button card for first recommendation
                card(
                  card_body(
                    h5("Increase Electronics Inventory"),
                    p(
                      paste(
                        "Current inventory will only support 2 more",
                        "weeks of sales at current pace."
                      )
                    ),
                    actionButton(
                      "action1",
                      "Order Inventory",
                      class = "btn-primary w-100"
                    )
                  )
                ),
                # Button card for second recommendation
                card(
                  card_body(
                    h5("Expand Digital Marketing"),
                    p(
                      paste(
                        "Current campaigns are performing well.",
                        "Consider increasing budget allocation."
                      )
                    ),
                    actionButton(
                      "action2",
                      "Adjust Budget",
                      class = "btn-primary w-100"
                    )
                  )
                ),
                # Button card for third recommendation
                card(
                  card_body(
                    h5("Review West Region Strategy"),
                    p(
                      paste(
                        "Schedule meeting with regional team to",
                        "address performance gaps."
                      )
                    ),
                    actionButton(
                      "action3",
                      "Schedule Meeting",
                      class = "btn-primary w-100"
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Final call to action
      card_footer(
        layout_column_wrap(
          width = 1,
          p("These insights are based on data through Q1."),
          div(
            class = "text-end",
            actionButton(
              "export_pdf",
              "Export as PDF",
              class = "btn-outline-primary me-2"
            ),
            actionButton(
              "share_dashboard",
              "Share Dashboard",
              class = "btn-primary"
            )
          )
        )
      )
    )
  )
)
```

#### Key Elements of Implementing the Peak-End Rule:

1.  **Strong summary**: End with a clear summary of key findings
2.  **Actionable insights**: Provide specific recommendations
3.  **Visual emphasis**: Make the end section visually distinct and
    appealing
4.  **Call to action**: End with clear next steps
5.  **Positive tone**: End on a positive, forward-looking note

### Conclusion

These practical implementations demonstrate how to apply key BID
framework concepts using modern Shiny packages:

- **{bslib}** provides layout components, cards, and value boxes for
  structured information presentation
- **{reactable}** offers advanced data tables with conditional
  formatting and custom rendering
- **{echarts4r}** creates interactive visualizations with multiple
  reference points and comparative views

To explore more concepts and their implementation, use the
[`bid_concepts()`](https://jrwinget.github.io/bidux/reference/bid_concepts.md)
function to access the full concept dictionary:

``` r
bid_concepts() |>
  select(concept, category, implementation_tips) |>
  head(5)
```

For detailed information on a specific concept:

``` r
bid_concept("Anchoring Effect") |>
  select(concept, description, implementation_tips)
```

The BID framework provides a systematic approach to incorporating
behavioral science into your dashboard design, resulting in more
intuitive, usable, and effective Shiny applications.
