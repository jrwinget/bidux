#' Generate UI Component Suggestions Based on BID Application
#'
#' @description
#' This function provides concrete UI component suggestions based on your BID
#' framework application. It generates code examples and component
#' recommendations for various UI packages.
#'
#' @param bid_result A tibble from any BID stage function.
#' @param package The UI package to generate suggestions for: "shiny", "bslib",
#'        "reactable", "echarts4r", or "all" for suggestions from all packages.
#'
#' @return A tibble with component suggestions.
#'
#' @examples
#' \dontrun{
#'   # Get suggestions from a specific stage
#'   notice_result <- bid_notice(
#'     problem = "Too many filter options",
#'     evidence = "Users take 30+ seconds to configure dashboard"
#'   )
#' 
#'   bid_suggest_components(notice_result, package = "shiny")
#'   
#'   # Get suggestions for all packages
#'   final_result <- bid_validate(...)
#'   all_suggestions <- bid_suggest_components(final_result, package = "all")
#' }
#'
#' @export
bid_suggest_components <- function(
    bid_result,
    package = c("shiny", "bslib", "reactable", "echarts4r", "all")) {
  validate_required_params(bid_result = bid_result)

  package <- match.arg(package)

  stage <- if ("stage" %in% names(bid_result)) {
    bid_result$stage[1]
  } else {
    "Unknown"
  }

  suggestions <- tibble::tibble(
    stage = character(),
    package = character(),
    component = character(),
    description = character(),
    code_example = character(),
    relevance = numeric()
  )

  # {shiny} suggestions
  shiny_suggestions <- tibble::tibble(
    stage = "Notice",
    package = "shiny",
    component = "radioButtons",
    description = "Use radio buttons instead of dropdown for small option sets",
    code_example = 'radioButtons("selection", "Choose option:", choices = c("Option 1", "Option 2"), inline = TRUE)',
    relevance = 1.0
  )

  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "shiny",
    component = "valueBox",
    description = "Highlight key metrics for instant comprehension",
    code_example = 'valueBox(value = "65%", subtitle = "Completion Rate", color = "info")',
    relevance = 1.0
  ))

  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Structure",
    package = "shiny",
    component = "tabsetPanel",
    description = "Group related content to reduce cognitive load",
    code_example = 'tabsetPanel(tabPanel("Overview", plotOutput("main_plot")), tabPanel("Details", dataTableOutput("details_table")))',
    relevance = 1.0
  ))

  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Anticipate",
    package = "shiny",
    component = "conditionalPanel",
    description = "Show context-appropriate insights based on user selection",
    code_example = 'conditionalPanel("input.view == \'comparison\'", plotOutput("comparison_chart"))',
    relevance = 1.0
  ))

  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Validate",
    package = "shiny",
    component = "actionButton",
    description = "Add export or sharing functionality",
    code_example = 'actionButton("share", "Share Insights", icon = icon("share-alt"))',
    relevance = 1.0
  ))
  
  # {shiny} accessibility suggestions
  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Structure",
    package = "shiny",
    component = "tags$div with ARIA attributes",
    description = "Add accessibility attributes to improve screen reader support",
    code_example = 'tags$div(id = "chart-container", role = "img", `aria-label` = "Chart showing sales trends over time", plotOutput("sales_chart"))',
    relevance = 0.9
  ))
  
  shiny_suggestions <- rbind(shiny_suggestions, tibble::tibble(
    stage = "Anticipate",
    package = "shiny",
    component = "shinyFeedback",
    description = "Provide immediate visual feedback on user interactions",
    code_example = 'shinyFeedback::useShinyFeedback()\nshinyFeedback::feedbackWarning("input_id", !is.numeric(input$value), "Please enter a number")',
    relevance = 0.9
  ))

  # {bslib} suggestions
  bslib_suggestions <- tibble::tibble(
    stage = "Notice",
    package = "bslib",
    component = "layout_columns",
    description = "Create responsive layouts that adapt to screen size",
    code_example = 'layout_columns(col_widths = c(4, 8), plotOutput("overview"), plotOutput("details"))',
    relevance = 1.0
  )

  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "bslib",
    component = "card",
    description = "Visually group related information",
    code_example = 'card(card_header("Key Metrics"), plotOutput("main_chart"), card_footer(actionLink("details", "View details")))',
    relevance = 1.0
  ))

  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Structure",
    package = "bslib",
    component = "navset_card_tab",
    description = "Create tabbed interfaces within a card",
    code_example = 'navset_card_tab(nav_panel("Overview", plotOutput("main_plot")), nav_panel("Details", dataTableOutput("data_table")))',
    relevance = 1.0
  ))

  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Anticipate",
    package = "bslib",
    component = "nav_panel",
    description = "Toggle between different framing perspectives",
    code_example = 'navset_pill(nav_panel("Progress View", progressOutput), nav_panel("Gap View", gapOutput))',
    relevance = 1.0
  ))

  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Validate",
    package = "bslib",
    component = "accordion",
    description = "Progressive disclosure of detailed information",
    code_example = 'accordion(accordion_panel("Key Finding 1", "Details about finding 1"), accordion_panel("Key Finding 2", "Details about finding 2"))',
    relevance = 1.0
  ))
  
  # {bslib} accessibility suggestions
  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Structure",
    package = "bslib",
    component = "layout_column_wrap with variable gap",
    description = "Create breathable layouts with appropriate spacing",
    code_example = 'layout_column_wrap(width = 1/2, gap = "2rem", ...))',
    relevance = 0.9
  ))
  
  bslib_suggestions <- rbind(bslib_suggestions, tibble::tibble(
    stage = "Structure",
    package = "bslib",
    component = "value_box",
    description = "Create visually prominent KPI displays with icons",
    code_example = 'value_box(title = "Conversion Rate", value = "24.8%", showcase = bsicons::bs_icon("graph-up-arrow"), theme = "success")',
    relevance = 0.9
  ))

  # {reactable} suggestions
  reactable_suggestions <- tibble::tibble(
    stage = "Notice",
    package = "reactable",
    component = "defaultColDef",
    description = "Simplify table appearance to reduce cognitive load",
    code_example = 'reactable(data, defaultColDef = colDef(headerStyle = list(background = "#f7f7f7"), align = "center"))',
    relevance = 1.0
  )

  reactable_suggestions <- rbind(reactable_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "reactable",
    component = "conditional style",
    description = "Use color coding to enhance processing fluency",
    code_example = 'colDef(style = function(value) { if(value > 50) { list(color = "green") } else { list(color = "red") } })',
    relevance = 1.0
  ))

  reactable_suggestions <- rbind(reactable_suggestions, tibble::tibble(
    stage = "Structure",
    package = "reactable",
    component = "groupBy",
    description = "Group related data to implement Principle of Proximity",
    code_example = 'reactable(data, groupBy = "category")',
    relevance = 1.0
  ))

  reactable_suggestions <- rbind(reactable_suggestions, tibble::tibble(
    stage = "Anticipate",
    package = "reactable",
    component = "columns with custom rendering",
    description = "Add benchmark indicators to address Anchoring Effect",
    code_example = 'colDef(cell = function(value) { div(value, div(style = list(width = "80%", height = "4px", background = ifelse(value > 50, "green", "red"))) })',
    relevance = 1.0
  ))

  reactable_suggestions <- rbind(reactable_suggestions, tibble::tibble(
    stage = "Validate",
    package = "reactable",
    component = "details",
    description = "Progressive disclosure for detailed information",
    code_example = 'reactable(data, details = function(index) { div(style = list(padding = "1rem"), "Additional details for row ", index) })',
    relevance = 1.0
  ))
  
  # {reactable} accessibility suggestions
  reactable_suggestions <- rbind(reactable_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "reactable",
    component = "themed table with accessibility attributes",
    description = "Create visually clean tables with proper accessibility markup",
    code_example = 'reactable(data, theme = reactableTheme(borderColor = "#dfe2e5", stripedColor = "#f6f8fa"), defaultColDef = colDef(headerStyle = list(fontWeight = "bold")), pagination = FALSE, highlight = TRUE, sortable = TRUE, filterable = TRUE, minRows = 1)',
    relevance = 0.9
  ))

  # {echarts4r} suggestions
  echarts_suggestions <- tibble::tibble(
    stage = "Notice",
    package = "echarts4r",
    component = "e_title",
    description = "Clear titles help establish visual hierarchy",
    code_example = 'e_charts(data) |> e_line(x) |> e_title("Main Metric Trend", "Supporting context")',
    relevance = 1.0
  )

  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "echarts4r",
    component = "e_tooltip",
    description = "Tooltips provide additional context without cluttering interface",
    code_example = 'e_charts(data) |> e_bar(y) |> e_tooltip(trigger = "item", formatter = "{b}: {c}")',
    relevance = 1.0
  ))

  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Structure",
    package = "echarts4r",
    component = "e_grid",
    description = "Control chart layout precisely",
    code_example = 'e_charts(data) |> e_line(x) |> e_grid(height = "50%", top = "10%")',
    relevance = 1.0
  ))

  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Anticipate",
    package = "echarts4r",
    component = "e_mark_line",
    description = "Add reference lines to address Anchoring Effect",
    code_example = "e_charts(data) |> e_bar(y) |> e_mark_line(data = list(yAxis = 50))",
    relevance = 1.0
  ))

  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Validate",
    package = "echarts4r",
    component = "e_toolbox",
    description = "Add export options for sharing insights",
    code_example = 'e_charts(data) |> e_line(x) |> e_toolbox_feature(feature = "saveAsImage")',
    relevance = 1.0
  ))
  
  # {echarts4r} accessibility suggestions
  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Interpret",
    package = "echarts4r",
    component = "e_color with designed palette",
    description = "Use a coherent color palette to enhance visual hierarchy and accessibility",
    code_example = 'e_charts(data) |> e_bar(y) |> e_color(c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5"))',
    relevance = 0.9
  ))
  
  echarts_suggestions <- rbind(echarts_suggestions, tibble::tibble(
    stage = "Structure",
    package = "echarts4r",
    component = "e_theme with customization",
    description = "Apply a consistent theme to enhance aesthetic-usability effect",
    code_example = 'e_charts(data) |> e_line(x) |> e_theme("custom") |> e_theme_custom("{\"color\":[\"#5B8FF9\",\"#5AD8A6\",\"#FFD666\"],\"backgroundColor\":\"rgba(0,0,0,0)\",\"textStyle\":{},\"title\":{\"textStyle\":{\"color\":\"#666666\"},\"subtextStyle\":{\"color\":\"#999999\"}},\"line\":{\"itemStyle\":{\"borderWidth\":2},\"lineStyle\":{\"width\":2},\"symbol\":\"emptyCircle\"}}")',
    relevance = 0.8
  ))
  
  # final suggestions
  all_suggestions <- switch(package,
    "shiny" = shiny_suggestions,
    "bslib" = bslib_suggestions,
    "reactable" = reactable_suggestions,
    "echarts4r" = echarts_suggestions,
    "all" = dplyr::bind_rows(
      shiny_suggestions,
      bslib_suggestions,
      reactable_suggestions,
      echarts_suggestions
    )
  )
  
  # filter suggestions by current stage
  if (stage != "Unknown" && package != "all") {
    stage_matches <- dplyr::filter(all_suggestions, stage == !!stage)

    if (nrow(stage_matches) > 0) {
      result <- stage_matches
    } else {
      if (stage == "Validate") {
        result <- all_suggestions
      } else {
        result <- all_suggestions
      }
    }
  } else {
    result <- all_suggestions
  }
  
  if ("relevance" %in% names(result)) {
    result <- dplyr::arrange(result, dplyr::desc(relevance))
  }
  
  if ("relevance" %in% names(result)) {
    result <- dplyr::select(result, -relevance)
  }

  # message about future capabilities
  bid_message(
    "UI Component Suggestions",
    paste0("Generated suggestions for package: ", package),
    if (stage != "Unknown") paste0("Current stage: ", stage),
    "Phase 3 will include ready-to-use code templates and UI components based on your specific BID application."
  )

  return(result)
}
