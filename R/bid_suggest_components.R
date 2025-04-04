#' Generate UI Component Suggestions Based on BID Application
#'
#' @description
#' This function provides concrete UI component suggestions based on your BID
#' framework application. It's a preview of future capabilities that will be
#' expanded in Phase 3.
#'
#' @param bid_result A tibble from any BID stage function.
#' @param package The UI package to generate suggestions for: "shiny", "bslib",
#'        "reactable", or "echarts4r".
#'
#' @return A tibble with component suggestions.
#'
#' @examples
#' \dontrun{
#'   notice_result <- bid_notice(
#'     problem = "Too many filter options",
#'     evidence = "Users take 30+ seconds to configure dashboard"
#'   )
#' 
#'   bid_suggest_components(notice_result, package = "shiny")
#' }
#'
#' @export
bid_suggest_components <- function(
    bid_result,
    package = c("shiny", "bslib", "reactable", "echarts4r")) {
  package <- match.arg(package)

  # Extract stage information
  stage <- if ("stage" %in% names(bid_result)) {
    bid_result$stage[1]
  } else {
    "Unknown"
  }

  # Define component suggestions by stage and package
  suggestions <- tibble::tibble(
    stage = character(),
    component = character(),
    description = character(),
    code_example = character()
  )

  # Shiny suggestions
  if (package == "shiny") {
    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Notice",
      component = "radioButtons",
      description = "Use radio buttons instead of dropdown for small option sets",
      code_example = 'radioButtons("selection", "Choose option:", choices = c("Option 1", "Option 2"), inline = TRUE)'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Interpret",
      component = "valueBox",
      description = "Highlight key metrics for instant comprehension",
      code_example = 'valueBox(value = "65%", subtitle = "Completion Rate", color = "info")'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Structure",
      component = "tabsetPanel",
      description = "Group related content to reduce cognitive load",
      code_example = 'tabsetPanel(tabPanel("Overview", plotOutput("main_plot")), tabPanel("Details", dataTableOutput("details_table")))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Anticipate",
      component = "conditionalPanel",
      description = "Show context-appropriate insights based on user selection",
      code_example = 'conditionalPanel("input.view == \'comparison\'", plotOutput("comparison_chart"))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Validate",
      component = "actionButton",
      description = "Add export or sharing functionality",
      code_example = 'actionButton("share", "Share Insights", icon = icon("share-alt"))'
    ))
  }

  # bslib suggestions
  if (package == "bslib") {
    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Notice",
      component = "layout_columns",
      description = "Create responsive layouts that adapt to screen size",
      code_example = 'layout_columns(col_widths = c(4, 8), plotOutput("overview"), plotOutput("details"))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Interpret",
      component = "card",
      description = "Visually group related information",
      code_example = 'card(card_header("Key Metrics"), plotOutput("main_chart"), card_footer(actionLink("details", "View details")))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Structure",
      component = "navset_card_tab",
      description = "Create tabbed interfaces within a card",
      code_example = 'navset_card_tab(nav_panel("Overview", plotOutput("main_plot")), nav_panel("Details", dataTableOutput("data_table")))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Anticipate",
      component = "nav_panel",
      description = "Toggle between different framing perspectives",
      code_example = 'navset_pill(nav_panel("Progress View", progressOutput), nav_panel("Gap View", gapOutput))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Validate",
      component = "accordion",
      description = "Progressive disclosure of detailed information",
      code_example = 'accordion(accordion_panel("Key Finding 1", "Details about finding 1"), accordion_panel("Key Finding 2", "Details about finding 2"))'
    ))
  }

  # reactable suggestions
  if (package == "reactable") {
    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Notice",
      component = "defaultColDef",
      description = "Simplify table appearance to reduce cognitive load",
      code_example = 'reactable(data, defaultColDef = colDef(headerStyle = list(background = "#f7f7f7"), align = "center"))'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Interpret",
      component = "conditional style",
      description = "Use color coding to enhance processing fluency",
      code_example = 'colDef(style = function(value) { if(value > 50) { list(color = "green") } else { list(color = "red") } })'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Structure",
      component = "groupBy",
      description = "Group related data to implement Principle of Proximity",
      code_example = 'reactable(data, groupBy = "category")'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Anticipate",
      component = "columns with custom rendering",
      description = "Add benchmark indicators to address Anchoring Effect",
      code_example = 'colDef(cell = function(value) { div(value, div(style = list(width = "80%", height = "4px", background = ifelse(value > 50, "green", "red"))) })'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Validate",
      component = "details",
      description = "Progressive disclosure for detailed information",
      code_example = 'reactable(data, details = function(index) { div(style = list(padding = "1rem"), "Additional details for row ", index) })'
    ))
  }

  # echarts4r suggestions
  if (package == "echarts4r") {
    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Notice",
      component = "e_title",
      description = "Clear titles help establish visual hierarchy",
      code_example = 'e_charts(data) |> e_line(x) |> e_title("Main Metric Trend", "Supporting context")'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Interpret",
      component = "e_tooltip",
      description = "Tooltips provide additional context without cluttering interface",
      code_example = 'e_charts(data) |> e_bar(y) |> e_tooltip(trigger = "item", formatter = "{b}: {c}")'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Structure",
      component = "e_grid",
      description = "Control chart layout precisely",
      code_example = 'e_charts(data) |> e_line(x) |> e_grid(height = "50%", top = "10%")'
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Anticipate",
      component = "e_mark_line",
      description = "Add reference lines to address Anchoring Effect",
      code_example = "e_charts(data) |> e_bar(y) |> e_mark_line(data = list(yAxis = 50))"
    ))

    suggestions <- rbind(suggestions, tibble::tibble(
      stage = "Validate",
      component = "e_toolbox",
      description = "Add export options for sharing insights",
      code_example = 'e_charts(data) |> e_line(x) |> e_toolbox_feature(feature = "saveAsImage")'
    ))
  }

  # Filter suggestions by current stage if available
  if (stage != "Unknown") {
    result <- dplyr::filter(suggestions, stage == !!stage)

    # If no matches for specific stage, return all
    if (nrow(result) == 0) {
      result <- suggestions
    }
  } else {
    result <- suggestions
  }

  # Add message about future capabilities
  message(
    "Component suggestions provided for ",
    package,
    ". In future versions, these will be integrated directly into your workflow."
  )

  message(
    paste(
      "Phase 3 will include ready-to-use code templates and UI components",
      "based on your specific BID application."
    )
  )

  return(result)
}
