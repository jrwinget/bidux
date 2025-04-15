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
#' # Get suggestions from a specific stage
#' notice_result <- bid_notice(
#'   problem = "Too many filter options",
#'   evidence = "Users take 30+ seconds to configure dashboard"
#' )
#'
#' bid_suggest_components(notice_result, package = "shiny")
#'
#' # Get suggestions for all packages
#' final_result <- bid_validate(...)
#' all_suggestions <- bid_suggest_components(final_result, package = "all")
#' }
#'
#' @export
bid_suggest_components <- function(
    bid_result,
    package = c("shiny", "bslib", "reactable", "echarts4r", "all")) {
  validate_required_params(bid_result = bid_result)
  package <- match.arg(package)

  cli::cli_alert_info("Generating suggestions for {.pkg {package}} package")

  stage <- if ("stage" %in% names(bid_result)) {
    bid_result$stage[1]
  } else {
    "Unknown"
  }

  if (stage != "Unknown") {
    cli::cli_alert_info("Tailoring suggestions for {.strong {stage}} stage")
  }

  layout <- extract_field(bid_result, "layout", "previous_layout")
  concepts <- extract_field(bid_result, "concepts", "previous_concepts")
  accessibility <- extract_field(
    bid_result,
    "accessibility",
    "previous_accessibility"
  )

  component_suggestions <- get_suggestions(
    package,
    stage,
    layout,
    accessibility
  )

  result <- filter_suggestions(component_suggestions, stage, package)

  if ("relevance" %in% names(result) && nrow(result) > 0) {
    result <- dplyr::arrange(result, dplyr::desc(relevance))
    result <- dplyr::select(result, -relevance)
  }

  if (nrow(result) > 0) {
    cli::cli_alert_success("Generated {nrow(result)} component suggestions")
  } else {
    cli::cli_alert_warning("No suggestions available for the given criteria")
  }

  return(result)
}

# helper function to extract fields with fallbacks
extract_field <- function(data, primary_field, fallback_field = NULL) {
  result <- NA_character_

  if (
    primary_field %in% names(data) &&
      !is.null(data[[primary_field]][1]) &&
      !is.na(data[[primary_field]][1])
  ) {
    result <- data[[primary_field]][1]
  } else if (
    !is.null(fallback_field) &&
      fallback_field %in% names(data) &&
      !is.null(data[[fallback_field]][1]) &&
      !is.na(data[[fallback_field]][1])
  ) {
    result <- data[[fallback_field]][1]
  }

  return(result)
}

# helper function to filter suggestions based on stage
filter_suggestions <- function(suggestions, stage, package) {
  if (stage != "Unknown" && package != "all") {
    stage_matches <- dplyr::filter(suggestions, stage == !!stage)

    if (nrow(stage_matches) > 0) {
      cli::cli_alert_success(
        "Found {nrow(stage_matches)} suggestions for {.val {stage}} stage"
      )
      return(stage_matches)
    } else {
      cli::cli_alert_warning(
        "No suggestions specifically for {.val {stage}} stage, using all available suggestions"
      )
      return(suggestions)
    }
  } else {
    return(suggestions)
  }
}

# helper function to create a suggestion
create_suggestion <- function(
    stage,
    package,
    component,
    description,
    code_example,
    relevance = 1.0) {
  tibble::tibble(
    stage = stage,
    package = package,
    component = component,
    description = description,
    code_example = code_example,
    relevance = relevance
  )
}

# helper function to get suggestions based on requested package
get_suggestions <- function(package, stage, layout, accessibility) {
  suggestions <- switch(package,
    "shiny" = get_shiny_suggestions(stage, layout, accessibility),
    "bslib" = get_bslib_suggestions(stage, layout, accessibility),
    "reactable" = get_reactable_suggestions(stage, layout, accessibility),
    "echarts4r" = get_echarts_suggestions(stage, layout, accessibility),
    "all" = dplyr::bind_rows(
      get_shiny_suggestions(stage, layout, accessibility),
      get_bslib_suggestions(stage, layout, accessibility),
      get_reactable_suggestions(stage, layout, accessibility),
      get_echarts_suggestions(stage, layout, accessibility)
    )
  )

  return(suggestions)
}

# helper function to get Shiny suggestions
get_shiny_suggestions <- function(stage, layout, accessibility) {
  suggestions <- tibble::tibble(
    stage = character(),
    package = character(),
    component = character(),
    description = character(),
    code_example = character(),
    relevance = numeric()
  )

  # notice stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Notice",
    "shiny",
    "radioButtons",
    "Use radio buttons instead of dropdown for small option sets",
    'radioButtons("selection", "Choose option:", choices = c("Option 1", "Option 2"), inline = TRUE)',
    1.0
  ))

  # interpret stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "shiny",
    "valueBox",
    "Highlight key metrics for instant comprehension",
    'valueBox(value = "65%", subtitle = "Completion Rate", color = "info")',
    1.0
  ))

  # structure stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "shiny",
    "tabsetPanel",
    "Group related content to reduce cognitive load",
    'tabsetPanel(tabPanel("Overview", plotOutput("main_plot")), tabPanel("Details", dataTableOutput("details_table")))',
    1.0
  ))

  # anticipate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Anticipate",
    "shiny",
    "conditionalPanel",
    "Show context-appropriate insights based on user selection",
    'conditionalPanel("input.view == \'comparison\'", plotOutput("comparison_chart"))',
    1.0
  ))

  # validate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Validate",
    "shiny",
    "actionButton",
    "Add export or sharing functionality",
    'actionButton("share", "Share Insights", icon = icon("share-alt"))',
    1.0
  ))

  # accessibility suggestion
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "shiny",
    "tags$div with ARIA attributes",
    "Add accessibility attributes to improve screen reader support",
    'tags$div(id = "chart-container", role = "img", `aria-label` = "Chart showing sales trends over time", plotOutput("sales_chart"))',
    0.9
  ))

  return(suggestions)
}

# helper function to get bslib suggestions
get_bslib_suggestions <- function(stage, layout, accessibility) {
  suggestions <- tibble::tibble(
    stage = character(),
    package = character(),
    component = character(),
    description = character(),
    code_example = character(),
    relevance = numeric()
  )

  # notice stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Notice",
    "bslib",
    "layout_columns",
    "Create responsive layouts that adapt to screen size",
    'layout_columns(col_widths = c(4, 8), plotOutput("overview"), plotOutput("details"))',
    1.0
  ))

  # interpret stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "bslib",
    "card",
    "Visually group related information",
    'card(card_header("Key Metrics"), plotOutput("main_chart"), card_footer(actionLink("details", "View details")))',
    1.0
  ))

  # structure stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "bslib",
    "navset_card_tab",
    "Create tabbed interfaces within a card",
    'navset_card_tab(nav_panel("Overview", plotOutput("main_plot")), nav_panel("Details", dataTableOutput("data_table")))',
    1.0
  ))

  # anticipate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Anticipate",
    "bslib",
    "nav_panel",
    "Toggle between different framing perspectives",
    'navset_pill(nav_panel("Progress View", progressOutput), nav_panel("Gap View", gapOutput))',
    1.0
  ))

  # validate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Validate",
    "bslib",
    "accordion",
    "Progressive disclosure of detailed information",
    'accordion(accordion_panel("Key Finding 1", "Details about finding 1"), accordion_panel("Key Finding 2", "Details about finding 2"))',
    1.0
  ))

  # accessibility suggestion
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "bslib",
    "layout_column_wrap with variable gap",
    "Create breathable layouts with appropriate spacing",
    'layout_column_wrap(width = 1/2, gap = "2rem", ...)',
    0.9
  ))

  return(suggestions)
}

# hepper function to get reactable suggestions
get_reactable_suggestions <- function(stage, layout, accessibility) {
  suggestions <- tibble::tibble(
    stage = character(),
    package = character(),
    component = character(),
    description = character(),
    code_example = character(),
    relevance = numeric()
  )

  # notice stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Notice",
    "reactable",
    "defaultColDef",
    "Simplify table appearance to reduce cognitive load",
    'reactable(data, defaultColDef = colDef(headerStyle = list(background = "#f7f7f7"), align = "center"))',
    1.0
  ))

  # interpret stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "reactable",
    "conditional style",
    "Use color coding to enhance processing fluency",
    'colDef(style = function(value) { if(value > 50) { list(color = "green") } else { list(color = "red") } })',
    1.0
  ))

  # structure stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "reactable",
    "groupBy",
    "Group related data to implement Principle of Proximity",
    'reactable(data, groupBy = "category")',
    1.0
  ))

  # anticipate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Anticipate",
    "reactable",
    "columns with reference indicators",
    "Add benchmark indicators to address Anchoring Effect",
    'colDef(cell = function(value) { div(value, div(style = list(width = "80%", height = "4px", background = ifelse(value > 50, "green", "red"))) })',
    1.0
  ))

  # validate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Validate",
    "reactable",
    "details",
    "Progressive disclosure for detailed information",
    'reactable(data, details = function(index) { div(style = list(padding = "1rem"), "Additional details for row ", index) })',
    1.0
  ))

  # accessibility suggestion
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "reactable",
    "themed table with accessibility attributes",
    "Create visually clean tables with proper accessibility markup",
    'reactable(data, theme = reactableTheme(borderColor = "#dfe2e5", stripedColor = "#f6f8fa"), defaultColDef = colDef(headerStyle = list(fontWeight = "bold")))',
    0.9
  ))

  return(suggestions)
}

# helper function to get echarts4r suggestions
get_echarts_suggestions <- function(stage, layout, accessibility) {
  suggestions <- tibble::tibble(
    stage = character(),
    package = character(),
    component = character(),
    description = character(),
    code_example = character(),
    relevance = numeric()
  )

  # notice stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Notice",
    "echarts4r",
    "e_title",
    "Clear titles help establish visual hierarchy",
    'e_charts(data) |> e_line(x) |> e_title("Main Metric Trend", "Supporting context")',
    1.0
  ))

  # interpret stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "echarts4r",
    "e_tooltip",
    "Tooltips provide additional context without cluttering interface",
    'e_charts(data) |> e_bar(y) |> e_tooltip(trigger = "item", formatter = "{b}: {c}")',
    1.0
  ))

  # structure stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Structure",
    "echarts4r",
    "e_grid",
    "Control chart layout precisely",
    'e_charts(data) |> e_line(x) |> e_grid(height = "50%", top = "10%")',
    1.0
  ))

  # anticipate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Anticipate",
    "echarts4r",
    "e_mark_line",
    "Add reference lines to address Anchoring Effect",
    "e_charts(data) |> e_bar(y) |> e_mark_line(data = list(yAxis = 50))",
    1.0
  ))

  # validate stage
  suggestions <- rbind(suggestions, create_suggestion(
    "Validate",
    "echarts4r",
    "e_toolbox",
    "Add export options for sharing insights",
    'e_charts(data) |> e_line(x) |> e_toolbox_feature(feature = "saveAsImage")',
    1.0
  ))

  # accessibility suggestion
  suggestions <- rbind(suggestions, create_suggestion(
    "Interpret",
    "echarts4r",
    "e_color with accessible palette",
    "Use a colorblind-friendly palette to enhance visual hierarchy and accessibility",
    'e_charts(data) |> e_bar(y) |> e_color(c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5"))',
    0.9
  ))

  return(suggestions)
}
