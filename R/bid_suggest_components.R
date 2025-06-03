#' Suggest UI Components Based on BID Framework Analysis
#'
#' @description
#' This function analyzes the results from BID framework stages and suggests
#' appropriate UI components from popular R packages like shiny, bslib, DT, etc.
#' The suggestions are based on the design principles and user needs identified
#' in the BID process.
#'
#' @param bid_stage A tibble output from any BID framework stage function
#' @param package Optional character string specifying which package to focus
#'   suggestions on. Options include "shiny", "bslib", "DT", "plotly",
#'   "reactable", "htmlwidgets". If NULL, suggestions from all packages
#'   are provided.
#'
#' @return A tibble containing component suggestions with relevance scores
#'
#' @examples
#' \dontrun{
#' # After completing BID stages
#' notice_result <- bid_notice(
#'   problem = "Users struggle with complex data",
#'   theory = "Cognitive Load Theory"
#' )
#'
#' # Get all component suggestions
#' bid_suggest_components(notice_result)
#'
#' # Get only bslib suggestions
#' bid_suggest_components(notice_result, package = "bslib")
#'
#' # Get shiny-specific suggestions
#' bid_suggest_components(notice_result, package = "shiny")
#' }
#'
#' @export
bid_suggest_components <- function(bid_stage, package = NULL) {
  # Validation
  if (is.null(bid_stage)) {
    cli::cli_abort("bid_stage cannot be NULL")
  }

  if (!tibble::is_tibble(bid_stage)) {
    cli::cli_abort(c(
      "bid_stage must be a tibble",
      "i" = "Received: {.cls {class(bid_stage)[1]}}"
    ))
  }

  if (!("stage" %in% names(bid_stage))) {
    cli::cli_abort(c(
      "bid_stage must contain a 'stage' column",
      "i" = "Available columns: {.field {names(bid_stage)}}"
    ))
  }

  # Validate package parameter
  valid_packages <- c(
    "shiny",
    "bslib",
    "DT",
    "plotly",
    "reactable",
    "htmlwidgets"
  )
  if (!is.null(package) && !(package %in% valid_packages)) {
    cli::cli_abort(c(
      "Invalid package specified",
      "i" = "Valid options: {.val {valid_packages}}",
      "x" = "You provided: {.val {package}}"
    ))
  }

  # Get component database
  components_db <- get_components_database()

  # Filter by package if specified
  if (!is.null(package)) {
    components_db <- components_db[components_db$package == package, ]
    if (nrow(components_db) == 0) {
      cli::cli_warn("No components found for package: {package}")
      return(create_empty_components_result())
    }
  }

  # Calculate relevance scores based on BID stage analysis
  components_with_scores <- calculate_relevance_scores(bid_stage, components_db)

  # Sort by relevance (descending) and return top suggestions
  components_with_scores <- components_with_scores[
    order(components_with_scores$relevance, decreasing = TRUE),
  ]

  # Reset row names for clean output
  rownames(components_with_scores) <- NULL

  # Provide user feedback
  stage_name <- bid_stage$stage[1]
  total_suggestions <- nrow(components_with_scores)

  if (!is.null(package)) {
    cli::cli_alert_success(
      "Found {total_suggestions} {package} component suggestion{?s} for BID {stage_name} stage"
    )
  } else {
    cli::cli_alert_success(
      "Found {total_suggestions} component suggestion{?s} for BID {stage_name} stage"
    )
  }

  if (total_suggestions > 10) {
    cli::cli_alert_info(
      "Showing components ordered by relevance to your BID analysis"
    )
  }

  return(components_with_scores)
}

get_components_database <- function() {
  tibble::tibble(
    package = c(
      # bslib components
      rep("bslib", 12),
      # shiny components
      rep("shiny", 15),
      # DT components
      rep("DT", 4),
      # plotly components
      rep("plotly", 6),
      # reactable components
      rep("reactable", 5),
      # htmlwidgets components
      rep("htmlwidgets", 3)
    ),
    component = c(
      # bslib components
      "value_box",
      "card",
      "layout_column_wrap",
      "page_sidebar",
      "nav_panel",
      "accordion",
      "tooltip",
      "popover",
      "input_dark_mode",
      "layout_columns",
      "card_header",
      "card_body",

      # shiny components
      "tabsetPanel",
      "conditionalPanel",
      "wellPanel",
      "fluidRow",
      "column",
      "actionButton",
      "downloadButton",
      "modalDialog",
      "showModal",
      "updateSelectInput",
      "renderUI",
      "uiOutput",
      "observeEvent",
      "reactive",
      "isolate",

      # DT components
      "datatable",
      "renderDT",
      "DTOutput",
      "formatStyle",

      # plotly components
      "plot_ly",
      "ggplotly",
      "renderPlotly",
      "plotlyOutput",
      "event_data",
      "plotlyProxy",

      # reactable components
      "reactable",
      "colDef",
      "reactableOutput",
      "renderReactable",
      "getReactableState",

      # htmlwidgets components
      "createWidget",
      "saveWidget",
      "onRender"
    ),
    description = c(
      # bslib descriptions
      "Display key metrics prominently with customizable styling",
      "Organize content in visually distinct containers with headers/footers",
      "Create responsive grid layouts that adapt to screen size",
      "Build dashboard layout with collapsible sidebar navigation",
      "Create tabbed interfaces for organizing related content",
      "Implement collapsible content sections to reduce cognitive load",
      "Provide contextual help without cluttering the interface",
      "Show detailed information on demand without navigation",
      "Allow users to toggle between light and dark themes",
      "Create flexible column-based layouts with fine control",
      "Add headers to cards for clear content organization",
      "Control card content padding and spacing",

      # shiny descriptions
      "Organize related content in tabs to reduce visual complexity",
      "Show/hide UI elements based on user selections",
      "Group related inputs in a visually distinct panel",
      "Create responsive layouts that work across devices",
      "Control precise positioning of elements in grid system",
      "Trigger actions with clear, accessible button interactions",
      "Enable users to export data and results for further analysis",
      "Present focused information overlays without losing context",
      "Display modal dialogs for important communications",
      "Dynamically update input choices based on user selections",
      "Generate UI elements dynamically based on data or user input",
      "Display dynamically generated UI in the interface",
      "React to user actions and input changes efficiently",
      "Create reactive expressions for efficient data processing",
      "Control reactive dependencies to optimize performance",

      # DT descriptions
      "Display data tables with sorting, filtering, and pagination",
      "Render interactive data tables in Shiny applications",
      "Create output containers for DT tables in UI",
      "Apply conditional formatting to highlight important data patterns",

      # plotly descriptions
      "Create interactive visualizations with zoom, pan, and hover",
      "Convert ggplot objects to interactive plotly visualizations",
      "Render interactive plots in Shiny applications",
      "Display interactive plots in Shiny UI",
      "Capture user interactions with plots for further analysis",
      "Modify existing plots without full re-rendering",

      # reactable descriptions
      "Build feature-rich data tables with sorting, filtering, grouping",
      "Define column properties including formatting and styling",
      "Create output containers for reactable tables",
      "Render reactable tables in Shiny applications",
      "Access table state for integration with other components",

      # htmlwidgets descriptions
      "Build custom interactive widgets using JavaScript libraries",
      "Save interactive widgets as standalone HTML files",
      "Add custom JavaScript behavior to widgets"
    ),
    bid_stage_relevance = c(
      # bslib relevance
      "Stage 2,Stage 3,Stage 5",
      "Stage 3,Stage 5",
      "Stage 3",
      "Stage 3",
      "Stage 1,Stage 3",
      "Stage 1,Stage 3",
      "Stage 2,Stage 4",
      "Stage 2,Stage 4",
      "Stage 3,Stage 5",
      "Stage 3",
      "Stage 3",
      "Stage 3",

      # shiny relevance
      "Stage 1,Stage 3",
      "Stage 1,Stage 4",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 4,Stage 5",
      "Stage 5",
      "Stage 2,Stage 4",
      "Stage 2,Stage 4",
      "Stage 4",
      "Stage 1,Stage 4",
      "Stage 1,Stage 4",
      "Stage 4,Stage 5",
      "Stage 1,Stage 4",
      "Stage 1,Stage 4",

      # DT relevance
      "Stage 2,Stage 3,Stage 5",
      "Stage 2,Stage 3,Stage 5",
      "Stage 2,Stage 3,Stage 5",
      "Stage 2,Stage 4",

      # plotly relevance
      "Stage 2,Stage 4,Stage 5",
      "Stage 2,Stage 4,Stage 5",
      "Stage 2,Stage 4,Stage 5",
      "Stage 2,Stage 4,Stage 5",
      "Stage 4,Stage 5",
      "Stage 4,Stage 5",

      # reactable relevance
      "Stage 2,Stage 3,Stage 5",
      "Stage 2,Stage 3",
      "Stage 2,Stage 3,Stage 5",
      "Stage 2,Stage 3,Stage 5",
      "Stage 4,Stage 5",

      # htmlwidgets relevance
      "Stage 3,Stage 4,Stage 5",
      "Stage 5",
      "Stage 4,Stage 5"
    ),
    cognitive_concepts = c(
      # bslib concepts
      "Visual Hierarchy,Aesthetic-Usability",
      "Principle of Proximity,Visual Hierarchy",
      "Cognitive Load Theory,Visual Hierarchy",
      "Information Hierarchy,Progressive Disclosure",
      "Cognitive Load Theory,Information Hierarchy",
      "Progressive Disclosure,Cognitive Load Theory",
      "Processing Fluency,Information Scent",
      "Progressive Disclosure,Processing Fluency",
      "Aesthetic-Usability,User-Centric Design",
      "Visual Hierarchy,Principle of Proximity",
      "Information Hierarchy,Visual Hierarchy",
      "Breathable Layouts,Visual Hierarchy",

      # shiny concepts
      "Information Hierarchy,Progressive Disclosure",
      "Cognitive Load Theory,Progressive Disclosure",
      "Principle of Proximity,Visual Hierarchy",
      "Visual Hierarchy,Breathable Layouts",
      "Visual Hierarchy,Information Hierarchy",
      "Fitts's Law,Affordance",
      "Cooperation & Coordination,Peak-End Rule",
      "Cognitive Load Theory,Processing Fluency",
      "Processing Fluency,Cognitive Load Theory",
      "Default Effect,Cognitive Load Theory",
      "Progressive Disclosure,Cognitive Load Theory",
      "Progressive Disclosure,Cognitive Load Theory",
      "Norman's Stages of Action,Fitts's Law",
      "Processing Fluency,Cognitive Load Theory",
      "Processing Fluency,Cognitive Load Theory",

      # DT concepts
      "Information Hierarchy,Visual Hierarchy",
      "Information Hierarchy,Visual Hierarchy",
      "Information Hierarchy,Visual Hierarchy",
      "Pre-attentive Processing,Visual Hierarchy",

      # plotly concepts
      "Data Storytelling Framework,Processing Fluency",
      "Data Storytelling Framework,Processing Fluency",
      "Data Storytelling Framework,Processing Fluency",
      "Data Storytelling Framework,Processing Fluency",
      "Norman's Stages of Action,Cooperation & Coordination",
      "Processing Fluency,Norman's Stages of Action",

      # reactable concepts
      "Information Hierarchy,Pre-attentive Processing",
      "Visual Hierarchy,Pre-attentive Processing",
      "Information Hierarchy,Pre-attentive Processing",
      "Information Hierarchy,Pre-attentive Processing",
      "Norman's Stages of Action,Cooperation & Coordination",

      # htmlwidgets concepts
      "Affordance,Norman's Stages of Action",
      "Peak-End Rule,Cooperation & Coordination",
      "Norman's Stages of Action,Interaction Hints"
    ),
    use_cases = c(
      # bslib use cases
      "KPI displays,metric summaries,status indicators",
      "Content organization,feature grouping,progressive disclosure",
      "responsive dashboards,multi-column layouts,adaptive grids",
      "navigation-heavy apps,filter panels,sidebar controls",
      "content categorization,workflow organization,multi-view interfaces",
      "FAQ sections,help documentation,optional details",
      "contextual help,feature explanations,guidance text",
      "detailed information,specifications,expanded content",
      "accessibility,user preference,visual comfort",
      "precise layouts,custom grids,flexible positioning",
      "section titles,content labeling,organization",
      "content spacing,visual breathing room,padding control",

      # shiny use cases
      "multi-step workflows,content organization,feature separation",
      "dynamic interfaces,personalized views,conditional features",
      "form organization,input grouping,visual structure",
      "dashboard layouts,responsive design,grid systems",
      "precise positioning,custom layouts,element alignment",
      "user actions,form submission,workflow triggers",
      "data export,report generation,file downloads",
      "alerts,confirmations,detailed forms,focused interactions",
      "important messages,confirmations,alerts,focused content",
      "dependent dropdowns,dynamic filtering,cascading selections",
      "conditional content,dynamic interfaces,personalized views",
      "dynamic content display,conditional rendering,flexible interfaces",
      "user interaction handling,real-time updates,responsive behavior",
      "data processing,computed values,efficient updates",
      "performance optimization,controlled updates,dependency management",

      # DT use cases
      "data exploration,reporting,administrative interfaces",
      "interactive data display,filtering,sorting",
      "data table containers,structured layouts",
      "pattern highlighting,status indication,conditional formatting",

      # plotly use cases
      "exploratory data analysis,interactive reporting,data storytelling",
      "enhanced ggplot visualizations,interactive scientific plots",
      "dashboard visualizations,interactive analytics,data exploration",
      "plot containers,visualization layouts,interactive displays",
      "click analysis,selection tracking,user interaction capture",
      "real-time updates,efficient plot modifications,performance optimization",

      # reactable use cases
      "advanced data tables,business intelligence,reporting dashboards",
      "custom table styling,data presentation,formatted displays",
      "table containers,data display layouts,structured interfaces",
      "interactive tables,business applications,data management",
      "table interaction tracking,selection management,state synchronization",

      # htmlwidgets use cases
      "custom visualizations,specialized interfaces,unique interactions",
      "standalone reports,offline viewing,portable visualizations",
      "enhanced interactivity,custom behaviors,specialized functionality"
    )
  )
}

calculate_relevance_scores <- function(bid_stage, components_db) {
  stage_name <- bid_stage$stage[1]

  # Initialize relevance scores
  components_db$relevance <- 0

  # Stage-based scoring (highest weight) - need to handle different stage name formats
  stage_pattern <- paste0(
    "Stage ",
    switch(
      stage_name,
      "Notice" = "1",
      "Interpret" = "2",
      "Structure" = "3",
      "Anticipate" = "4",
      "Deploy" = "5",
      "1" # fallback
    )
  )

  stage_matches <- grepl(
    stage_pattern,
    components_db$bid_stage_relevance,
    fixed = TRUE
  )
  components_db$relevance[stage_matches] <- components_db$relevance[
    stage_matches
  ] +
    50

  # Extract information from bid_stage for concept matching
  concepts_to_match <- extract_relevant_concepts(bid_stage)

  # Concept-based scoring with more flexible matching
  for (concept in concepts_to_match) {
    # Try exact match first
    concept_matches <- grepl(
      concept,
      components_db$cognitive_concepts,
      fixed = TRUE
    )
    if (!any(concept_matches)) {
      # Try partial matching for common concepts
      concept_lower <- tolower(concept)
      if (grepl("visual", concept_lower)) {
        concept_matches <- grepl(
          "Visual",
          components_db$cognitive_concepts,
          fixed = TRUE
        )
      } else if (grepl("cognitive", concept_lower)) {
        concept_matches <- grepl(
          "Cognitive",
          components_db$cognitive_concepts,
          fixed = TRUE
        )
      } else if (grepl("hierarchy", concept_lower)) {
        concept_matches <- grepl(
          "Hierarchy",
          components_db$cognitive_concepts,
          fixed = TRUE
        )
      } else if (grepl("proximity", concept_lower)) {
        concept_matches <- grepl(
          "Proximity",
          components_db$cognitive_concepts,
          fixed = TRUE
        )
      }
    }
    components_db$relevance[concept_matches] <- components_db$relevance[
      concept_matches
    ] +
      25
  }

  # Problem/theory-based scoring for additional context
  additional_context <- extract_additional_context(bid_stage)
  for (context_term in additional_context) {
    context_matches <- grepl(
      context_term,
      components_db$description,
      ignore.case = TRUE
    ) |
      grepl(context_term, components_db$use_cases, ignore.case = TRUE)
    components_db$relevance[context_matches] <- components_db$relevance[
      context_matches
    ] +
      10
  }

  # Layout-specific scoring for Structure stage
  if (stage_name == "Structure" && "layout" %in% names(bid_stage)) {
    layout_value <- bid_stage$layout[1]
    if (!is.na(layout_value)) {
      layout_keywords <- extract_layout_keywords(layout_value)
      for (keyword in layout_keywords) {
        layout_matches <- grepl(
          keyword,
          components_db$description,
          ignore.case = TRUE
        ) |
          grepl(keyword, components_db$use_cases, ignore.case = TRUE)
        components_db$relevance[layout_matches] <- components_db$relevance[
          layout_matches
        ] +
          15
      }
    }
  }

  # Ensure we always return some components, even if relevance is low
  if (all(components_db$relevance == 0)) {
    # Give small scores to some general components for any stage
    general_components <- c(
      "card",
      "value_box",
      "layout",
      "tabset",
      "datatable"
    )
    for (comp in general_components) {
      comp_matches <- grepl(comp, components_db$component, ignore.case = TRUE)
      components_db$relevance[comp_matches] <- components_db$relevance[
        comp_matches
      ] +
        5
    }
  }

  # Remove components with zero relevance for cleaner output
  components_db <- components_db[components_db$relevance > 0, ]

  return(components_db)
}

extract_relevant_concepts <- function(bid_stage) {
  concepts <- character(0)

  # Extract from theory field (Notice stage)
  if ("theory" %in% names(bid_stage) && !is.na(bid_stage$theory[1])) {
    theory <- bid_stage$theory[1]
    concepts <- c(concepts, theory)
  }

  # Extract from concepts field (Structure stage)
  if ("concepts" %in% names(bid_stage) && !is.na(bid_stage$concepts[1])) {
    stage_concepts <- bid_stage$concepts[1]
    # Handle both comma-separated and individual concepts
    if (grepl(",", stage_concepts)) {
      concepts <- c(concepts, trimws(unlist(strsplit(stage_concepts, ","))))
    } else {
      concepts <- c(concepts, stage_concepts)
    }
  }

  # Extract from previous stage information
  previous_fields <- c("previous_theory", "previous_concepts")
  for (field in previous_fields) {
    if (field %in% names(bid_stage) && !is.na(bid_stage[[field]][1])) {
      concepts <- c(concepts, bid_stage[[field]][1])
    }
  }

  return(unique(concepts))
}

extract_additional_context <- function(bid_stage) {
  context_terms <- character(0)

  # Extract problem keywords
  if ("problem" %in% names(bid_stage) && !is.na(bid_stage$problem[1])) {
    problem <- tolower(bid_stage$problem[1])
    # Extract key terms that might relate to UI components
    if (grepl("complex|overwhelm|too many", problem)) {
      context_terms <- c(context_terms, "progressive", "accordion", "tab")
    }
    if (grepl("find|search|locate", problem)) {
      context_terms <- c(context_terms, "filter", "search", "navigation")
    }
    if (grepl("slow|performance|delay", problem)) {
      context_terms <- c(context_terms, "reactive", "efficient", "optimize")
    }
    if (grepl("mobile|phone|tablet", problem)) {
      context_terms <- c(context_terms, "responsive", "layout", "grid")
    }
  }

  # Extract from central question (Interpret stage)
  if (
    "central_question" %in%
      names(bid_stage) &&
      !is.na(bid_stage$central_question[1])
  ) {
    question <- tolower(bid_stage$central_question[1])
    if (grepl("simplify|reduce|minimize", question)) {
      context_terms <- c(context_terms, "card", "accordion", "modal")
    }
    if (grepl("organize|structure|arrange", question)) {
      context_terms <- c(context_terms, "layout", "grid", "column")
    }
    if (grepl("collaborate|share|team", question)) {
      context_terms <- c(context_terms, "download", "export", "modal")
    }
  }

  # Extract from audience information
  audience_fields <- c("audience", "target_audience", "previous_audience")
  for (field in audience_fields) {
    if (field %in% names(bid_stage) && !is.na(bid_stage[[field]][1])) {
      audience <- tolower(bid_stage[[field]][1])
      if (grepl("executive|manager|leadership", audience)) {
        context_terms <- c(context_terms, "value_box", "card", "summary")
      }
      if (grepl("analyst|technical|developer", audience)) {
        context_terms <- c(context_terms, "datatable", "plotly", "interactive")
      }
      if (grepl("mobile|field", audience)) {
        context_terms <- c(context_terms, "responsive", "touch", "mobile")
      }
    }
  }

  return(unique(context_terms))
}

extract_layout_keywords <- function(layout_value) {
  layout_lower <- tolower(layout_value)
  keywords <- character(0)

  if (grepl("dual|two|split", layout_lower)) {
    keywords <- c(keywords, "column", "grid", "layout")
  }
  if (grepl("sidebar|side", layout_lower)) {
    keywords <- c(keywords, "sidebar", "nav", "panel")
  }
  if (grepl("card|dashboard", layout_lower)) {
    keywords <- c(keywords, "card", "value_box", "grid")
  }
  if (grepl("tab|navigation", layout_lower)) {
    keywords <- c(keywords, "tab", "nav", "panel")
  }
  if (grepl("accordion|collaps", layout_lower)) {
    keywords <- c(keywords, "accordion", "conditional", "progressive")
  }

  return(keywords)
}

create_empty_components_result <- function() {
  tibble::tibble(
    package = character(0),
    component = character(0),
    description = character(0),
    bid_stage_relevance = character(0),
    cognitive_concepts = character(0),
    use_cases = character(0),
    relevance = numeric(0)
  )
}
