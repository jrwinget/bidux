#' List all BID Framework Concepts
#'
#' @description
#' This function returns a tibble listing all the key concepts in the Behavior Insight Design framework,
#' including their descriptions, categories, references, and example applications.
#'
#' @param search Optional search term to filter concepts
#' @return A tibble with concept details.
#' @examples
#' bid_concepts()
#' bid_concepts("cognitive")
#' @export
bid_concepts <- function(search = NULL) {
  concepts <- tibble::tibble(
    concept = c(
      "Cognitive Load Theory", "Hick's Law", "Visual Hierarchies",
      "Data Storytelling Framework", "Processing Fluency", "Emotion & Fluency Effects",
      "Principle of Proximity", "Dual-Processing Theory", "Default Effect",
      "Aesthetic-Usability", "Beautiful-Is-Good Stereotype",
      "Anchoring Effect", "Framing & Loss Aversion", "Confirmation Bias",
      "Belief Perseverance", "Risk Perception",
      "Peak-End Rule", "Cooperation & Coordination",
      "Progressive Disclosure", "Fitts's Law", "Miller's Law",
      "Information Scent", "Affordance", "Gestalt Principles",
      "Preattentive Processing", "Shneiderman's Mantra", "Norman's Stages of Action",
      "Information Hierarchy", "Cognitive Dimensions Framework", "Change Blindness"
    ),
    description = c(
      "Theory that suggests minimizing extraneous load to improve user understanding.",
      "Principle stating that increasing number of choices increases decision time.",
      "Design principle that uses layout and whitespace to guide attention.",
      "Framework for crafting a narrative with data to emphasize insights.",
      "The ease with which information is processed, affecting comprehension and trust.",
      "How emotional resonance influences interpretation of data.",
      "Grouping related elements to create a coherent visual structure.",
      "Model distinguishing between fast (System 1) and slow (System 2) thinking.",
      "How default selections can nudge user behavior.",
      "The effect where aesthetically pleasing designs are perceived as more usable.",
      "The assumption that attractive things are inherently good.",
      "Bias where initial values influence subsequent judgments.",
      "Bias in decision-making influenced by how choices are framed.",
      "Tendency to favor information that confirms pre-existing beliefs.",
      "Clinging to initial beliefs even when faced with contradictory evidence.",
      "Perception of risk that is often influenced by emotional reactions.",
      "Rule stating that people judge experiences by their peak and end moments.",
      "Collaborative processes that enhance group decision-making.",
      "Technique of showing only necessary information to reduce cognitive load.",
      "Law predicting that interaction time is based on distance and target size.",
      "Principle that humans can hold 7±2 items in working memory.",
      "Signals that guide users toward relevant information.",
      "Quality of an object that suggests how it should be used.",
      "Visual perception principles governing how we group similar elements.",
      "Visual properties processed before conscious attention.",
      "Overview first, zoom and filter, then details-on-demand.",
      "Seven-stage model of how users interact with systems.",
      "Organization of information by importance and relationships.",
      "Framework for analyzing notation systems and user interfaces.",
      "Failure to notice visual changes when they occur gradually."
    ),
    category = c(
      "Stage 1", "Stage 1", "Stage 1",
      "Stage 2", "Stage 2", "Stage 2",
      "Stage 3", "Stage 3", "Stage 3",
      "Stage 3", "Stage 3",
      "Stage 4", "Stage 4", "Stage 4",
      "Stage 4", "Stage 4",
      "Stage 5", "Stage 5",
      "Stage 1", "Stage 1", "Stage 1",
      "Stage 2", "Stage 3", "Stage 3",
      "Stage 2", "Stage 2", "Stage 1",
      "Stage 3", "Stage 1", "Stage 4"
    ),
    reference = c(
      "Sweller (1988)", "Hick (1952)", "Tufte (1983)",
      "Matei & Hunter (2021)", "Alter & Oppenheimer (2009)", "Song & Schwarz (2009)",
      "Gestalt Principles", "Tversky & Kahneman (1979)", "Johnson & Goldstein (2003)",
      "Norman (2002)", "Dion et al. (1972)",
      "Tversky & Kahneman (1974)", "Tversky & Kahneman (1981)", "Nickerson (1998)",
      "Ross & Lepper (1980)", "Slovic (1987)",
      "Kahneman et al. (1993)", "Tindale & Kluwe (2015)",
      "Krug (2014)", "Fitts (1954)", "Miller (1956)",
      "Pirolli & Card (1999)", "Norman (1988)", "Wertheimer (1923)",
      "Healey (1996)", "Shneiderman (1996)", "Norman (1986)",
      "Djamasbi et al. (2010)", "Green & Petre (1996)", "Rensink et al. (1997)"
    ),
    example = c(
      "Reducing clutter in dashboards",
      "Simplifying dropdown menus",
      "Emphasizing key metrics through layout",
      "Highlighting a data narrative",
      "Using simple visuals for clarity",
      "Applying subtle color cues for emotional impact",
      "Grouping related controls",
      "Providing quick overviews and detailed views",
      "Pre-selecting best practice options",
      "Creating visually appealing interfaces",
      "Designing attractive summary cards",
      "Using baseline comparisons",
      "Presenting data with gain-framed messaging",
      "Including alternative scenarios",
      "Showing counter-evidence",
      "Visualizing risk probabilities",
      "Ending with a compelling summary",
      "Enabling team annotations",
      "Expanding details only when needed through accordion panels",
      "Making important action buttons larger and more accessible",
      "Limiting navigation options to 5-9 main categories",
      "Providing clear pathways to deeper analysis through visual cues",
      "Using familiar UI patterns that suggest their function",
      "Arranging related filters side by side to indicate relationship",
      "Using color, size, and shape to highlight key metrics before text",
      "Starting with overview charts before detailed data tables",
      "Reducing steps needed to complete common dashboard actions",
      "Making primary metrics visually dominant over secondary ones",
      "Evaluating dashboard complexity across multiple dimensions",
      "Using animation to highlight data changes between states"
    ),
    implementation_tips = c(
      "Use tabs or collapsible sections to organize complex information.",
      "Reduce dropdown options or use hierarchical menus for better organization.",
      "Use size, color, and position to indicate importance of elements.",
      "Start with a clear headline insight before showing supporting details.",
      "Use clean, consistent typography and simple chart types.",
      "Consider subtle color cues that match the message (red/green appropriately).",
      "Place related controls and visualizations in proximity to each other.",
      "Provide both KPI summary cards and detailed data tables.",
      "Pre-select the most useful timeframe or metrics for initial view.",
      "Ensure visual cohesion with consistent colors, spacing, and alignment.",
      "Pay attention to polish in summary cards with clear typography and spacing.",
      "Always show reference points like previous period, budget, or industry average.",
      "Toggle between progress (65% complete) and gap (35% remaining) framing.",
      "Include alternative views that might challenge the main narrative.",
      "Proactively show content that might disprove initial assumptions.",
      "Use appropriate visual encodings for uncertainty and risk.",
      "End dashboards with clear summary insights or next step recommendations.",
      "Include comment/annotation features in dashboards for team input.",
      "Use shiny::actionButton with shinyjs to reveal additional content.",
      "Make clickable elements at least 44x44 pixels for touch interfaces.",
      "Group dashboard sections into logical chunks of 5-9 items.",
      "Use tooltips, highlights, and breadcrumbs to enhance navigation.",
      "Follow standard dashboard patterns that users already understand.",
      "Use layout_column_wrap() to group related metrics visually.",
      "Leverage reactable's conditional formatting for instant pattern recognition.",
      "Implement drill-down functionality from summary to detail views.",
      "Minimize the steps required to perform common dashboard interactions.",
      "Use font size, weight, and color to establish visual hierarchy.",
      "Evaluate your UI across visibility, abstraction, and consistency dimensions.",
      "Use transitions or animations when filtering or changing data views."
    )
  )

  if (!is.null(search)) {
    search_pattern <- paste0("(?i)", search)
    concepts <- concepts |>
      dplyr::filter(
        stringr::str_detect(concept, search_pattern) |
          stringr::str_detect(description, search_pattern) |
          stringr::str_detect(category, search_pattern)
      )
  }

  return(concepts)
}

#' Get Detailed Information for a BID Concept
#'
#' @description
#' This function retrieves detailed information about a specific concept within
#' the BID framework. If the concept is not found, it suggests similar concepts
#' based on fuzzy matching.
#'
#' @param concept_name A character string specifying the name of the concept.
#'
#' @return A tibble with details for the specified concept.
#'
#' @examples
#' bid_concept("Cognitive Load Theory")
#' bid_concept("cognitive load") # Case insensitive, partial matching
#'
#' @export
bid_concept <- function(concept_name) {
  all_concepts <- bid_concepts()

  # Try exact match first (case insensitive)
  result <- dplyr::filter(
    all_concepts,
    stringr::str_to_lower(concept) == stringr::str_to_lower(concept_name)
  )

  # If no exact match, try partial match
  if (nrow(result) == 0) {
    result <- dplyr::filter(
      all_concepts,
      stringr::str_detect(
        stringr::str_to_lower(concept),
        stringr::str_to_lower(concept_name)
      )
    )
  }

  # If still no match, provide suggestions using fuzzy matching
  if (nrow(result) == 0) {
    distances <- stringdist::stringdistmatrix(
      concept_name,
      all_concepts$concept,
      method = "jw"
    )

    closest_matches <- all_concepts$concept[order(distances)[1:3]]

    message <- paste0(
      "Concept '", concept_name, "' not found. Did you mean one of these?\n",
      paste0("  - ", closest_matches, collapse = "\n")
    )

    message(message)

    # Return NULL but with an attribute containing suggestions
    suggestions <- tibble::tibble(suggested_concept = closest_matches)
    return(structure(NULL, suggestions = suggestions))
  }

  # Add usage recommendations based on the concept
  result <- result |>
    dplyr::mutate(
      recommendations = dplyr::case_when(
        category == "Stage 1" ~ paste(
          "Consider how this concept can help identify friction points in",
          "your UI."
        ),
        category == "Stage 2" ~ "
          Apply this concept to improve how users interpret data.
        ",
        category == "Stage 3" ~ "
          Use this concept to better structure dashboard elements.
        ",
        category == "Stage 4" ~ "
          Consider how this concept influences user decision-making.
        ",
        category == "Stage 5" ~ "
          Apply this concept to create a more memorable final impression.
        ",
        TRUE ~ "Consider how this concept can improve your dashboard."
      )
    )

  return(result)
}
