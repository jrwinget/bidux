#' Search BID Framework Concepts
#'
#' @description
#' Search for behavioral science and UX concepts used in the BID framework.
#' Returns concepts matching the search term along with their descriptions,
#' categories, and implementation guidance.
#'
#' @param search A character string to search for. If NULL or empty, returns all
#'        concepts.
#' @param fuzzy_match Logical indicating whether to use fuzzy string matching
#'        (default: TRUE)
#' @param max_distance Maximum string distance for fuzzy matching (default: 2)
#'
#' @return A tibble containing matching concepts with their details
#' @export
bid_concepts <- function(search = NULL, fuzzy_match = TRUE, max_distance = 2) {
  concepts_data <- get_concepts_data()

  if (is.null(search) || nchar(trimws(search)) == 0) {
    message("Returning all ", nrow(concepts_data), " concepts")
    return(concepts_data)
  }

  # direct search in concept names and descriptions
  search_terms <- tolower(trimws(unlist(strsplit(search, ","))))

  matches <- c()
  for (term in search_terms) {
    term <- trimws(term)
    if (nchar(term) > 0) {
      # exact matches in concept names or descriptions
      exact_matches <- which(
        grepl(term, tolower(concepts_data$concept)) |
          grepl(term, tolower(concepts_data$description))
      )
      matches <- c(matches, exact_matches)

      # fuzzy matching if enabled and no exact matches
      if (fuzzy_match && length(exact_matches) == 0) {
        distances <- stringdist::stringdistmatrix(
          term,
          tolower(concepts_data$concept),
          method = "jw"
        )
        fuzzy_matches <- which(distances <= max_distance / 10)
        matches <- c(matches, fuzzy_matches)
      }
    }
  }

  matches <- unique(matches)

  if (length(matches) == 0) {
    message("No concepts found matching '", search, "'")
    return(concepts_data[0, ])
  }

  result <- concepts_data[matches, ]
  message("Found ", nrow(result), " concept(s) matching '", search, "'")
  return(result)
}

#' Get detailed information about a specific concept
#'
#' @description
#' Returns detailed information about a specific BID framework concept,
#' including implementation recommendations based on the concept's stage.
#'
#' @param concept_name A character string with the exact or partial concept name
#' @param add_recommendations Logical indicating whether to add stage-specific
#'        recommendations
#'
#' @return A tibble with detailed concept information
#' @export
bid_concept <- function(concept_name, add_recommendations = TRUE) {
  if (is.null(concept_name) || nchar(trimws(concept_name)) == 0) {
    message("Please provide a concept name")
    return(get_concepts_data()[0, ])
  }

  concepts_data <- get_concepts_data()
  concept_clean <- trimws(concept_name)

  # try exact match first
  exact_match <- which(tolower(concepts_data$concept) == tolower(concept_clean))

  if (length(exact_match) > 0) {
    result <- concepts_data[exact_match[1], ]
  } else {
    # try partial match
    partial_matches <- which(grepl(
      tolower(concept_clean),
      tolower(concepts_data$concept)
    ))

    if (length(partial_matches) > 0) {
      result <- concepts_data[partial_matches[1], ]
      message("Found partial match: ", result$concept[1])
    } else {
      message("Concept '", concept_name, "' not found")
      return(concepts_data[0, ])
    }
  }

  # add recommendations based on stage
  if (add_recommendations) {
    stage_recs <- switch(
      result$category[1],
      "Stage 1" = "NOTICE: Use this concept when identifying user problems and cognitive barriers",
      "Stage 2" = "INTERPRET: Apply this concept when developing data stories and central questions",
      "Stage 3" = "STRUCTURE: Implement this concept in dashboard layout and information organization",
      "Stage 4" = "ANTICIPATE: Consider this concept when designing interactions and mitigating biases",
      "Stage 5" = "VALIDATE: Use this concept in user empowerment and collaboration features",
      "All Stages" = "UNIVERSAL: This concept applies throughout the entire BID framework",
      "General guidance for concept application"
    )

    result$recommendations <- stage_recs
  }

  return(result)
}

#' Internal function to get concepts data from external files
#' @return A tibble with all BID framework concepts
#' @keywords internal
get_concepts_data <- function() {
  # reuse the unified loading pattern
  required_cols <- c(
    "concept",
    "description",
    "category",
    "reference",
    "example",
    "implementation_tips",
    "related_concepts"
  )

  # load external data and ensure it's returned as tibble
  data <- load_external_data(
    "bid_concepts_data.csv",
    required_cols,
    get_default_concepts_data,
    NULL
  )

  return(tibble::as_tibble(data))
}

#' Get default concepts data (fallback when external file unavailable)
#' @return A tibble with default BID framework concepts
#' @keywords internal
get_default_concepts_data <- function() {
  tibble::tibble(
    concept = c(
      "Cognitive Load Theory",
      "Hick's Law",
      "Visual Hierarchies",
      "Data Storytelling Framework",
      "Processing Fluency",
      "Emotion & Fluency Effects",
      "Principle of Proximity",
      "Dual-Processing Theory",
      "Default Effect",
      "Aesthetic-Usability",
      "Beautiful-Is-Good Stereotype",
      "Anchoring Effect",
      "Framing & Loss Aversion",
      "Confirmation Bias",
      "Belief Perseverance",
      "Risk Perception",
      "Peak-End Rule",
      "Cooperation & Coordination",
      "Progressive Disclosure",
      "Fitts's Law",
      "Miller's Law",
      "Information Scent",
      "Affordance",
      "Gestalt Principles",
      "Pre-attentive Processing",
      "Shneiderman's Mantra",
      "Norman's Stages of Action",
      "Information Hierarchy",
      "Cognitive Dimensions Framework",
      "Change Blindness",
      "Visual Hierarchy",
      "Breathable Layouts",
      "Gherkin Method",
      "Interaction Hints",
      "Visual Feedback",
      "Accessibility Contrast",
      "Keyboard Navigation",
      "Screen Reader Compatibility",
      "User Personas",
      "User-Centric Design",
      "Nudge Theory"
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
      "Principle that humans can hold 7 (plus or minus 2) items in working memory.",
      "Signals that guide users toward relevant information.",
      "Quality of an object that suggests how it should be used.",
      "Visual perception principles governing how we group similar elements.",
      "Visual properties processed before conscious attention.",
      "Overview first, zoom and filter, then details-on-demand.",
      "Seven-stage model of how users interact with systems.",
      "Organization of information by importance and relationships.",
      "Framework for analyzing notation systems and user interfaces.",
      "Failure to notice visual changes when they occur gradually.",
      "Organizing visual elements by importance to guide the user's attention.",
      "Using whitespace effectively to create visual rhythm and reduce cognitive load.",
      "Structured format for creating user stories with Given-When-Then syntax.",
      "Visual cues that suggest possible interactions to users.",
      "Immediate visual response to user actions to confirm their effect.",
      "Ensuring sufficient color contrast for users with visual impairments.",
      "Supporting navigation and interaction without requiring a mouse.",
      "Making content accessible to users with screen readers.",
      "Fictional representations of user types to guide design decisions.",
      "Design approach that prioritizes user needs and goals throughout the process.",
      "Framework for designing choices that guide people toward beneficial decisions."
    ),
    category = c(
      "Stage 1",
      "Stage 1",
      "Stage 1",
      "Stage 2",
      "Stage 2",
      "Stage 2",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 4",
      "Stage 4",
      "Stage 4",
      "Stage 4",
      "Stage 4",
      "Stage 5",
      "Stage 5",
      "Stage 1",
      "Stage 1",
      "Stage 1",
      "Stage 2",
      "Stage 3",
      "Stage 3",
      "Stage 2",
      "Stage 2",
      "Stage 1",
      "Stage 3",
      "Stage 1",
      "Stage 4",
      "Stage 1",
      "Stage 3",
      "Stage 2",
      "Stage 4",
      "Stage 4",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 2",
      "All Stages",
      "Stage 4"
    ),
    reference = c(
      "Sweller (1988)",
      "Hick (1952)",
      "Tufte (1983)",
      "Matei & Hunter (2021)",
      "Alter & Oppenheimer (2009)",
      "Song & Schwarz (2009)",
      "Gestalt Principles",
      "Tversky & Kahneman (1979)",
      "Johnson & Goldstein (2003)",
      "Norman (2002)",
      "Dion et al. (1972)",
      "Tversky & Kahneman (1974)",
      "Tversky & Kahneman (1981)",
      "Nickerson (1998)",
      "Ross & Lepper (1980)",
      "Slovic (1987)",
      "Kahneman et al. (1993)",
      "Tindale & Kluwe (2015)",
      "Krug (2014)",
      "Fitts (1954)",
      "Miller (1956)",
      "Pirolli & Card (1999)",
      "Norman (1988)",
      "Wertheimer (1923)",
      "Healey (1996)",
      "Shneiderman (1996)",
      "Norman (1986)",
      "Djamasbi et al. (2010)",
      "Green & Petre (1996)",
      "Rensink et al. (1997)",
      "Djamasbi et al. (2010)",
      "White (2011)",
      "North (2006)",
      "Norman (1988)",
      "Shneiderman (2016)",
      "WCAG 2.1",
      "WCAG 2.1",
      "WCAG 2.1",
      "Cooper (1999)",
      "Winget & Eckhoff (2025)",
      "Thaler & Sunstein (2008)"
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
      "Using animation to highlight data changes between states",
      "Creating clear visual hierarchy with primary KPIs at top",
      "Using whitespace between sections with consistent margins",
      "Structuring user stories with Given-When-Then syntax",
      "Adding hover effects and cursor changes for interactive elements",
      "Highlighting active states and providing loading feedback",
      "Ensuring text has 4.5:1 contrast ratio against background",
      "Making all interactive elements focusable with Tab key",
      "Adding descriptive alt text to charts and visuals",
      "Creating design personas representing different user types",
      "Conducting user interviews before and during design process",
      "Setting beneficial defaults like automatic savings enrollment"
    ),
    implementation_tips = c(
      "Use tabs or collapsible sections to organize complex information.",
      "Reduce dropdown options or use hierarchical menus for better organization.",
      "Use size, color, and position to indicate importance of elements.",
      "Start with a clear headline insight before showing supporting details.",
      "Use clean, consistent typography and simple chart types.",
      "Consider subtle color cues that match the message appropriately.",
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
      "Use transitions or animations when filtering or changing data views.",
      "Apply different font sizes and weights to create clear information hierarchy.",
      "Use bslib::card_body(padding = 4) to add consistent padding and whitespace.",
      "Structure user stories as 'As a [user], I want to [action] so that [benefit]'.",
      "Add CSS hover states to clickable elements using the :hover pseudo-class.",
      "Use shinyFeedback to provide immediate validation feedback on inputs.",
      "Test color combinations with WebAIM's contrast checker to meet WCAG standards.",
      "Use tabindex attributes to ensure logical keyboard navigation flow.",
      "Add aria-label attributes to interactive elements and charts.",
      "Create 3-5 distinct personas representing your dashboard's primary users.",
      "Conduct user interviews and usability testing throughout the development process.",
      "Design choice architecture that makes good choices easy and bad choices harder."
    ),
    related_concepts = c(
      "Progressive Disclosure, Miller's Law, Visual Hierarchies",
      "Decision Theory, Cognitive Load Theory, Default Effect",
      "Information Hierarchy, Pre-attentive Processing, Visual Hierarchy",
      "Processing Fluency, Emotion & Fluency Effects, Peak-End Rule",
      "Data Storytelling Framework, Pre-attentive Processing, Aesthetic-Usability",
      "Processing Fluency, Data Storytelling Framework, Beautiful-Is-Good Stereotype",
      "Gestalt Principles, Visual Hierarchy, Information Hierarchy",
      "Processing Fluency, Cognitive Load Theory, Progressive Disclosure",
      "Anchoring Effect, Framing & Loss Aversion, Nudge Theory",
      "Beautiful-Is-Good Stereotype, Processing Fluency, Visual Feedback",
      "Aesthetic-Usability, Emotion & Fluency Effects, Visual Hierarchy",
      "Default Effect, Framing & Loss Aversion, Confirmation Bias",
      "Anchoring Effect, Risk Perception, Default Effect",
      "Belief Perseverance, Framing & Loss Aversion, Risk Perception",
      "Confirmation Bias, Anchoring Effect, Information Scent",
      "Framing & Loss Aversion, Confirmation Bias, Emotion & Fluency Effects",
      "Data Storytelling Framework, Emotion & Fluency Effects, Cooperation & Coordination",
      "Peak-End Rule, Visual Feedback, User-Centric Design",
      "Cognitive Load Theory, Information Hierarchy, Visual Hierarchy",
      "Affordance, Norman's Stages of Action, Interaction Hints",
      "Cognitive Load Theory, Information Hierarchy, Progressive Disclosure",
      "Information Hierarchy, Affordance, Visual Hierarchy",
      "Fitts's Law, Visual Feedback, Interaction Hints",
      "Principle of Proximity, Visual Hierarchy, Pre-attentive Processing",
      "Visual Hierarchy, Gestalt Principles, Visual Feedback",
      "Progressive Disclosure, Information Hierarchy, Cognitive Load Theory",
      "Affordance, Fitts's Law, Cognitive Dimensions Framework",
      "Visual Hierarchy, Principle of Proximity, Pre-attentive Processing",
      "Norman's Stages of Action, Cognitive Load Theory, Affordance",
      "Pre-attentive Processing, Visual Feedback, Visual Hierarchy",
      "Visual Hierarchies, Information Hierarchy, Principle of Proximity",
      "Visual Hierarchy, Information Hierarchy, Aesthetic-Usability",
      "User Personas, User-Centric Design, Data Storytelling Framework",
      "Affordance, Visual Feedback, Fitts's Law",
      "Interaction Hints, Affordance, Change Blindness",
      "Keyboard Navigation, Screen Reader Compatibility, User-Centric Design",
      "Accessibility Contrast, Screen Reader Compatibility, Fitts's Law",
      "Accessibility Contrast, Keyboard Navigation, User-Centric Design",
      "User-Centric Design, Gherkin Method, Data Storytelling Framework",
      "User-Centric Design, Gherkin Method, Cooperation & Coordination",
      "Default Effect, Anchoring Effect, Framing & Loss Aversion"
    )
  )
}
