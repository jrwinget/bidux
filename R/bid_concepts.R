#' List all BID Framework Concepts
#'
#' @description
#' This function returns a tibble listing all the key concepts in the Behavior
#' Insight Design framework, including their descriptions, categories,
#' references, and example applications.
#'
#' @param search Optional search term to filter concepts. Multiple terms can be provided
#'        separated by commas or spaces.
#' @return A tibble with concept details.
#'
#' @examples
#' bid_concepts()
#' bid_concepts("cognitive")
#' bid_concepts("visual, hierarchy") # Multiple search terms
#'
#' @export
bid_concepts <- function(search = NULL) {
  concepts <- tibble::tibble(
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
      "Progressive Disclosure",
      "Visual Hierarchy",
      "Breathable Layouts",
      "Gherkin Method",
      "Interaction Hints",
      "Visual Feedback",
      "Accessibility Contrast",
      "Keyboard Navigation",
      "Screen Reader Compatibility",
      "User Personas",
      "User-Centric Design"
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
      "Failure to notice visual changes when they occur gradually.",
      "Revealing information gradually as needed to prevent overwhelming users.",
      "Organizing visual elements by importance to guide the user's attention.",
      "Using whitespace effectively to create visual rhythm and reduce cognitive load.",
      "Structured format for creating user stories with Given-When-Then syntax.",
      "Visual cues that suggest possible interactions to users.",
      "Immediate visual response to user actions to confirm their effect.",
      "Ensuring sufficient color contrast for users with visual impairments.",
      "Supporting navigation and interaction without requiring a mouse.",
      "Making content accessible to users with screen readers.",
      "Fictional representations of user types to guide design decisions.",
      "Design approach that prioritizes user needs and goals throughout the process."
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
      "Stage 3",
      "Stage 1",
      "Stage 3",
      "Stage 2",
      "Stage 4",
      "Stage 4",
      "Stage 3",
      "Stage 3",
      "Stage 3",
      "Stage 2",
      "All Stages"
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
      "Nielsen (2006)",
      "Djamasbi et al. (2010)",
      "White (2011)",
      "North (2006)",
      "Norman (1988)",
      "Shneiderman (2016)",
      "WCAG 2.1",
      "WCAG 2.1",
      "WCAG 2.1",
      "Cooper (1999)",
      "Winget & Eckhoff (2025)"
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
      "Using collapsible sections to reveal details only when needed",
      "Making key metrics larger and more prominent than supporting data",
      "Adding appropriate spacing between dashboard sections",
      "Documenting user stories in Given-When-Then format",
      "Using hover effects to indicate clickable elements",
      "Highlighting active filters or selections with visual cues",
      "Ensuring text has a 4.5:1 contrast ratio against its background",
      "Making all interactive elements focusable with the Tab key",
      "Adding descriptive alt text to charts and visuals",
      "Creating design personas representing different user types",
      "Conducting user research before and during the design process"
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
      "Use transitions or animations when filtering or changing data views.",
      "Use bslib::accordion() or shiny::conditionalPanel() to implement progressive disclosure.",
      "Apply different font sizes and weights to create clear information hierarchy.",
      "Use bslib::card_body(padding = 4) to add consistent padding and whitespace.",
      "Structure user stories as 'As a [user], I want to [action] so that [benefit]'.",
      "Add CSS hover states to clickable elements using the :hover pseudo-class.",
      "Use shinyFeedback to provide immediate validation feedback on inputs.",
      "Test color combinations with WebAIM's contrast checker to meet WCAG standards.",
      "Use tabindex attributes to ensure logical keyboard navigation flow.",
      "Add aria-label attributes to interactive elements and charts.",
      "Create 3-5 distinct personas representing your dashboard's primary users.",
      "Conduct user interviews and usability testing throughout the development process."
    ),
    # cross-referencing between related concepts
    related_concepts = c(
      "Progressive Disclosure, Miller's Law, Visual Hierarchies", # Cognitive Load Theory
      "Decision Theory, Cognitive Load Theory, Default Effect", # Hick's Law
      "Information Hierarchy, Pre-attentive Processing, Visual Hierarchy", # Visual Hierarchies
      "Processing Fluency, Emotion & Fluency Effects, Peak-End Rule", # Data Storytelling Framework
      "Data Storytelling Framework, Pre-attentive Processing, Aesthetic-Usability", # Processing Fluency
      "Processing Fluency, Data Storytelling Framework, Beautiful-Is-Good Stereotype", # Emotion & Fluency Effects
      "Gestalt Principles, Visual Hierarchy, Information Hierarchy", # Principle of Proximity
      "Processing Fluency, Cognitive Load Theory, Progressive Disclosure", # Dual-Processing Theory
      "Anchoring Effect, Framing & Loss Aversion, Nudge Theory", # Default Effect
      "Beautiful-Is-Good Stereotype, Processing Fluency, Visual Feedback", # Aesthetic-Usability
      "Aesthetic-Usability, Emotion & Fluency Effects, Visual Hierarchy", # Beautiful-Is-Good Stereotype
      "Default Effect, Framing & Loss Aversion, Confirmation Bias", # Anchoring Effect
      "Anchoring Effect, Risk Perception, Default Effect", # Framing & Loss Aversion
      "Belief Perseverance, Framing & Loss Aversion, Risk Perception", # Confirmation Bias
      "Confirmation Bias, Anchoring Effect, Information Scent", # Belief Perseverance
      "Framing & Loss Aversion, Confirmation Bias, Emotion & Fluency Effects", # Risk Perception
      "Data Storytelling Framework, Emotion & Fluency Effects, Cooperation & Coordination", # Peak-End Rule
      "Peak-End Rule, Visual Feedback, User-Centric Design", # Cooperation & Coordination
      "Cognitive Load Theory, Information Hierarchy, Visual Hierarchy", # Progressive Disclosure
      "Affordance, Norman's Stages of Action, Interaction Hints", # Fitts's Law
      "Cognitive Load Theory, Information Hierarchy, Progressive Disclosure", # Miller's Law
      "Information Hierarchy, Affordance, Visual Hierarchy", # Information Scent
      "Fitts's Law, Visual Feedback, Interaction Hints", # Affordance
      "Principle of Proximity, Visual Hierarchy, Pre-attentive Processing", # Gestalt Principles
      "Visual Hierarchy, Gestalt Principles, Visual Feedback", # Pre-attentive Processing
      "Progressive Disclosure, Information Hierarchy, Cognitive Load Theory", # Shneiderman's Mantra
      "Affordance, Fitts's Law, Cognitive Dimensions Framework", # Norman's Stages of Action
      "Visual Hierarchy, Principle of Proximity, Pre-attentive Processing", # Information Hierarchy
      "Norman's Stages of Action, Cognitive Load Theory, Affordance", # Cognitive Dimensions Framework
      "Pre-attentive Processing, Visual Feedback, Visual Hierarchy", # Change Blindness
      "Cognitive Load Theory, Miller's Law, Information Hierarchy", # Progressive Disclosure (duplicate)
      "Visual Hierarchies, Information Hierarchy, Principle of Proximity", # Visual Hierarchy
      "Visual Hierarchy, Information Hierarchy, Aesthetic-Usability", # Breathable Layouts
      "User Personas, User-Centric Design, Data Storytelling Framework", # Gherkin Method
      "Affordance, Visual Feedback, Fitts's Law", # Interaction Hints
      "Interaction Hints, Affordance, Change Blindness", # Visual Feedback
      "Keyboard Navigation, Screen Reader Compatibility, User-Centric Design", # Accessibility Contrast
      "Accessibility Contrast, Screen Reader Compatibility, Fitts's Law", # Keyboard Navigation
      "Accessibility Contrast, Keyboard Navigation, User-Centric Design", # Screen Reader Compatibility
      "User-Centric Design, Gherkin Method, Data Storytelling Framework", # User Personas
      "User Personas, Gherkin Method, Cooperation & Coordination" # User-Centric Design
    )
  )

  concepts$example[concepts$concept == "Visual Hierarchy"] <-
    "Creating a clear hierarchy with primary KPIs at top (large font), secondary metrics below (medium font), and detailed data at bottom; using bslib::value_box() components with varying sizes for different metrics; applying color saturation to distinguish between critical alerts (bright colors) and normal metrics (muted colors)."

  concepts$example[concepts$concept == "Breathable Layouts"] <-
    "Using bslib::layout_column_wrap(width = 1/3, gap = '2rem') to create well-spaced card layouts; implementing whitespace between sections with consistent margins (12-24px); applying content 'breathing room' within cards using card_body(padding = 4); creating focused UIs that limit information density to prevent cognitive overload."

  concepts$example[concepts$concept == "Gherkin Method"] <-
    "Structuring user stories for dashboard features like: 'GIVEN I am a marketing analyst, WHEN I select multiple channels in the filter, THEN I should see a comparison chart of all selected channels'; defining acceptance criteria as 'GIVEN no filters are selected, THEN all data should be shown'; documenting edge cases with 'GIVEN all filters are applied AND no data exists, THEN show a helpful empty state message'."

  concepts$example[concepts$concept == "Interaction Hints"] <-
    "Adding subtle hover effects to cards that can be clicked for details; using cursor changes (cursor: pointer) for interactive elements; adding tooltip hints such as 'Click to expand' or 'Drag to rearrange'; implementing skeleton loading states to indicate content is coming; providing visual affordances like slight elevation or borders for clickable elements."

  concepts$example[concepts$concept == "Visual Feedback"] <-
    "Highlighting the active tab with a clear indicator; showing loading states when data is refreshing; providing success/error notifications after user actions; implementing animated transitions when states change; adding subtle animations for filtering actions; using progress bars for ongoing processes; highlighting selected data points in linked visualizations."

  concepts$example[concepts$concept == "User-Centric Design"] <-
    "Conducting user interviews before designing; creating card sorting exercises to understand mental models; performing usability testing with real users; building user journey maps to identify pain points; implementing feedback mechanisms within the app; establishing KPIs based on user behavior; iterating designs based on usage analytics; personalizing experiences based on user roles and preferences."

  if (!is.null(search)) {
    search_terms <- unlist(strsplit(search, "[,\\s]+"))
    search_terms <- search_terms[search_terms != ""]

    if (length(search_terms) > 0) {
      matches <- matrix(FALSE, nrow = nrow(concepts), ncol = length(search_terms))

      for (i in seq_along(search_terms)) {
        term <- tolower(search_terms[i])
        for (
          col in c(
            "concept",
            "description",
            "category",
            "implementation_tips",
            "example",
            "related_concepts"
          )
        ) {
          if (col %in% names(concepts)) {
            matches[, i] <- matches[, i] | grepl(term, tolower(concepts[[col]]), fixed = TRUE)
          }
        }
      }

      matches_any <- rowSums(matches) > 0
      concepts <- concepts[matches_any, ]

      # relevance score (number of terms matched)
      if (nrow(concepts) > 0) {
        relevance <- rowSums(matches[matches_any, , drop = FALSE])
        concepts <- concepts[order(relevance, decreasing = TRUE), ]
      }

      search_str <- paste(search_terms, collapse = ", ")
      if (nrow(concepts) > 0) {
        cli::cli_alert_success(
          "Found {nrow(concepts)} concept{?s} matching '{search_str}'"
        )
        if (nrow(concepts) > 5) {
          cli::cli_alert_info("Showing results ordered by relevance")
        }
      } else {
        cli::cli_alert_warning("No concepts found matching '{search_str}'")
        all_concepts <- data.frame(concept = unique(tolower(unlist(strsplit(
          paste(concepts$concept, concepts$related_concepts, sep = ", "),
          "[,\\s]+"
        )))))
        all_concepts <- all_concepts[all_concepts$concept != "", ]

        similar_concepts <- character(0)
        for (term in search_terms) {
          distances <- stringdist::stringdistmatrix(
            tolower(term),
            all_concepts$concept,
            method = "jw"
          )
          best_matches <- head(order(distances), 3)
          similar_concepts <- c(similar_concepts, all_concepts$concept[best_matches])
        }

        if (length(similar_concepts) > 0) {
          similar_concepts <- unique(similar_concepts)
          cli::cli_alert_info("Try searching for similar concepts:")
          for (concept in similar_concepts) {
            cli::cli_li("{.field {concept}}")
          }
        }

        cli::cli_alert_info(
          "Or use {.code bid_concepts()} without a search term to see all concepts"
        )
      }
    }
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
#' bid_concept("proximity") # Works with partial concept names
#'
#' @export
bid_concept <- function(concept_name) {
  all_concepts <- bid_concepts()

  result <- dplyr::filter(
    all_concepts,
    tolower(concept) == tolower(concept_name)
  )

  # try word-by-word partial match
  if (nrow(result) == 0) {
    words <- strsplit(tolower(concept_name), "\\s+")[[1]]
    if (length(words) > 1) {
      # if multi-word query, try to match all words in any order
      matches <- rep(TRUE, nrow(all_concepts))
      for (word in words) {
        if (nchar(word) >= 3) { # only use words with sufficient length
          matches <- matches & grepl(word, tolower(all_concepts$concept))
        }
      }
      if (any(matches)) {
        result <- all_concepts[matches, ]
      }
    }
  }

  # try general partial match
  if (nrow(result) == 0) {
    result <- dplyr::filter(
      all_concepts,
      grepl(tolower(concept_name), tolower(concept))
    )
  }

  # try matching by related concepts
  if (nrow(result) == 0) {
    related_matches <- sapply(all_concepts$related_concepts, function(related) {
      grepl(tolower(concept_name), tolower(related))
    })

    if (any(related_matches)) {
      result <- all_concepts[related_matches, ]
      similarities <- sapply(result$related_concepts, function(related) {
        match_words <- sum(sapply(strsplit(tolower(concept_name), "\\s+")[[1]], function(word) {
          grepl(word, tolower(related))
        }))
        return(match_words)
      })
      result <- result[order(similarities, decreasing = TRUE), ]
    }
  }

  # fuzzy matching suggestions
  if (nrow(result) == 0) {
    # calculate string distances
    distances <- stringdist::stringdistmatrix(
      tolower(concept_name),
      tolower(all_concepts$concept),
      method = "jw"
    )

    closest_indices <- order(distances)[1:min(5, length(distances))]
    closest_matches <- all_concepts$concept[closest_indices]
    match_scores <- 1 - distances[closest_indices]
    match_categories <- all_concepts$category[closest_indices]

    synonyms <- list(
      "hierarchy" = c("structure", "organization", "layout"),
      "cognitive" = c("mental", "thinking", "brain"),
      "bias" = c("preference", "tendency", "inclination"),
      "visual" = c("display", "graphic", "interface"),
      "proximity" = c("nearness", "closeness", "grouping"),
      "choice" = c("option", "selection", "decision"),
      "information" = c("data", "content", "details"),
      "feedback" = c("response", "reaction", "output"),
      "user" = c("person", "visitor", "customer")
    )

    query_words <- strsplit(tolower(concept_name), "\\s+")[[1]]
    synonym_matches <- integer(0)

    for (i in seq_along(query_words)) {
      word <- query_words[i]
      for (syn_key in names(synonyms)) {
        if (word %in% synonyms[[syn_key]] || grepl(word, syn_key)) {
          matches <- grepl(syn_key, tolower(all_concepts$concept))
          if (any(matches)) {
            synonym_matches <- c(synonym_matches, which(matches))
          }
        }
      }
    }

    if (length(synonym_matches) > 0) {
      synonym_concepts <- all_concepts$concept[unique(synonym_matches)]
      synonym_concepts <- setdiff(synonym_concepts, closest_matches)
      if (length(synonym_concepts) > 0) {
        closest_matches <- c(closest_matches, head(synonym_concepts, 2))
      }
    }

    cli::cli_h1("Concept Not Found")
    cli::cli_alert_warning(
      "Concept '{concept_name}' not found in the BID framework dictionary."
    )

    cli::cli_h2("Did you mean one of these?")
    for (i in seq_along(closest_matches)) {
      if (i <= length(match_scores)) {
        score_pct <- round(match_scores[i] * 100)
        category <- if (i <= length(match_categories)) match_categories[i] else ""

        if (score_pct >= 75) {
          match_indicator <- cli::col_green("\u2713 High match")
        } else if (score_pct >= 50) {
          match_indicator <- cli::col_yellow("\u26A0 Possible match")
        } else {
          match_indicator <- cli::col_red("\u2022 Related concept")
        }

        cli::cli_li("{.strong {closest_matches[i]}} {match_indicator} ({category})")

        if (i <= 2) {
          concept_idx <- which(all_concepts$concept == closest_matches[i])[1]
          if (!is.na(concept_idx)) {
            desc <- all_concepts$description[concept_idx]
            cli::cli_alert_info("  {desc}")
          }
        }
      }
    }

    cli::cli_alert_success(
      "Try using {.code bid_concept(\"{closest_matches[1]}\")} to get more information."
    )

    cli::cli_alert_info(
      "Or use {.code bid_concepts()} to browse all available concepts."
    )

    suggestions <- tibble::tibble(suggested_concept = closest_matches)
    return(structure(NULL, suggestions = suggestions))
  }

  result <- result |>
    dplyr::mutate(
      recommendations = dplyr::case_when(
        category == "Stage 1" ~
          paste(
            "Consider how this concept can help identify friction points in",
            "your UI. Focus on reducing cognitive barriers for users."
          ),
        category == "Stage 2" ~
          paste(
            "Apply this concept to improve how users interpret data.",
            "Enhance information processing and meaning extraction."
          ),
        category == "Stage 3" ~
          paste(
            "Use this concept to better structure dashboard elements.",
            "Create intuitive layouts that match users' mental models."
          ),
        category == "Stage 4" ~
          paste(
            "Consider how this concept influences user decision-making.",
            "Mitigate cognitive biases to improve information interpretation."
          ),
        category == "Stage 5" ~
          paste(
            "Apply this concept to create a more memorable final impression.",
            "Help users extract actionable insights and collaborate effectively."
          ),
        category == "All Stages" ~
          paste(
            "This concept is relevant throughout the entire BID process.",
            "Incorporate it at each stage of your dashboard development."
          ),
        TRUE ~ "Consider how this concept can improve your dashboard."
      )
    )

  if (nrow(result) > 0 && "related_concepts" %in% names(result)) {
    related <- result$related_concepts[1]
    if (!is.na(related) && related != "") {
      cli::cli_h2("Related concepts you might find useful:")
      related_list <- unlist(strsplit(related, ",\\s*"))
      for (rel in related_list) {
        rel_info <- dplyr::filter(all_concepts, concept == rel)
        if (nrow(rel_info) > 0) {
          cli::cli_li("{.field {rel}}: {rel_info$description[1]}")
        } else {
          cli::cli_li("{.field {rel}}")
        }
      }
      cli::cli_alert_info(
        "To explore these concepts further, use {.code bid_concept(\"concept name\")}"
      )
    }
  }

  if (nrow(result) > 0) {
    cli::cli_alert_success("Found information for {.strong {result$concept[1]}}")
    cli::cli_alert_info("Category: {result$category[1]}")
  }

  return(result)
}
