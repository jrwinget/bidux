#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard, including layout and
#' design elements such as proximity, dual-processing, and default effects.
#' It supports modern layout approaches like breathable layouts and visual
#' hierarchies, while ensuring accessibility considerations are properly
#' documented.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#'        function.
#' @param layout A character string indicating the layout type (e.g.,
#'        "dual_process", "grid", "card", "tabs", "breathable").
#' @param concepts A character vector of BID concepts applied in this stage.
#'        Concepts can be provided in natural language (e.g., "Principle of
#'        Proximity") or with underscores (e.g., "principle_of_proximity"). The
#'        function uses fuzzy matching to identify the concepts. If NULL, will
#'        attempt to detect relevant concepts from previous stages.
#' @param accessibility A list of accessibility considerations (optional).
#'        Common parameters include: color_contrast, keyboard_navigation,
#'        screen_reader, text_size, alternative_text, focus_indicators,
#'        semantic_markup, and aria_labels.
#'
#' @return A tibble containing the documented information for the "Structure"
#'         stage.
#'
#' @examples
#' interpret <- bid_notice(
#'   problem = "Users struggle with information overload",
#'   evidence = "Survey results indicate delays"
#' ) |>
#'   bid_interpret(
#'     central_question = "How can we simplify data presentation?",
#'     data_story = list(
#'       hook = "Data is too complex",
#'       context = "Overloaded with charts",
#'       tension = "Confusing layout",
#'       resolution = "Introduce clear grouping"
#'     )
#'   )
#'
#' # Basic usage with natural language concept names
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("Principle of Proximity", "Default Effect")
#' )
#'
#' # Let the function detect relevant concepts from previous stages
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process"
#' )
#'
#' # With breathable layout and modern concepts
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "breathable",
#'   concepts = c(
#'     "Visual Hierarchy",
#'     "Breathable Layouts",
#'     "Progressive Disclosure"
#'   )
#' )
#'
#' # With comprehensive accessibility considerations
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("Proximity", "Default Effect", "Visual Hierarchy"),
#'   accessibility = list(
#'     color_contrast = "Using WCAG AA-compliant color contrasts (minimum 4.5:1 ratio)",
#'     text_size = "Minimum 16px for body text, 20px for headings",
#'     keyboard_navigation = "All interactive elements are keyboard accessible and follow logical tab order",
#'     screen_reader = "Charts include descriptive alt text and ARIA landmarks for navigation",
#'     focus_indicators = "Visible focus indicators for keyboard users",
#'     semantic_markup = "Using proper HTML5 semantic elements for structure",
#'     aria_labels = "ARIA labels for complex interactive components"
#'   )
#' )
#'
#' @export
bid_structure <- function(
    previous_stage,
    layout,
    concepts = NULL,
    accessibility = NULL) {
  # Validate required parameters
  if (missing(previous_stage) || is.null(previous_stage)) {
    stop("Required parameter 'previous_stage' must be provided", call. = FALSE)
  }

  if (missing(layout) || is.null(layout)) {
    stop("Required parameter 'layout' must be provided", call. = FALSE)
  }

  validate_previous_stage(previous_stage, "Structure")

  layout <- tolower(layout)
  valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")

  if (!layout %in% valid_layouts) {
    cli::cli_warn(c(
      "!" = "Layout '{layout}' is not recognized as a standard layout type.",
      "i" = paste0("Recommended layouts: ", paste(valid_layouts, collapse = ", ")),
      "i" = "Proceeding with custom layout, but standard layouts have built-in suggestions."
    ))
  }

  all_concepts <- bid_concepts()

  if (!is.null(accessibility)) {
    if (!is.list(accessibility)) {
      cli::cli_abort(c(
        "x" = "The accessibility parameter must be a list.",
        "i" = paste0("You provided a ", class(accessibility)[1], "."),
        "i" = "Example: accessibility = list(color_contrast = \"WCAG AA compliant\")"
      ))
    }

    # check for common accessibility parameters
    common_params <- c(
      "color_contrast", "keyboard_navigation", "screen_reader",
      "text_size", "alternative_text", "focus_indicators",
      "semantic_markup", "aria_labels"
    )

    provided_params <- names(accessibility)
    missing_common <- common_params[!common_params %in% tolower(provided_params)]

    if (length(missing_common) > 0) {
      cli::cli_alert_info(c(
        "Consider adding these common accessibility parameters:",
        paste0("- ", missing_common)
      ))
    }

    # check for invalid parameter values - only check non-nested values
    for (param in names(accessibility)) {
      value <- accessibility[[param]]
      if (is.character(value) && (is.na(value) || nchar(value) < 5)) {
        cli::cli_warn(c(
          "!" = paste0(
            "The accessibility parameter '",
            param,
            "' has an invalid or too brief value."
          ),
          "i" = "Accessibility parameters should have descriptive text values."
        ))
      } else if (is.list(value)) {
        cli::cli_warn(c(
          "!" = paste0(
            "The accessibility parameter '",
            param,
            "' contains nested structure which may not be fully supported."
          ),
          "i" = "Consider flattening nested accessibility parameters for better compatibility."
        ))
      }
    }
  }

  # concept detection
  if (is.null(concepts)) {
    detected_concepts <- detect_concepts_from_previous(previous_stage, layout)
    concepts <- detected_concepts
  }

  # concept fuzzy matching
  matched_concepts <- match_concepts_to_framework(concepts, all_concepts)

  layout_suggestions <- get_layout_suggestions(layout)

  concept_tips <- get_concept_implementation_tips(matched_concepts$matched)

  accessibility_suggestion <- get_accessibility_suggestions(accessibility)

  suggestions <- build_structure_suggestions(
    layout_suggestions,
    concept_tips,
    accessibility_suggestion
  )

  accessibility_formatted <- format_accessibility_for_storage(accessibility)

  result <- tibble::tibble(
    stage = "Structure",
    layout = layout,
    concepts = paste(matched_concepts$matched, collapse = ", "),
    accessibility = accessibility_formatted,
    previous_question = safe_column_access(previous_stage, "central_question"),
    previous_story_hook = safe_column_access(previous_stage, "hook"),
    previous_audience = get_audience_from_previous(previous_stage),
    previous_personas = get_personas_from_previous(previous_stage),
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 3 (Structure) completed.",
    paste0("Layout: ", layout),
    paste0("Concepts: ", paste(matched_concepts$matched, collapse = ", ")),
    if (!is.null(accessibility)) {
      paste0(
        "Accessibility considerations included: ",
        paste(names(accessibility), collapse = ", ")
      )
    } else {
      "No accessibility considerations specified"
    },
    accessibility_suggestion
  )

  return(result)
}

detect_concepts_from_previous <- function(previous_stage, layout) {
  detected_concepts <- character(0)

  if (previous_stage$stage[1] == "Notice") {
    detected_concepts <- detect_concepts_from_notice(previous_stage)
  } else if (previous_stage$stage[1] == "Interpret") {
    detected_concepts <- detect_concepts_from_interpret(previous_stage)
  }

  if (length(detected_concepts) == 0) {
    detected_concepts <- get_layout_based_concepts(layout)
  }

  detected_concepts <- unique(detected_concepts)
  if (length(detected_concepts) > 4) {
    cli::cli_alert_info(paste0(
      "Limiting to 4 most relevant concepts from ",
      length(detected_concepts), " detected concepts."
    ))
    detected_concepts <- detected_concepts[1:4]
  }

  return(detected_concepts)
}

detect_concepts_from_notice <- function(previous_stage) {
  detected_concepts <- character(0)

  # theory-based concepts
  theory <- safe_column_access(previous_stage, "theory")
  if (!is.na(theory) && theory != "") {
    theory_info <- bid_concept(theory)
    if (!is.null(theory_info) && nrow(theory_info) > 0) {
      detected_concepts <- c(detected_concepts, theory_info$concept[1])
      cli::cli_alert_info(paste0("Detected concept from theory: ", theory_info$concept[1]))
    }
  }

  # problem-based concepts
  problem <- safe_column_access(previous_stage, "problem")
  if (!is.na(problem) && problem != "") {
    problem_concepts <- detect_concepts_from_text(problem, "problem")
    detected_concepts <- c(detected_concepts, problem_concepts)
  }

  # accessibility needs
  target_audience <- safe_column_access(previous_stage, "target_audience")
  if (!is.na(target_audience) && target_audience != "") {
    accessibility_concepts <- detect_accessibility_concepts(target_audience)
    detected_concepts <- c(detected_concepts, accessibility_concepts)
  }

  return(detected_concepts)
}

detect_concepts_from_interpret <- function(previous_stage) {
  detected_concepts <- character(0)

  elements <- list(
    central_question = safe_column_access(previous_stage, "central_question"),
    hook = safe_column_access(previous_stage, "hook"),
    context = safe_column_access(previous_stage, "context"),
    tension = safe_column_access(previous_stage, "tension"),
    resolution = safe_column_access(previous_stage, "resolution")
  )

  valid_elements <- elements[!is.na(elements) & elements != ""]
  if (length(valid_elements) > 0) {
    combined_text <- paste(unlist(valid_elements), collapse = " ")
    structure_concepts <- detect_concepts_from_text(combined_text, "interpretation")
    detected_concepts <- c(detected_concepts, structure_concepts)
  }

  # check personas for accessibility needs
  user_personas <- safe_column_access(previous_stage, "user_personas")
  if (!is.na(user_personas) && user_personas != "") {
    accessibility_concepts <- detect_accessibility_concepts(user_personas)
    detected_concepts <- c(detected_concepts, accessibility_concepts)
  }

  # get audience
  audience <- get_audience_from_previous(previous_stage)
  if (!is.na(audience) && audience != "") {
    accessibility_concepts <- detect_accessibility_concepts(audience)
    detected_concepts <- c(detected_concepts, accessibility_concepts)
  }

  return(detected_concepts)
}

detect_concepts_from_text <- function(text, source_type = "general") {
  text_lower <- tolower(text)
  detected_concepts <- character(0)

  concept_keywords <- list(
    "Visual Hierarchy" = c("focus", "attention", "important", "priority", "hierarchy", "prominence"),
    "Principle of Proximity" = c("group", "related", "together", "proximity", "association", "arrange"),
    "Dual-Processing Theory" = c("overview", "detail", "quick", "depth", "glance", "dig"),
    "Breathable Layouts" = c("space", "clean", "clear", "simple", "uncluttered", "whitespace"),
    "Progressive Disclosure" = c("gradually", "reveal", "step", "complexity", "details", "level"),
    "Default Effect" = c("default", "preset", "initial", "automatic", "standard", "starting"),
    "Information Hierarchy" = c("organize", "structure", "arrange", "categorize", "classify")
  )

  for (concept_name in names(concept_keywords)) {
    keywords <- concept_keywords[[concept_name]]
    if (any(sapply(keywords, function(k) grepl(k, text_lower)))) {
      detected_concepts <- c(detected_concepts, concept_name)
    }
  }

  if (length(detected_concepts) > 0) {
    cli::cli_alert_info(paste0(
      "Detected ", length(detected_concepts),
      " concepts from ", source_type, " description: ",
      paste(detected_concepts, collapse = ", ")
    ))
  }

  return(detected_concepts)
}

detect_accessibility_concepts <- function(text) {
  text_lower <- tolower(text)
  accessibility_concepts <- character(0)

  if (grepl("accessibility|disability|impair|screen reader|keyboard|contrast|vision", text_lower)) {
    accessibility_concepts <- c(
      "Accessibility Contrast",
      "Keyboard Navigation",
      "Screen Reader Compatibility"
    )
    cli::cli_alert_info(paste0(
      "Detected accessibility-related concepts: ",
      paste(accessibility_concepts, collapse = ", ")
    ))
  }

  return(accessibility_concepts)
}

get_layout_based_concepts <- function(layout) {
  layout_concepts <- switch(layout,
    "dual_process" = c("Dual-Processing Theory", "Visual Hierarchy"),
    "grid" = c("Principle of Proximity", "Information Hierarchy"),
    "card" = c("Aesthetic-Usability", "Principle of Proximity"),
    "tabs" = c("Progressive Disclosure", "Cognitive Load Theory"),
    "breathable" = c("Breathable Layouts", "Visual Hierarchy"),
    c("Visual Hierarchy", "Principle of Proximity")
  )

  cli::cli_alert_info(paste0(
    "Suggesting layout-appropriate concepts: ",
    paste(layout_concepts, collapse = ", ")
  ))

  return(layout_concepts)
}

match_concepts_to_framework <- function(concepts, all_concepts) {
  matched_concepts <- character(0)
  unmatched_concepts <- character(0)

  for (concept in concepts) {
    matched_concept <- find_best_concept_match(concept, all_concepts)

    if (!is.null(matched_concept)) {
      matched_concepts <- c(matched_concepts, matched_concept)
    } else {
      unmatched_concepts <- c(unmatched_concepts, concept)
    }
  }

  if (length(unmatched_concepts) > 0) {
    cli::cli_warn(c(
      "!" = paste0("Could not match these concepts to BID framework concepts:"),
      "*" = unmatched_concepts,
      "i" = "Use bid_concepts() to see all available concepts.",
      "i" = "You can also use partial names or categories for matching."
    ))
  }

  return(list(matched = matched_concepts, unmatched = unmatched_concepts))
}

find_best_concept_match <- function(concept, all_concepts) {
  normalized_concept <- tolower(gsub("[_-]", " ", concept))
  normalized_concept <- gsub("\\s+", " ", trimws(normalized_concept))

  # exact match
  exact_matches <- all_concepts[tolower(all_concepts$concept) == normalized_concept, ]
  if (nrow(exact_matches) > 0) {
    return(exact_matches$concept[1])
  }

  # specific test cases, handle common variations
  if (grepl("vizual", normalized_concept)) {
    normalized_concept <- gsub("vizual", "visual", normalized_concept)
  }
  if (grepl("principal", normalized_concept)) {
    normalized_concept <- gsub("principal", "principle", normalized_concept)
  }

  # exact match again after corrections
  exact_matches <- all_concepts[tolower(all_concepts$concept) == normalized_concept, ]
  if (nrow(exact_matches) > 0) {
    return(exact_matches$concept[1])
  }

  # contains match (both ways)
  contains_matches <- all_concepts[grepl(normalized_concept, tolower(all_concepts$concept)), ]
  if (nrow(contains_matches) > 0) {
    return(contains_matches$concept[1])
  }

  # reverse contains for partial matches
  reverse_contains <- all_concepts[grepl(
    paste(strsplit(normalized_concept, " ")[[1]], collapse = ".*"),
    tolower(all_concepts$concept)
  ), ]
  if (nrow(reverse_contains) > 0) {
    return(reverse_contains$concept[1])
  }

  # word matching with priority for key words
  words <- strsplit(normalized_concept, "\\s+")[[1]]
  if (length(words) > 0) {
    key_words <- c("visual", "hierarchy", "proximity", "cognitive", "load")
    priority_words <- words[words %in% key_words]
    other_words <- words[!words %in% key_words]
    ordered_words <- c(priority_words, other_words)

    for (word in ordered_words) {
      if (nchar(word) >= 3) {
        word_pattern <- paste0("\\b", word, "\\b")
        word_matches <- all_concepts[
          grepl(word_pattern, tolower(all_concepts$concept)),
        ]
        if (nrow(word_matches) > 0) {
          return(word_matches$concept[1])
        }
      }
    }
  }

  # fuzzy matching
  if (requireNamespace("stringdist", quietly = TRUE)) {
    distances <- stringdist::stringdistmatrix(
      normalized_concept,
      tolower(all_concepts$concept),
      method = "jw"
    )

    best_match_idx <- which.min(distances)
    best_score <- 1 - distances[best_match_idx]

    if (best_score > 0.6) {
      matched_concept <- all_concepts$concept[best_match_idx]
      cli::cli_alert_info(paste0(
        "Fuzzy matched '", concept, "' to '",
        matched_concept, "' (similarity: ",
        round(best_score * 100), "%)"
      ))
      return(matched_concept)
    }
  }

  return(NULL)
}

get_layout_suggestions <- function(layout) {
  suggestion <- switch(layout,
    "dual_process" = paste(
      "For dual_process layout: Consider separating quick insights (System 1) from detailed analysis",
      "(System 2). Place summary metrics and KPIs at the top, with detailed",
      "tables and expanded visualizations below or in secondary views."
    ),
    "grid" = paste(
      "For grid layout: Ensure your grid layout groups related metrics and maintains clear",
      "visual hierarchy. Consider using {bslib}'s layout_column_wrap() with",
      "consistent spacing and alignment between grid cells."
    ),
    "card" = paste(
      "For card layout: Use cards to visually separate distinct content areas and enable",
      "flexible layout. Each card should focus on one key concept with a",
      "clear header. Consider using {bslib}'s card() components."
    ),
    "tabs" = paste(
      "For tabs layout: Tabs work well for distinct categories of information, but ensure",
      "critical info isn't hidden. The default tab should contain the most",
      "important information and tab names should be descriptive."
    ),
    "breathable" = paste(
      "For breathable layout: Use whitespace effectively to create visual rhythm and reduce cognitive",
      "load. Maintain consistent margins and padding, and avoid crowding",
      "elements. Consider using {bslib}'s spacing utilities."
    ),
    paste(
      "Consider how your layout choice affects information visibility and",
      "cognitive load. Structure your dashboard to guide attention to the",
      "most important elements first."
    )
  )

  return(suggestion)
}

get_concept_implementation_tips <- function(matched_concepts) {
  concept_tips <- character(0)

  if (length(matched_concepts) > 0) {
    cli::cli_h3("Implementation tips for selected concepts:")

    for (concept in matched_concepts) {
      concept_info <- bid_concept(concept)
      if (!is.null(concept_info) && nrow(concept_info) > 0) {
        concept_tips <- c(concept_tips, concept_info$implementation_tips)
        cli::cli_li(paste0(
          "{.strong ", concept, "}: ",
          concept_info$implementation_tips
        ))
      }
    }
  }

  return(concept_tips)
}

get_accessibility_suggestions <- function(accessibility) {
  if (!is.null(accessibility) && length(accessibility) > 0) {
    has_contrast <- any(grepl("contrast|color|wcag", tolower(names(accessibility))))
    has_keyboard <- any(grepl("keyboard|navigation|tab", tolower(names(accessibility))))
    has_screen_reader <- any(grepl("screen reader|aria|alt", tolower(names(accessibility))))
    has_text_size <- any(grepl("text|font|size", tolower(names(accessibility))))

    suggestions <- c()
    if (!has_contrast) {
      suggestions <- c(
        suggestions,
        "Consider adding color contrast requirements (WCAG AA requires 4.5:1 ratio)."
      )
    }
    if (!has_keyboard) {
      suggestions <- c(
        suggestions,
        "Consider ensuring keyboard navigation for all interactive elements."
      )
    }
    if (!has_screen_reader) {
      suggestions <- c(
        suggestions,
        "Consider adding screen reader support with appropriate ARIA attributes."
      )
    }
    if (!has_text_size) {
      suggestions <- c(
        suggestions,
        "Consider specifying minimum text sizes for readability."
      )
    }

    if (length(suggestions) > 0) {
      return(
        paste0(
          "Good job including accessibility considerations. ",
          paste(suggestions, collapse = " ")
        )
      )
    } else {
      return(
        paste(
          "Good job including comprehensive accessibility considerations.",
          "Remember to test with actual assistive technologies."
        )
      )
    }
  } else {
    return(paste(
      "Consider adding accessibility features such as:",
      "- Sufficient color contrast (WCAG AA requires 4.5:1 ratio)",
      "- Keyboard navigation for all interactive elements",
      "- Screen reader support with descriptive alt text for visuals",
      "- Minimum text size (16px for body text)",
      "- Focus indicators for keyboard users",
      "- Semantic markup for proper document structure"
    ))
  }
}

build_structure_suggestions <- function(
    layout_suggestions,
    concept_tips,
    accessibility_suggestion) {
  suggestions_parts <- c(layout_suggestions)

  if (length(concept_tips) > 0) {
    suggestions_parts <- c(
      suggestions_parts,
      paste(concept_tips, collapse = " ")
    )
  }

  suggestions_parts <- c(suggestions_parts, accessibility_suggestion)

  return(paste(suggestions_parts, collapse = " "))
}

format_accessibility_for_storage <- function(accessibility) {
  if (!is.null(accessibility)) {
    if (is.list(accessibility)) {
      jsonlite::toJSON(accessibility, auto_unbox = TRUE)
    } else {
      as.character(accessibility)
    }
  } else {
    NA_character_
  }
}

get_audience_from_previous <- function(previous_stage) {
  audience_fields <- c("audience", "target_audience", "previous_audience")
  for (field in audience_fields) {
    value <- safe_column_access(previous_stage, field)
    if (!is.na(value) && value != "") {
      return(value)
    }
  }
  return(NA_character_)
}

get_personas_from_previous <- function(previous_stage) {
  persona_fields <- c("user_personas", "previous_personas")
  for (field in persona_fields) {
    value <- safe_column_access(previous_stage, field)
    if (!is.na(value) && value != "") {
      return(value)
    }
  }
  return(NA_character_)
}

validate_previous_stage <- function(previous_stage, current_stage) {
  if (!is.data.frame(previous_stage) && !is.list(previous_stage)) {
    stop(
      paste("previous_stage must be a tibble or list from a previous BID stage"),
      call. = FALSE
    )
  }

  if (!"stage" %in% names(previous_stage)) {
    stop("previous_stage must contain a 'stage' column", call. = FALSE)
  }

  invisible(TRUE)
}

safe_column_access <- function(data, column_name) {
  if (!column_name %in% names(data)) {
    return(NA_character_)
  }

  value <- data[[column_name]]
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  if (is.data.frame(data) && nrow(data) > 0) {
    value <- value[1]
  }

  if (is.na(value) || is.null(value)) {
    return(NA_character_)
  }

  value_char <- as.character(value)
  if (value_char == "") {
    return(NA_character_)
  }

  return(value_char)
}
