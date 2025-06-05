# Functions for loading and using external knowledge base mappings

#' Load theory mappings from external file or use defaults
#'
#' @param custom_mappings Optional custom mappings data frame
#' @return Data frame with theory mappings
#' @keywords internal
load_theory_mappings <- function(custom_mappings = NULL) {
  if (!is.null(custom_mappings)) {
    required_cols <- c("keywords", "theory", "confidence")
    if (!all(required_cols %in% names(custom_mappings))) {
      stop(
        "Custom mappings must contain columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }
    return(custom_mappings)
  }

  mappings_file <- system.file(
    "extdata",
    "theory_mappings.csv",
    package = "bidux"
  )
  if (file.exists(mappings_file)) {
    tryCatch(
      {
        mappings <- readr::read_csv(mappings_file, show_col_types = FALSE)
        return(mappings)
      },
      error = function(e) {
        warning(
          "Could not load theory mappings file: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  return(get_default_theory_mappings())
}

#' Get default theory mappings (fallback)
#'
#' @return Data frame with default theory mappings
#' @keywords internal
get_default_theory_mappings <- function() {
  data.frame(
    keywords = c(
      "too many.*option|overwhelm.*too many|dropdown.*option|too many.*choice",
      "find.*information|search|locate|discover|navigation",
      "visual.*layout|hierarchy|organization|attention|cluttered",
      "complex|overwhelm|too much|confus|mental load|difficult",
      "mobile|touch|responsive|screen",
      "aesthetic|beautiful|appearance|design"
    ),
    theory = c(
      "Hick's Law",
      "Information Scent",
      "Visual Hierarchies",
      "Cognitive Load Theory",
      "Fitts's Law",
      "Aesthetic-Usability"
    ),
    confidence = c(0.9, 0.8, 0.85, 0.9, 0.8, 0.7),
    stringsAsFactors = FALSE
  )
}

#' Suggest theory based on problem and evidence using mappings
#'
#' @param problem Character string describing the problem
#' @param evidence Optional character string with supporting evidence
#' @param mappings Optional custom theory mappings
#' @return Character string with suggested theory
#' @export
suggest_theory_from_mappings <- function(
  problem,
  evidence = NULL,
  mappings = NULL
) {
  # If no problem text, fall back to Cognitive Load Theory
  if (is.null(problem) || is.na(problem) || nchar(trimws(problem)) == 0) {
    return("Cognitive Load Theory")
  }

  # Combine any user‐provided mappings with the built‐in defaults
  default_mappings <- get_default_theory_mappings()
  if (!is.null(mappings)) {
    # validate that 'mappings' has the required columns
    required_cols <- c("keywords", "theory", "confidence")
    if (!all(required_cols %in% names(mappings))) {
      stop(
        "Custom mappings must contain columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }
    # put user rows first so they take precedence
    theory_mappings <- rbind(mappings, default_mappings)
  } else {
    # no custom mappings parameter: attempt to load from extdata CSV
    theory_mappings <- load_theory_mappings(NULL)
  }

  combined_text <- tolower(paste(problem, evidence %||% "", sep = " "))

  best_match <- NULL
  best_confidence <- 0

  for (i in seq_len(nrow(theory_mappings))) {
    k <- theory_mappings$keywords[i]

    # Determine whether to treat 'k' as regex or as a literal token
    if (!is.null(mappings)) {
      # If user passed mappings, check if 'k' contains regex metacharacters
      if (grepl("[\\.\\*\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\]", k)) {
        found_i <- grepl(k, combined_text, perl = TRUE)
      } else {
        pattern_i <- paste0("\\b", k, "\\b")
        found_i <- grepl(pattern_i, combined_text, perl = TRUE)
      }
    } else {
      # default mappings always treat 'k' as regex
      found_i <- grepl(k, combined_text, perl = TRUE)
    }

    if (found_i) {
      conf_i <- theory_mappings$confidence[i]
      if (conf_i > best_confidence) {
        best_match <- theory_mappings$theory[i]
        best_confidence <- conf_i
      }
    }
  }

  return(best_match %||% "Cognitive Load Theory")
}

#' Load concept-bias mappings
#'
#' @param custom_mappings Optional custom mappings data frame
#' @return Data frame with concept-bias mappings
#' @keywords internal
load_concept_bias_mappings <- function(custom_mappings = NULL) {
  if (!is.null(custom_mappings)) {
    required_cols <- c("concept", "bias_type", "mitigation_strategy")
    if (!all(required_cols %in% names(custom_mappings))) {
      stop(
        "Custom concept-bias mappings must contain columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }
    return(custom_mappings)
  }

  mappings_file <- system.file(
    "extdata",
    "concept_bias_mappings.csv",
    package = "bidux"
  )
  if (file.exists(mappings_file)) {
    tryCatch(
      {
        return(readr::read_csv(mappings_file, show_col_types = FALSE))
      },
      error = function(e) {
        warning(
          "Could not load concept-bias mappings file: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  return(data.frame(
    concept = character(0),
    bias_type = character(0),
    mitigation_strategy = character(0),
    stringsAsFactors = FALSE
  ))
}

#' Get bias mitigation strategies for concepts
#'
#' @param concepts Character vector of concept names
#' @param mappings Optional custom concept-bias mappings
#' @return Data frame with relevant bias mappings
#' @export
get_concept_bias_mappings <- function(concepts, mappings = NULL) {
  if (length(concepts) == 0) {
    return(data.frame(
      concept = character(0),
      bias_type = character(0),
      mitigation_strategy = character(0),
      stringsAsFactors = FALSE
    ))
  }

  concept_mappings <- load_concept_bias_mappings(mappings)

  if (nrow(concept_mappings) == 0) {
    return(concept_mappings)
  }

  relevant_mappings <- concept_mappings[
    concept_mappings$concept %in% concepts,
  ]

  if (nrow(relevant_mappings) == 0) {
    for (concept in concepts) {
      partial_matches <- concept_mappings[
        grepl(concept, concept_mappings$concept, ignore.case = TRUE),
      ]
      relevant_mappings <- rbind(relevant_mappings, partial_matches)
    }
  }

  return(unique(relevant_mappings))
}

#' Load layout-concept mappings
#'
#' @param custom_mappings Optional custom mappings data frame
#' @return Data frame with layout-concept mappings
#' @keywords internal
load_layout_mappings <- function(custom_mappings = NULL) {
  if (!is.null(custom_mappings)) {
    required_cols <- c("layout", "primary_concepts", "description")
    if (!all(required_cols %in% names(custom_mappings))) {
      stop(
        "Custom layout mappings must contain columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }
    return(custom_mappings)
  }

  mappings_file <- system.file(
    "extdata",
    "layout_concepts.csv",
    package = "bidux"
  )
  if (file.exists(mappings_file)) {
    tryCatch(
      {
        return(readr::read_csv(mappings_file, show_col_types = FALSE))
      },
      error = function(e) {
        warning(
          "Could not load layout mappings file: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  return(get_default_layout_mappings())
}

#' Get default layout mappings (fallback)
#'
#' @return Data frame with default layout mappings
#' @keywords internal
get_default_layout_mappings <- function() {
  data.frame(
    layout = c("dual_process", "grid", "card", "tabs", "breathable"),
    primary_concepts = c(
      "Dual-Processing Theory,Visual Hierarchy",
      "Principle of Proximity,Information Hierarchy",
      "Aesthetic-Usability,Principle of Proximity",
      "Progressive Disclosure,Cognitive Load Theory",
      "Breathable Layouts,Visual Hierarchy"
    ),
    description = c(
      "Separates quick insights from detailed analysis",
      "Groups related metrics with clear visual hierarchy",
      "Organizes content in visually distinct containers",
      "Reduces complexity through progressive disclosure",
      "Uses whitespace for cognitive load reduction"
    ),
    stringsAsFactors = FALSE
  )
}

#' Get concepts recommended for a layout
#'
#' @param layout Character string indicating layout type
#' @param mappings Optional custom layout mappings
#' @return Character vector of recommended concepts
#' @export
get_layout_concepts <- function(layout, mappings = NULL) {
  if (is.null(layout) || is.na(layout) || nchar(trimws(layout)) == 0) {
    return(c("Visual Hierarchy", "Principle of Proximity"))
  }

  layout_mappings <- load_layout_mappings(mappings)
  layout_lower <- tolower(trimws(layout))

  exact_match <- layout_mappings[
    tolower(layout_mappings$layout) == layout_lower,
  ]
  if (nrow(exact_match) > 0) {
    concepts <- strsplit(exact_match$primary_concepts[1], ",")[[1]]
    return(trimws(concepts))
  }

  partial_match <- layout_mappings[
    grepl(layout_lower, tolower(layout_mappings$layout)),
  ]
  if (nrow(partial_match) > 0) {
    concepts <- strsplit(partial_match$primary_concepts[1], ",")[[1]]
    return(trimws(concepts))
  }

  return(c("Visual Hierarchy", "Principle of Proximity"))
}

#' Load accessibility guidelines
#'
#' @param custom_guidelines Optional custom guidelines data frame
#' @return Data frame with accessibility guidelines
#' @keywords internal
load_accessibility_guidelines <- function(custom_guidelines = NULL) {
  if (!is.null(custom_guidelines)) {
    return(custom_guidelines)
  }

  guidelines_file <- system.file(
    "extdata",
    "accessibility_guidelines.csv",
    package = "bidux"
  )
  if (file.exists(guidelines_file)) {
    tryCatch(
      {
        return(readr::read_csv(guidelines_file, show_col_types = FALSE))
      },
      error = function(e) {
        warning(
          "Could not load accessibility guidelines file: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  return(data.frame(
    guideline = c("color_contrast", "keyboard_navigation", "screen_reader"),
    requirement = c(
      "4.5:1 ratio for normal text",
      "All interactive elements keyboard accessible",
      "Descriptive alt text and ARIA labels"
    ),
    wcag_level = c("AA", "AA", "AA"),
    stringsAsFactors = FALSE
  ))
}

#' Get accessibility recommendations for a given context
#'
#' @param context Character string describing the interface context
#' @param guidelines Optional custom accessibility guidelines
#' @return Character vector of relevant accessibility recommendations
#' @export
get_accessibility_recommendations <- function(context = "", guidelines = NULL) {
  accessibility_guidelines <- load_accessibility_guidelines(guidelines)

  if (nrow(accessibility_guidelines) == 0) {
    return(
      "Consider basic accessibility: color contrast, keyboard navigation, screen reader support"
    )
  }

  context_lower <- tolower(context)
  relevant_guidelines <- character(0)

  if (grepl("visual|color|chart|graph", context_lower)) {
    color_guidelines <- accessibility_guidelines[
      grepl("color|contrast", accessibility_guidelines$guideline),
    ]
    if (nrow(color_guidelines) > 0) {
      relevant_guidelines <- c(
        relevant_guidelines,
        color_guidelines$requirement
      )
    }
  }

  if (grepl("interactive|button|input|form", context_lower)) {
    interaction_guidelines <- accessibility_guidelines[
      grepl("keyboard|focus|aria", accessibility_guidelines$guideline),
    ]
    if (nrow(interaction_guidelines) > 0) {
      relevant_guidelines <- c(
        relevant_guidelines,
        interaction_guidelines$requirement
      )
    }
  }

  if (grepl("chart|data|visualization", context_lower)) {
    data_guidelines <- accessibility_guidelines[
      grepl("screen.*reader|aria|semantic", accessibility_guidelines$guideline),
    ]
    if (nrow(data_guidelines) > 0) {
      relevant_guidelines <- c(relevant_guidelines, data_guidelines$requirement)
    }
  }

  if (length(relevant_guidelines) == 0) {
    top_guidelines <- accessibility_guidelines[
      accessibility_guidelines$wcag_level == "AA",
    ]
    if (nrow(top_guidelines) >= 3) {
      relevant_guidelines <- top_guidelines$requirement[1:3]
    } else {
      relevant_guidelines <- accessibility_guidelines$requirement[
        1:min(3, nrow(accessibility_guidelines))
      ]
    }
  }

  return(unique(relevant_guidelines))
}
