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
  validate_required_params(previous_stage = previous_stage, layout = layout)
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

    # check for invalid parameter values
    for (param in names(accessibility)) {
      value <- accessibility[[param]]
      if (!is.character(value) || is.na(value) || nchar(value) < 5) {
        cli::cli_warn(c(
          "!" = paste0("The accessibility parameter '", param, "' has an invalid or too brief value."),
          "i" = "Accessibility parameters should have descriptive text values."
        ))
      }
    }
  }

  # concept detection
  if (is.null(concepts)) {
    detected_concepts <- character(0)

    if (previous_stage$stage[1] == "Notice") {
      theory <- if ("theory" %in% names(previous_stage)) {
        previous_stage$theory[1] %||% NA_character_
      } else {
        NA_character_
      }

      if (!is.na(theory)) {
        theory_info <- bid_concept(theory)
        if (!is.null(theory_info) && nrow(theory_info) > 0) {
          detected_concepts <- c(detected_concepts, theory_info$concept[1])
          cli::cli_alert_info(paste0("Detected concept from theory: ", theory_info$concept[1]))
        }
      }

      problem <- if ("problem" %in% names(previous_stage)) {
        previous_stage$problem[1] %||% NA_character_
      } else {
        NA_character_
      }

      if (!is.na(problem)) {
        problem_lower <- tolower(problem)

        concept_keywords <- list(
          "visual hierarchy" = c("layout", "hierarchy", "attention", "focus", "important", "prominence"),
          "principle of proximity" = c("group", "related", "together", "proximity", "cluster", "organization"),
          "breathable layouts" = c("space", "whitespace", "breathing", "clean", "uncluttered", "minimal"),
          "cognitive load" = c("complex", "overwhelm", "confus", "mental", "load", "difficult"),
          "hick's law" = c("choice", "option", "select", "decision", "choose", "menu", "dropdown"),
          "progressive disclosure" = c("reveal", "gradually", "step", "hide", "show", "expand"),
          "interaction hints" = c("cue", "hint", "indicator", "suggest", "guide", "show how"),
          "visual feedback" = c("response", "feedback", "react", "indicate", "show result", "confirm"),
          "gestalt principles" = c("gestalt", "similarity", "proximity", "closure", "group", "perceive")
        )

        detected_keyword_concepts <- character(0)
        for (concept_name in names(concept_keywords)) {
          keywords <- concept_keywords[[concept_name]]
          if (any(sapply(keywords, function(k) grepl(k, problem_lower)))) {
            detected_keyword_concepts <- c(detected_keyword_concepts, concept_name)
          }
        }

        if (length(detected_keyword_concepts) > 0) {
          detected_concepts <- c(detected_concepts, detected_keyword_concepts)
          cli::cli_alert_info(paste0(
            "Detected ", length(detected_keyword_concepts),
            " concepts from problem description: ",
            paste(detected_keyword_concepts, collapse = ", ")
          ))
        }
      }

      # check for accessibility needs from target audience
      target_audience <- if ("target_audience" %in% names(previous_stage)) {
        previous_stage$target_audience[1] %||% NA_character_
      } else {
        NA_character_
      }

      if (!is.na(target_audience)) {
        audience_lower <- tolower(target_audience)
        if (
          grepl(
            "accessibility|disability|impair|screen reader|keyboard|contrast",
            audience_lower
          )
        ) {
          accessibility_concepts <- c(
            "Accessibility Contrast",
            "Keyboard Navigation",
            "Screen Reader Compatibility"
          )
          detected_concepts <- c(detected_concepts, accessibility_concepts)
          cli::cli_alert_info(paste0(
            "Detected accessibility-related concepts from target audience: ",
            paste(accessibility_concepts, collapse = ", ")
          ))
        }
      }
    } else if (previous_stage$stage[1] == "Interpret") {
      elements <- list(
        central_question = if ("central_question" %in% names(previous_stage)) {
          previous_stage$central_question[1] %||% NA_character_
        } else {
          NA_character_
        },
        hook = if ("hook" %in% names(previous_stage)) {
          previous_stage$hook[1] %||% NA_character_
        } else {
          NA_character_
        },
        context = if ("context" %in% names(previous_stage)) {
          previous_stage$context[1] %||% NA_character_
        } else {
          NA_character_
        },
        tension = if ("tension" %in% names(previous_stage)) {
          previous_stage$tension[1] %||% NA_character_
        } else {
          NA_character_
        },
        resolution = if ("resolution" %in% names(previous_stage)) {
          previous_stage$resolution[1] %||% NA_character_
        } else {
          NA_character_
        }
      )

      combined_text <- tolower(paste(
        unlist(elements[!is.na(elements)]),
        collapse = " "
      ))

      structure_keywords <- list(
        "visual hierarchy" = c("focus", "attention", "important", "priority", "hierarchy", "prominence"),
        "principle of proximity" = c("group", "related", "together", "proximity", "association", "arrange"),
        "dual-processing theory" = c("overview", "detail", "quick", "depth", "glance", "dig"),
        "breathable layouts" = c("space", "clean", "clear", "simple", "uncluttered", "whitespace"),
        "progressive disclosure" = c("gradually", "reveal", "step", "complexity", "details", "level"),
        "default effect" = c("default", "preset", "initial", "automatic", "standard", "starting"),
        "interaction hints" = c("cue", "hint", "indicator", "suggest", "guide", "show how"),
        "visual feedback" = c("response", "feedback", "react", "indicate", "show result", "confirm")
      )

      detected_interpret_concepts <- character(0)
      for (concept_name in names(structure_keywords)) {
        keywords <- structure_keywords[[concept_name]]
        if (any(sapply(keywords, function(k) grepl(k, combined_text)))) {
          detected_interpret_concepts <- c(detected_interpret_concepts, concept_name)
        }
      }

      if (length(detected_interpret_concepts) > 0) {
        detected_concepts <- c(detected_concepts, detected_interpret_concepts)
        cli::cli_alert_info(paste0(
          "Detected ", length(detected_interpret_concepts),
          " concepts from interpretation data: ",
          paste(detected_interpret_concepts, collapse = ", ")
        ))
      }

      # check personas for accessibility needs
      if (
        "user_personas" %in% names(previous_stage) &&
          !is.na(previous_stage$user_personas[1])
      ) {
        personas_text <- tolower(previous_stage$user_personas[1])
        if (
          grepl(
            "accessibility|disability|impair|screen reader|keyboard|contrast|vision",
            personas_text
          )
        ) {
          accessibility_concepts <- c(
            "Accessibility Contrast",
            "Keyboard Navigation",
            "Screen Reader Compatibility"
          )
          detected_concepts <- c(detected_concepts, accessibility_concepts)
          cli::cli_alert_info(paste0(
            "Detected accessibility-related concepts from user personas: ",
            paste(accessibility_concepts, collapse = ", ")
          ))
        }
      }

      # get audience from interpret stage or previous stage
      audience <- if ("audience" %in% names(previous_stage)) {
        previous_stage$audience[1] %||% NA_character_
      } else if ("previous_audience" %in% names(previous_stage)) {
        previous_stage$previous_audience[1] %||% NA_character_
      } else {
        NA_character_
      }

      if (!is.na(audience)) {
        audience_lower <- tolower(audience)
        if (
          grepl(
            "accessibility|disability|impair|screen reader|keyboard|contrast",
            audience_lower
          )
        ) {
          accessibility_concepts <- c(
            "Accessibility Contrast",
            "Keyboard Navigation",
            "Screen Reader Compatibility"
          )
          detected_concepts <- c(detected_concepts, accessibility_concepts)
          cli::cli_alert_info(paste0(
            "Detected accessibility-related concepts from audience data: ",
            paste(accessibility_concepts, collapse = ", ")
          ))
        }
      }
    }

    if (length(detected_concepts) == 0) {
      layout_based_concepts <- switch(layout,
        "dual_process" = c("Dual-Processing Theory", "Visual Hierarchy"),
        "grid" = c("Principle of Proximity", "Information Hierarchy"),
        "card" = c("Aesthetic-Usability", "Principle of Proximity"),
        "tabs" = c("Progressive Disclosure", "Cognitive Load Theory"),
        "breathable" = c("Breathable Layouts", "Visual Hierarchy"),
        c("Visual Hierarchy", "Principle of Proximity")
      )
      detected_concepts <- c(detected_concepts, layout_based_concepts)
      cli::cli_alert_info(paste0(
        "Suggesting layout-appropriate concepts: ",
        paste(layout_based_concepts, collapse = ", ")
      ))
    }

    if (length(detected_concepts) > 0) {
      detected_concepts <- detected_concepts[!duplicated(tolower(detected_concepts))]
    }

    if (length(detected_concepts) > 4) {
      cli::cli_alert_info(paste0(
        "Limiting to 4 most relevant concepts from ",
        length(detected_concepts), " detected concepts."
      ))
      detected_concepts <- detected_concepts[1:4]
    }

    concepts <- detected_concepts
  }

  # concept fuzzy matching
  matched_concepts <- character(0)
  unmatched_concepts <- character(0)

  for (concept in concepts) {
    normalized_concept <- tolower(gsub("_", " ", concept))

    # try exact match
    exact_matches <- all_concepts[tolower(all_concepts$concept) == normalized_concept, ]

    # try contains match
    if (nrow(exact_matches) == 0) {
      contains_matches <- all_concepts[grepl(normalized_concept, tolower(all_concepts$concept)), ]

      # if multiple, sort by string similarity
      if (nrow(contains_matches) > 1) {
        similarities <- sapply(tolower(contains_matches$concept), function(c) {
          stringdist::stringsim(normalized_concept, c, method = "jw")
        })
        contains_matches <- contains_matches[order(similarities, decreasing = TRUE), ]
      }

      if (nrow(contains_matches) > 0) {
        exact_matches <- contains_matches[1, , drop = FALSE]
      }
    }

    # try word matching
    if (nrow(exact_matches) == 0) {
      words <- strsplit(normalized_concept, "\\s+")[[1]]
      if (length(words) > 0) {
        word_matches <- NULL
        word_scores <- numeric(0)

        for (word in words) {
          if (nchar(word) >= 3) { # only use words with sufficient length
            matches <- all_concepts[
              grepl(word, tolower(all_concepts$concept)),
            ]

            if (nrow(matches) > 0) {
              # match score based on how much of the original concept it covers
              for (i in 1:nrow(matches)) {
                match_concept <- tolower(matches$concept[i])
                matched_words <- sum(sapply(words, function(w) grepl(w, match_concept)))
                score <- matched_words/length(words)

                if (is.null(word_matches)) {
                  word_matches <- list(matches[i, , drop = FALSE])
                } else {
                  word_matches <- c(word_matches, list(matches[i, , drop = FALSE]))
                }
                word_scores <- c(word_scores, score)
              }
            }
          }
        }

        if (length(word_matches) > 0) {
          best_match_idx <- which.max(word_scores)
          exact_matches <- word_matches[[best_match_idx]]
        }
      }
    }

    # use stringdist for fuzzy matching as last resort
    if (nrow(exact_matches) == 0) {
      distances <- stringdist::stringdistmatrix(
        normalized_concept,
        tolower(all_concepts$concept),
        method = "jw"
      )

      best_match_idx <- which.min(distances)
      best_score <- 1 - distances[best_match_idx]

      if (best_score > 0.7) {
        exact_matches <- all_concepts[best_match_idx, , drop = FALSE]
        cli::cli_alert_info(paste0(
          "Fuzzy matched '", concept, "' to '",
          exact_matches$concept[1], "' (similarity: ",
          round(best_score * 100), "%)"
        ))
      }
    }

    if (nrow(exact_matches) > 0) {
      matched_concepts <- c(matched_concepts, exact_matches$concept[1])
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

  layout_suggestions <- switch(layout,
    "dual_process" = paste(
      "Consider separating quick insights (System 1) from detailed analysis",
      "(System 2). Place summary metrics and KPIs at the top, with detailed",
      "tables and expanded visualizations below or in secondary views."
    ),
    "grid" = paste(
      "Ensure your grid layout groups related metrics and maintains clear",
      "visual hierarchy. Consider using {bslib}'s layout_column_wrap() with",
      "consistent spacing and alignment between grid cells."
    ),
    "card" = paste(
      "Use cards to visually separate distinct content areas and enable",
      "flexible layout. Each card should focus on one key concept with a",
      "clear header. Consider using {bslib}'s card() components."
    ),
    "tabs" = paste(
      "Tabs work well for distinct categories of information, but ensure",
      "critical info isn't hidden. The default tab should contain the most",
      "important information and tab names should be descriptive."
    ),
    "breathable" = paste(
      "Use whitespace effectively to create visual rhythm and reduce cognitive",
      "load. Maintain consistent margins and padding, and avoid crowding",
      "elements. Consider using {bslib}'s spacing utilities."
    ),
    paste(
      "Consider how your layout choice affects information visibility and",
      "cognitive load. Structure your dashboard to guide attention to the",
      "most important elements first."
    )
  )

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

  if (is.null(accessibility)) {
    needs_accessibility <- FALSE

    if ("target_audience" %in% names(previous_stage) &&
      !is.na(previous_stage$target_audience[1]) &&
      !is.null(previous_stage$target_audience[1])) {
      audience_lower <- tolower(previous_stage$target_audience[1])
      if (grepl("accessibility|disability|impair|screen reader|keyboard|contrast", audience_lower)) {
        needs_accessibility <- TRUE
        cli::cli_alert_info(c(
          "i" = "Detected potential accessibility needs in target audience.",
          "i" = "Consider adding accessibility parameters to your design."
        ))
      }
    }

    if ("user_personas" %in% names(previous_stage) &&
      !is.na(previous_stage$user_personas[1])) {
      personas_lower <- tolower(previous_stage$user_personas[1])
      if (grepl("accessibility|disability|impair|screen reader|keyboard|contrast", personas_lower)) {
        needs_accessibility <- TRUE
        cli::cli_alert_info(c(
          "i" = "Detected potential accessibility needs in user personas.",
          "i" = "Consider adding accessibility parameters to your design."
        ))
      }
    }
  }

  if (!is.null(accessibility) && length(accessibility) > 0) {
    has_contrast <- any(grepl("contrast|color|wcag", tolower(names(accessibility))))
    has_keyboard <- any(grepl("keyboard|navigation|tab", tolower(names(accessibility))))
    has_screen_reader <- any(grepl("screen reader|aria|alt", tolower(names(accessibility))))
    has_text_size <- any(grepl("text|font|size", tolower(names(accessibility))))

    accessibility_suggestions <- c()
    if (!has_contrast) {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "Consider adding color contrast requirements (WCAG AA requires 4.5:1 ratio)."
      )
    }
    if (!has_keyboard) {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "Consider ensuring keyboard navigation for all interactive elements."
      )
    }
    if (!has_screen_reader) {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "Consider adding screen reader support with appropriate ARIA attributes."
      )
    }
    if (!has_text_size) {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "Consider specifying minimum text sizes for readability."
      )
    }

    if (length(accessibility_suggestions) > 0) {
      accessibility_suggestion <- paste0(
        "Good job including accessibility considerations. ",
        paste(accessibility_suggestions, collapse = " ")
      )
    } else {
      accessibility_suggestion <- paste(
        "Good job including comprehensive accessibility considerations.",
        "Remember to test with actual assistive technologies."
      )
    }
  } else {
    accessibility_suggestion <- paste(
      "Consider adding accessibility features such as:",
      "- Sufficient color contrast (WCAG AA requires 4.5:1 ratio)",
      "- Keyboard navigation for all interactive elements",
      "- Screen reader support with descriptive alt text for visuals",
      "- Minimum text size (16px for body text)",
      "- Focus indicators for keyboard users",
      "- Semantic markup for proper document structure"
    )
  }

  accessibility_formatted <- if (!is.null(accessibility) && length(accessibility) > 0) {
    if (is.list(accessibility)) {
      jsonlite::toJSON(accessibility)
    } else {
      as.character(accessibility)
    }
  } else {
    NA_character_
  }

  if (length(concept_tips) > 0) {
    suggestions <- paste(
      layout_suggestions,
      paste(concept_tips, collapse = " "),
      accessibility_suggestion,
      sep = " "
    )
  } else {
    suggestions <- paste(
      layout_suggestions,
      accessibility_suggestion,
      sep = " "
    )
  }

  result <- tibble::tibble(
    stage = "Structure",
    layout = layout,
    concepts = paste(matched_concepts, collapse = ", "),
    accessibility = accessibility_formatted,
    previous_question = if ("central_question" %in% names(previous_stage)) {
      previous_stage$central_question[1] %||% NA_character_
    } else {
      NA_character_
    },
    previous_story_hook = if ("hook" %in% names(previous_stage)) {
      previous_stage$hook[1] %||% NA_character_
    } else {
      NA_character_
    },
    previous_audience = if ("audience" %in% names(previous_stage)) {
      previous_stage$audience[1] %||% NA_character_
    } else if ("previous_audience" %in% names(previous_stage)) {
      previous_stage$previous_audience[1] %||% NA_character_
    } else {
      NA_character_
    },
    previous_personas = if ("user_personas" %in% names(previous_stage)) {
      previous_stage$user_personas[1] %||% NA_character_
    } else if ("previous_personas" %in% names(previous_stage)) {
      previous_stage$previous_personas[1] %||% NA_character_
    } else {
      NA_character_
    },
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 3 (Structure) completed.",
    paste0("Layout: ", layout),
    paste0("Concepts: ", paste(matched_concepts, collapse = ", ")),
    if (!is.null(accessibility)) {
      paste0("Accessibility considerations included: ", paste(names(accessibility), collapse = ", "))
    } else {
      "No accessibility considerations specified"
    },
    accessibility_suggestion
  )

  return(result)
}
