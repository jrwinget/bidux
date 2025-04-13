#' Document Dashboard Structure Stage in BID Framework
#'
#' @description
#' This function documents the structure of the dashboard, including layout and
#' design elements such as proximity, dual-processing, and default effects.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage function.
#' @param layout A character string indicating the layout type (e.g.,
#'        "dual_process", "grid", "card", "tabs", "breathable").
#' @param concepts A character vector of BID concepts applied in this stage.
#'        Concepts can be provided in natural language (e.g., "Principle of Proximity") 
#'        or with underscores (e.g., "principle_of_proximity"). The function uses
#'        fuzzy matching to identify the concepts. If NULL, will attempt to detect 
#'        relevant concepts from previous stages.
#' @param accessibility A list of accessibility considerations (optional).
#'
#' @return A tibble containing the documented information for the "Structure"
#'         stage.
#'
#' @examples
#' interpret <- bid_interpret(
#'   bid_notice(
#'     problem = "Users struggle with information overload",
#'     evidence = "Survey results indicate delays"
#'   ),
#'   central_question = "How can we simplify data presentation?",
#'   data_story = list(
#'     hook = "Data is too complex",
#'     context = "Overloaded with charts",
#'     tension = "Confusing layout",
#'     resolution = "Introduce clear grouping"
#'   )
#' )
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
#' # With accessibility considerations
#' bid_structure(
#'   previous_stage = interpret,
#'   layout = "dual_process",
#'   concepts = c("Proximity", "Default Effect", "Visual Hierarchy"),
#'   accessibility = list(
#'     color_contrast = "Using WCAG AA-compliant color contrasts",
#'     keyboard_navigation = "All interactive elements are keyboard accessible",
#'     screen_reader = "Charts include descriptive alt text"
#'   )
#' )
#'
#' @export
bid_structure <- function(
    previous_stage,
    layout,
    concepts = NULL,
    accessibility = NULL) {
  validate_required_params(
    previous_stage = previous_stage,
    layout = layout
  )
  
  validate_previous_stage(previous_stage, "Structure")

  layout <- tolower(layout)
  valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")
  
  if (!layout %in% valid_layouts) {
    warning(paste0(
      "Layout '", layout, "' is not one of the recommended layouts: ",
      paste(valid_layouts, collapse = ", "), ". ",
      "Proceeding with custom layout."
    ))
  }

  all_concepts <- bid_concepts()
  
  if (is.null(concepts)) {
    detected_concepts <- character(0)
    
    if (previous_stage$stage[1] == "Notice") {
      theory <- previous_stage$theory[1]
      if (!is.na(theory)) {
        theory_info <- bid_concept(theory)
        if (!is.null(theory_info) && nrow(theory_info) > 0) {
          detected_concepts <- c(detected_concepts, theory_info$concept[1])
        }
      }
      
      problem <- previous_stage$problem[1]
      if (!is.na(problem)) {
        problem_lower <- tolower(problem)
        
        concept_keywords <- list(
          "visual hierarchy" = c("layout", "hierarchy", "attention", "focus", "important"),
          "principle of proximity" = c("group", "related", "together", "proximity", "cluster"),
          "breathable layouts" = c("space", "whitespace", "breath", "clean", "uncluttered"),
          "cognitive load" = c("complex", "overwhelm", "confus", "mental", "load"),
          "hick's law" = c("choice", "option", "select", "decision", "choose", "menu")
        )
        
        for (concept_name in names(concept_keywords)) {
          keywords <- concept_keywords[[concept_name]]
          if (any(sapply(keywords, function(k) grepl(k, problem_lower)))) {
            detected_concepts <- c(detected_concepts, concept_name)
          }
        }
      }
    } 
    else if (previous_stage$stage[1] == "Interpret") {
      central_question <- previous_stage$central_question[1]
      hook <- previous_stage$hook[1]
      context <- previous_stage$context[1]
      tension <- previous_stage$tension[1]
      resolution <- previous_stage$resolution[1]
      
      combined_text <- tolower(paste(
        central_question, hook, context, tension, resolution, sep = " "
      ))
      
      structure_keywords <- list(
        "visual hierarchy" = c("focus", "attention", "important", "priority", "hierarchy"),
        "principle of proximity" = c("group", "related", "together", "proximity", "association"),
        "dual-processing theory" = c("overview", "detail", "quick", "depth", "glance", "dig"),
        "breathable layouts" = c("space", "clean", "clear", "simple", "uncluttered"),
        "progressive disclosure" = c("gradually", "reveal", "step", "complexity", "details"),
        "default effect" = c("default", "preset", "initial", "automatic", "standard")
      )
      
      for (concept_name in names(structure_keywords)) {
        keywords <- structure_keywords[[concept_name]]
        if (any(sapply(keywords, function(k) grepl(k, combined_text)))) {
          detected_concepts <- c(detected_concepts, concept_name)
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
      }
    }
    
    if (length(detected_concepts) > 4) {
      detected_concepts <- detected_concepts[1:4]
    }
    
    concepts <- detected_concepts
    
    if (length(concepts) > 0) {
      message(paste0(
        "Automatically detected relevant concepts based on previous stages: ",
        paste(concepts, collapse = ", "), "."
      ))
    } else {
      concepts <- switch(layout,
        "dual_process" = c("Dual-Processing Theory"),
        "grid" = c("Principle of Proximity"),
        "card" = c("Aesthetic-Usability"),
        "tabs" = c("Progressive Disclosure"),
        "breathable" = c("Breathable Layouts"),
        c("Visual Hierarchy")
      )
      message(paste0(
        "No concepts detected from previous stages. Using default concept for layout: ",
        paste(concepts, collapse = ", "), "."
      ))
    }
  }
  
  matched_concepts <- character(0)
  unmatched_concepts <- character(0)
  
  for (concept in concepts) {
    normalized_concept <- tolower(gsub("_", " ", concept))
    
    matches <- all_concepts[tolower(all_concepts$concept) == normalized_concept, ]
    
    if (nrow(matches) == 0) {
      matches <- all_concepts[grepl(normalized_concept, tolower(all_concepts$concept)), ]
    }
    
    if (nrow(matches) == 0) {
      distances <- stringdist::stringdistmatrix(
        normalized_concept,
        tolower(all_concepts$concept),
        method = "jw"
      )
      
      best_match_idx <- which.min(distances)
      if (distances[best_match_idx] < 0.3) {
        matches <- all_concepts[best_match_idx, ]
      }
    }
    
    if (nrow(matches) > 0) {
      matched_concepts <- c(matched_concepts, matches$concept[1])
    } else {
      unmatched_concepts <- c(unmatched_concepts, concept)
    }
  }
  
  if (length(unmatched_concepts) > 0) {
    warning(
      "Some concepts could not be matched to known BID concepts: ",
      paste(unmatched_concepts, collapse = ", "), ". ",
      "Use bid_concepts() to see all available concepts."
    )
  }

  layout_suggestions <- switch(layout,
    "dual_process" = paste(
      "Consider separating quick insights (System 1) from detailed analysis",
      "(System 2)."
    ),
    "grid" = paste(
      "Ensure your grid layout groups related metrics and maintains clear",
      "visual hierarchy."
    ),
    "card" = paste(
      "Use cards to visually separate distinct content areas and enable",
      "flexible layout."
    ),
    "tabs" = paste(
      "Tabs work well for distinct categories of information, but ensure",
      "critical info isn't hidden."
    ),
    "breathable" = paste(
      "Use whitespace effectively to create visual rhythm and reduce cognitive",
      "load."
    ),
    paste(
      "Consider how your layout choice affects information visibility and",
      "cognitive load."
    )
  )

  concept_tips <- character(0)
  for (concept in matched_concepts) {
    concept_info <- bid_concept(concept)
    if (!is.null(concept_info) && nrow(concept_info) > 0) {
      concept_tips <- c(concept_tips, concept_info$implementation_tips)
    }
  }

  if (is.null(accessibility)) {
    if (previous_stage$stage[1] == "Notice" && "target_audience" %in% names(previous_stage)) {
      audience <- previous_stage$target_audience[1]
      if (!is.na(audience) && !is.null(audience)) {
        audience_lower <- tolower(audience)
        if (grepl("accessibility|disability|impair|screen reader|keyboard|contrast", audience_lower)) {
          message("Detected potential accessibility needs based on target audience. Consider including accessibility parameters.")
        }
      }
    }
  }

  accessibility_suggestion <- if (!is.null(accessibility) && length(accessibility) > 0) {
    "Good job including accessibility considerations. Remember to test with actual assistive technologies."
  } else {
    paste(
      "Consider adding accessibility features like sufficient color contrast,",
      "keyboard navigation, and screen reader support."
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
    previous_question = if ("central_question" %in% names(previous_stage)) previous_stage$central_question[1] else NA_character_,
    previous_story_hook = if ("hook" %in% names(previous_stage)) previous_stage$hook[1] else NA_character_,
    previous_audience = if ("audience" %in% names(previous_stage)) previous_stage$audience[1] %||% NA_character_ else NA_character_,
    previous_personas = if ("user_personas" %in% names(previous_stage)) previous_stage$user_personas[1] %||% NA_character_ else NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 3 (Structure) completed.",
    paste0("Layout: ", layout),
    paste0("Concepts: ", paste(matched_concepts, collapse = ", ")),
    if (!is.null(accessibility)) {
      "Accessibility considerations included"
    } else {
      "No accessibility considerations specified"
    },
    accessibility_suggestion
  )

  return(result)
}
