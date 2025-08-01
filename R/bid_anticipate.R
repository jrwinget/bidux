#' Document User Behavior Anticipation Stage in BID Framework
#'
#' @description
#' This function documents the anticipated user behavior by listing bias
#' mitigation strategies related to anchoring, framing, confirmation bias, etc.
#' It also supports adding interaction hints and visual feedback elements.
#'
#' @param previous_stage A tibble or list output from an earlier BID stage
#'        function.
#' @param bias_mitigations A named list of bias mitigation strategies. If NULL,
#'        the function will suggest bias mitigations based on information from
#'        previous stages.
#' @param interaction_principles A named list of interaction principles
#'        (optional).
#'
#' @return A tibble containing the documented information for the "Anticipate"
#'         stage.
#'
#' @examples
#' structure_info <- bid_structure(
#'   bid_interpret(
#'     bid_notice(
#'       "Issue with dropdown menus",
#'       evidence = "User testing indicated delays"
#'     ),
#'     central_question = "How can we improve selection efficiency?",
#'     data_story = list(
#'       hook = "Too many options",
#'       context = "Excessive choices",
#'       tension = "User frustration",
#'       resolution = "Simplify menu"
#'     )
#'   ),
#'   layout = "dual_process",
#'   concepts = c("principle_of_proximity", "default_effect")
#' )
#'
#' # Basic usage
#' bid_anticipate(
#'   previous_stage = structure_info,
#'   bias_mitigations = list(
#'     anchoring = "Use context-aware references",
#'     framing = "Toggle between positive and negative framing"
#'   )
#' )
#'
#' # Let the function suggest bias mitigations based on previous stages
#' bid_anticipate(
#'   previous_stage = structure_info
#' )
#'
#' # With interaction principles
#' bid_anticipate(
#'   previous_stage = structure_info,
#'   bias_mitigations = list(
#'     anchoring = "Use context-aware references",
#'     framing = "Toggle between positive and negative framing"
#'   ),
#'   interaction_principles = list(
#'     hover_effects = "Show additional information on hover",
#'     selection_feedback = "Highlight active filters with color change",
#'     progressive_actions = "Show advanced options only if basic ones are used"
#'   )
#' )
#'
#' @export
bid_anticipate <- function(
  previous_stage,
  bias_mitigations = NULL,
  interaction_principles = NULL
) {
  if (missing(previous_stage) || is.null(previous_stage)) {
    stop("Required parameter 'previous_stage' must be provided", call. = FALSE)
  }

  validate_previous_stage(previous_stage, "Anticipate")

  if (!is.null(bias_mitigations)) {
    if (!is.list(bias_mitigations)) {
      cli::cli_abort(
        c(
          "The bias_mitigations parameter must be a list",
          "i" = "You provided {.cls {class(bias_mitigations)}}"
        )
      )
    }

    if (
      length(bias_mitigations) == 0 ||
        is.null(names(bias_mitigations)) ||
        any(names(bias_mitigations) == "")
    ) {
      cli::cli_warn(
        c(
          "!" = "bias_mitigations must be a non-empty named list.",
          "i" = "Using automatically generated bias mitigations instead."
        )
      )
      bias_mitigations <- NULL
    } else {
      empty_values <- sapply(bias_mitigations, function(x) {
        is.null(x) || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
      })

      if (any(empty_values)) {
        cli::cli_warn(
          c(
            "!" = "bias_mitigations must be a non-empty named list.",
            "i" = "Using automatically generated bias mitigations instead."
          )
        )
        bias_mitigations <- NULL
      } else {
        for (i in seq_along(bias_mitigations)) {
          if (!is.character(bias_mitigations[[i]])) {
            bias_mitigations[[i]] <- as.character(bias_mitigations[[i]])
          }
        }
      }
    }
  }

  if (!is.null(interaction_principles)) {
    if (!is.list(interaction_principles)) {
      cli::cli_abort(c(
        "The interaction_principles parameter must be a list",
        "i" = "You provided {.cls {class(interaction_principles)}}"
      ))
    }

    if (
      length(interaction_principles) == 0 ||
        is.null(names(interaction_principles)) ||
        any(names(interaction_principles) == "")
    ) {
      cli::cli_warn(
        c(
          "!" = "interaction_principles must be a non-empty named list.",
          "i" = "Using automatically generated interaction principles instead."
        )
      )
      interaction_principles <- NULL
    }

    for (i in seq_along(interaction_principles)) {
      if (!is.character(interaction_principles[[i]])) {
        interaction_principles[[i]] <- as.character(interaction_principles[[i]])
      }
    }
  }

  layout <- NA_character_
  if (
    "previous_layout" %in%
      names(previous_stage) &&
      !is.na(previous_stage$previous_layout[1])
  ) {
    layout <- previous_stage$previous_layout[1]
  } else if (
    "layout" %in% names(previous_stage) && !is.na(previous_stage$layout[1])
  ) {
    layout <- previous_stage$layout[1]
  }

  if (!is.na(layout)) {
    valid_layouts <- c("dual_process", "grid", "card", "tabs", "breathable")
    if (!layout %in% valid_layouts) {
      cli::cli_warn(
        c(
          "!" = "
            Layout '{layout}' is not recognized as a standard layout type.
          ",
          "i" = paste0(
            "Recommended layouts: ",
            paste(valid_layouts, collapse = ", ")
          ),
          "i" = paste(
            "Some automatic suggestions may not be optimized for",
            "your custom layout."
          )
        )
      )
    }
  }

  concepts <- character(0)
  if (
    "previous_concepts" %in%
      names(previous_stage) &&
      !is.na(previous_stage$previous_concepts[1])
  ) {
    concepts <- strsplit(previous_stage$previous_concepts[1], ", ")[[1]]
  } else if (
    "concepts" %in% names(previous_stage) && !is.na(previous_stage$concepts[1])
  ) {
    concepts <- strsplit(previous_stage$concepts[1], ", ")[[1]]
  }
  concepts <- trimws(concepts)
  concepts <- concepts[concepts != ""]

  accessibility <- NA_character_
  if (
    "previous_accessibility" %in%
      names(previous_stage) &&
      !is.na(previous_stage$previous_accessibility[1])
  ) {
    accessibility <- previous_stage$previous_accessibility[1]
  } else if (
    "accessibility" %in%
      names(previous_stage) &&
      !is.na(previous_stage$accessibility[1])
  ) {
    accessibility <- previous_stage$accessibility[1]
  }

  if (is.null(bias_mitigations)) {
    suggested_biases <- list()

    if (previous_stage$stage[1] == "Structure") {
      concept_bias_map <- list(
        "Dual-Processing Theory" = c("framing", "anchoring"),
        "Visual Hierarchy" = c("attention bias", "belief perseverance"),
        "Principle of Proximity" = c("association bias", "clustering illusion"),
        "Default Effect" = c("status quo bias", "choice architecture"),
        "Aesthetic-Usability" = c(
          "beautiful-is-good stereotype",
          "halo effect"
        ),
        "Information Hierarchy" = c("availability bias", "prominence effect")
      )

      for (concept in concepts) {
        concept <- trimws(concept)
        for (known_concept in names(concept_bias_map)) {
          if (grepl(tolower(known_concept), tolower(concept))) {
            biases <- concept_bias_map[[known_concept]]
            for (bias in biases) {
              suggested_biases[[bias]] <- paste(
                "Consider how this bias affects user decisions."
              )
            }
          }
        }
      }

      layout_bias_map <- list(
        "dual_process" = c(
          "framing" = "
            Toggle between high-level insights and detailed analysis views
          "
        ),
        "grid" = c(
          "anchoring" = "Provide multiple reference points across grid cells"
        ),
        "card" = c(
          "beautiful-is-good stereotype" = "
            Ensure card aesthetics don't overshadow content quality
          "
        ),
        "tabs" = c(
          "availability bias" = "
            Make important information available in the default tab
          "
        ),
        "breathable" = c(
          "cognitive load" = "
            Use whitespace to reduce cognitive load and improve focus
          "
        )
      )

      if (!is.na(layout) && layout %in% names(layout_bias_map)) {
        for (bias_name in names(layout_bias_map[[layout]])) {
          suggested_biases[[bias_name]] <- layout_bias_map[[layout]][[
            bias_name
          ]]
        }
      }
    } else if (previous_stage$stage[1] == "Interpret") {
      story_elements <- list()
      for (field in c("central_question", "hook", "tension", "resolution")) {
        if (
          field %in% names(previous_stage) && !is.na(previous_stage[[field]][1])
        ) {
          story_elements[[field]] <- previous_stage[[field]][1]
        } else {
          story_elements[[field]] <- NA_character_
        }
      }

      valid_elements <- !sapply(story_elements, is.na)
      if (any(valid_elements)) {
        combined_text <- tolower(paste(
          unlist(story_elements[valid_elements]),
          collapse = " "
        ))

        bias_keywords <- list(
          "anchoring" = c(
            "compar",
            "reference",
            "previous",
            "baseline",
            "target"
          ),
          "framing" = c("positiv", "negativ", "gain", "loss", "perspective"),
          "confirmation bias" = c(
            "belief",
            "assumption",
            "expect",
            "hypothesis",
            "valid"
          ),
          "availability bias" = c(
            "recent",
            "memorable",
            "example",
            "recall",
            "top of mind"
          ),
          "loss aversion" = c("risk", "loss", "gain", "averse", "avoid"),
          "recency bias" = c("recent", "latest", "new", "trend", "last")
        )

        for (bias_name in names(bias_keywords)) {
          keywords <- bias_keywords[[bias_name]]
          if (any(sapply(keywords, function(k) grepl(k, combined_text)))) {
            tension_field <- if (!is.na(story_elements$tension)) {
              "tension"
            } else {
              "data story"
            }
            suggested_biases[[bias_name]] <- paste(
              "Address",
              bias_name,
              "based on",
              tension_field
            )
          }
        }
      }
    }

    common_biases <- c("anchoring", "framing", "confirmation bias")
    if (length(suggested_biases) < 2) {
      for (bias in common_biases) {
        if (!(bias %in% names(suggested_biases))) {
          suggested_biases[[bias]] <- paste(
            "Consider how",
            bias,
            "might affect user interpretation"
          )
        }
      }
    }

    if (length(suggested_biases) == 0) {
      suggested_biases <- list(
        anchoring = "
          Provide multiple reference points to reduce anchoring effect
        ",
        framing = "
          Toggle between positive and negative framing of the same data
        ",
        confirmation_bias = "
          Include evidence that challenges common assumptions
        "
      )
    }

    bias_mitigations <- suggested_biases

    message(
      paste0(
        "Automatically suggested bias mitigations: ",
        paste(names(bias_mitigations), collapse = ", "),
        "."
      )
    )
  }

  if (is.null(interaction_principles)) {
    suggested_principles <- list()

    if (previous_stage$stage[1] == "Structure") {
      layout_interaction_map <- list(
        "dual_process" = c(
          "progressive_disclosure" = "
            Reveal detailed analysis only when users want to explore further
          ",
          "hover_effects" = "
            Show additional context on hover for quick insights
          "
        ),
        "grid" = c(
          "selection_feedback" = "
            Highlight selected grid cells with visual feedback
          ",
          "cross_filtering" = "
            Allow filtering one grid cell to affect related cells
          "
        ),
        "card" = c(
          "card_expansion" = "Allow cards to expand for more details on click",
          "card_hover" = "Use subtle hover effects to indicate interactivity"
        ),
        "tabs" = c(
          "tab_feedback" = "Use clear visual indicators for the active tab",
          "persistent_elements" = "Keep key controls consistent across tabs"
        ),
        "breathable" = c(
          "subtle_animations" = "
            Use subtle animations to guide attention without clutter
          ",
          "contextual_controls" = "
            Show controls only when relevant to reduce visual noise
          "
        )
      )

      if (!is.na(layout) && layout %in% names(layout_interaction_map)) {
        for (principle_name in names(layout_interaction_map[[layout]])) {
          suggested_principles[[principle_name]] <- layout_interaction_map[[
            layout
          ]][[principle_name]]
        }
      }

      if (length(concepts) > 0) {
        concept_interaction_map <- list(
          "Visual Hierarchy" = c(
            "visual_prominence" = paste(
              "Give interactive elements visual prominence proportional",
              "to importance"
            )
          ),
          "Progressive Disclosure" = c(
            "progressive_interaction" = "
              Reveal more complex options only after basic ones are used
            "
          ),
          "Interaction Hints" = c(
            "hover_effects" = "Use hover effects to suggest interactivity",
            "cursor_changes" = "Change cursor to indicate interactive elements"
          ),
          "Visual Feedback" = c(
            "selection_feedback" = "
              Provide immediate visual feedback for user actions
            ",
            "state_indicators" = "
              Use clear visual indicators for different states
            "
          )
        )

        for (concept in concepts) {
          concept <- trimws(concept)
          for (known_concept in names(concept_interaction_map)) {
            if (grepl(tolower(known_concept), tolower(concept))) {
              principles <- concept_interaction_map[[known_concept]]
              for (principle_name in names(principles)) {
                suggested_principles[[principle_name]] <- principles[[
                  principle_name
                ]]
              }
            }
          }
        }
      }
    }

    if (length(suggested_principles) > 0) {
      interaction_principles <- suggested_principles

      message(paste0(
        "Automatically suggested interaction principles: ",
        paste(names(interaction_principles), collapse = ", "),
        "."
      ))
    }
  }

  bias_suggestions <- character(0)
  for (bias_name in names(bias_mitigations)) {
    bias_info <- bid_concept(bias_name)

    if (
      !is.null(bias_info) &&
        is.data.frame(bias_info) &&
        nrow(bias_info) > 0
    ) {
      implementation <- paste(
        bias_name,
        "mitigation:",
        bias_info$implementation_tips
      )
    } else {
      implementation <- paste(
        bias_name,
        "mitigation: Consider how this bias affects user decisions."
      )
    }

    bias_suggestions <- c(bias_suggestions, implementation)
  }

  common_biases <- c("anchoring", "framing", "confirmation")
  missing_biases <- common_biases[
    !common_biases %in% tolower(names(bias_mitigations))
  ]

  if (length(missing_biases) > 0) {
    missing_bias_suggestions <- paste(
      "Consider also addressing these common biases:",
      paste(missing_biases, collapse = ", ")
    )
    bias_suggestions <- c(bias_suggestions, missing_bias_suggestions)
  }

  interaction_suggestions <- character(0)

  if (!is.null(interaction_principles) && length(interaction_principles) > 0) {
    for (principle_name in names(interaction_principles)) {
      interaction_suggestions <- c(
        interaction_suggestions,
        paste0(principle_name, ": ", interaction_principles[[principle_name]])
      )
    }

    interaction_msg <- paste0(
      "Good job including interaction principles. ",
      "These will help users understand how to interact with your dashboard."
    )
  } else {
    interaction_msg <- paste(
      "Consider adding interaction principles like hover effects,",
      "selection feedback, and progressive disclosure to guide users."
    )

    interaction_suggestions <- c(interaction_suggestions, interaction_msg)
  }

  interaction_formatted <- if (
    !is.null(interaction_principles) &&
      length(interaction_principles) > 0
  ) {
    tryCatch(
      {
        jsonlite::toJSON(interaction_principles, auto_unbox = TRUE)
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "Failed to convert interaction_principles to JSON",
            "i" = "Using string representation instead"
          )
        )
        paste(
          names(interaction_principles),
          interaction_principles,
          sep = ": ",
          collapse = "; "
        )
      }
    )
  } else {
    NA_character_
  }

  bias_text <- paste(bias_suggestions, collapse = " ")
  interaction_text <- paste(interaction_suggestions, collapse = " ")
  suggestions <- paste(bias_text, interaction_text, sep = " ")

  result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = paste(
      names(bias_mitigations),
      unlist(bias_mitigations),
      sep = ": ",
      collapse = "; "
    ),
    interaction_principles = interaction_formatted,
    previous_layout = if (!is.na(layout)) layout else NA_character_,
    previous_concepts = if (length(concepts) > 0) {
      paste(concepts, collapse = ", ")
    } else {
      NA_character_
    },
    previous_accessibility = if (!is.na(accessibility)) {
      accessibility %||% NA_character_
    } else {
      NA_character_
    },
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 4 (Anticipate) completed.",
    paste0("Bias mitigations: ", length(names(bias_mitigations)), " defined"),
    if (!is.null(interaction_principles)) {
      paste0(
        "Interaction principles: ",
        length(names(interaction_principles)),
        " defined"
      )
    },
    paste(
      "Key suggestions:",
      paste(
        bias_suggestions[seq_len(min(3, length(bias_suggestions)))],
        collapse = ", "
      )
    )
  )

  return(result)
}
