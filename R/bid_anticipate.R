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
#' @param include_accessibility Logical indicating whether to include
#'        accessibility mitigations. Default is TRUE.
#' @param ... Additional parameters. If 'interaction_principles' is provided,
#'        it will be ignored with a warning.
#'
#' @return A tibble containing the documented information for the "Anticipate"
#'         stage.
#'
#' @examples
#' structure_info <- bid_interpret(
#'   central_question = "How can we improve selection efficiency?",
#'   data_story = list(
#'     hook = "Too many options",
#'     context = "Excessive choices",
#'     tension = "User frustration",
#'     resolution = "Simplify menu"
#'   )
#' ) |>
#'   bid_notice(
#'     "Issue with dropdown menus",
#'     evidence = "User testing indicated delays"
#'   ) |>
#'   bid_structure()
#'
#' # Let the function suggest bias mitigations based on previous stages
#' bid_anticipate(previous_stage = structure_info)
#'
#' # with accessibility included (default) and custom bias mitigations
#' bid_anticipate(
#'   previous_stage = structure_info,
#'   bias_mitigations = list(
#'     anchoring = "Use context-aware references",
#'     framing = "Toggle between positive and negative framing"
#'   ),
#'   include_accessibility = TRUE
#' )
#'
#' @export
bid_anticipate <- function(
    previous_stage,
    bias_mitigations = NULL,
    include_accessibility = TRUE,
    ...) {
  if (missing(previous_stage) || is.null(previous_stage)) {
    stop("Required parameter 'previous_stage' must be provided", call. = FALSE)
  }

  # handle deprecated interaction_principles parameter via ...
  dots <- list(...)
  if ("interaction_principles" %in% names(dots)) {
    cli::cli_warn(c(
      "!" = "The 'interaction_principles' parameter has been deprecated and removed.",
      "i" = "Interaction principles are no longer explicitly tracked in the Anticipate stage.",
      "i" = "This parameter will be ignored in this version."
    ))
    # remove from dots to avoid issues
    dots$interaction_principles <- NULL
  }

  # check for any other unexpected parameters
  if (length(dots) > 0) {
    unexpected_params <- names(dots)
    cli::cli_warn(c(
      "!" = "Unexpected parameters provided: {paste(unexpected_params, collapse = ', ')}",
      "i" = "These will be ignored."
    ))
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

  # validate accessibility parameter
  if (
    !is.logical(include_accessibility) || length(include_accessibility) != 1
  ) {
    cli::cli_warn(c(
      "!" = "include_accessibility must be a single logical value (TRUE/FALSE).",
      "i" = "Using default value TRUE."
    ))
    include_accessibility <- TRUE
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
          provide multiple reference points to reduce anchoring effect
        ",
        framing = "
          toggle between positive and negative framing of the same data
        ",
        confirmation_bias = "
          include evidence that challenges common assumptions
        "
      )
    }

    # add accessibility mitigation if requested and not already present
    if (
      include_accessibility && !"accessibility" %in% names(suggested_biases)
    ) {
      # get layout from previous stage for context-specific accessibility advice
      layout_context <- if (!is.na(layout)) layout else "general"
      accessibility_advice <- get_accessibility_advice(layout_context)
      suggested_biases$accessibility <- accessibility_advice
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

  # add user-provided accessibility mitigations if not auto-suggested
  if (
    include_accessibility &&
      !is.null(bias_mitigations) &&
      !"accessibility" %in% names(bias_mitigations)
  ) {
    layout_context <- if (!is.na(layout)) layout else "general"
    accessibility_advice <- get_accessibility_advice(layout_context)
    bias_mitigations$accessibility <- accessibility_advice

    message("Added accessibility mitigation based on layout context.")
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

  # generate suggestions based on accessibility inclusion
  accessibility_suggestions <- character(0)

  if (include_accessibility) {
    if ("accessibility" %in% names(bias_mitigations)) {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "accessibility considerations have been included in bias mitigations."
      )
    } else {
      accessibility_suggestions <- c(
        accessibility_suggestions,
        "consider adding accessibility mitigations for inclusive design."
      )
    }
  }

  bias_text <- paste(bias_suggestions, collapse = " ")
  accessibility_text <- paste(accessibility_suggestions, collapse = " ")
  suggestions <- paste(bias_text, accessibility_text, sep = " ")

  # normalize previous stage to ensure field name consistency
  normalized_previous <- normalize_previous_stage(previous_stage)

  result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = paste(
      names(bias_mitigations),
      unlist(bias_mitigations),
      sep = ": ",
      collapse = "; "
    ),
    accessibility = if (include_accessibility) {
      if ("accessibility" %in% names(bias_mitigations)) {
        bias_mitigations$accessibility
      } else {
        "accessibility mitigation not specified"
      }
    } else {
      NA_character_
    },
    previous_layout = if (!is.na(layout)) layout else NA_character_,
    previous_concepts = if (length(concepts) > 0) {
      paste(concepts, collapse = ", ")
    } else {
      NA_character_
    },
    previous_central_question = safe_column_access(
      normalized_previous,
      "central_question"
    ),
    previous_hook = safe_column_access(normalized_previous, "hook"),
    previous_problem = safe_column_access(normalized_previous, "problem"),
    previous_theory = safe_column_access(normalized_previous, "theory"),
    previous_audience = get_audience_from_previous(normalized_previous),
    previous_personas = get_personas_from_previous(normalized_previous),
    suggestions = suggestions,
    timestamp = .now()
  )

  bid_message(
    "Stage 4 (Anticipate) completed.",
    paste0("Bias mitigations: ", length(names(bias_mitigations)), " defined"),
    if (include_accessibility) {
      "Accessibility considerations included"
    } else {
      "Accessibility considerations not included"
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
