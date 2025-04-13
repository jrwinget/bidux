#' Document User Behavior Anticipation Stage in BID Framework
#'
#' @description
#' This function documents the anticipated user behavior by listing bias
#' mitigation strategies related to anchoring, framing, confirmation bias, etc.
#' It also supports adding interaction hints and visual feedback elements.
#'
#' @param previous_stage A tibble or list output from \code{bid_structure()}.
#' @param bias_mitigations A named list of bias mitigation strategies.
#' @param interaction_principles A named list of interaction principles
#'        (optional).
#'
#' @return A tibble containing the documented information for the "Anticipate"
#'         stage.
#'
#' @examples
#' structure_info <- bid_notice(
#'   problem = "Issue with dropdown menus",
#'   evidence = "User testing indicated delays"
#' ) |>
#'   bid_interpret(
#'     central_question = "How can we improve selection efficiency?",
#'     data_story = list(
#'       hook = "Too many options",
#'       context = "Excessive choices",
#'       tension = "User frustration",
#'       resolution = "Simplify menu"
#'     )
#'   ) |>
#'   bid_structure(
#'     layout = "dual_process",
#'     concepts = c("principle_of_proximity", "default_effect")
#'   )
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
#'     progressive_actions = "Reveal advanced options only when basic ones are used"
#'   )
#' )
#'
#' @export
bid_anticipate <- function(
    previous_stage,
    bias_mitigations,
    interaction_principles = NULL) {
  validate_required_params(
    previous_stage = previous_stage, 
    bias_mitigations = bias_mitigations
  )
  
  validate_previous_stage(previous_stage, "Structure")

  bias_concepts <- bid_concepts("bias|anchor|fram|confirm")

  # generate implementation suggestions
  bias_suggestions <- character(0)
  for (bias_name in names(bias_mitigations)) {
    bias_info <- bid_concept(bias_name)

    if (!is.null(bias_info) && nrow(bias_info) > 0) {
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

  # check common, missing biases
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

  # generate interaction principles suggestions
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
  
  # format interaction principles
  interaction_formatted <- if (
    !is.null(interaction_principles) && length(interaction_principles) > 0
  ) {
    if (is.list(interaction_principles)) {
      jsonlite::toJSON(interaction_principles)
    } else {
      as.character(interaction_principles)
    }
  } else {
    NA_character_
  }

  # final suggestions
  bias_text <- paste(bias_suggestions, collapse = " ")
  interaction_text <- paste(interaction_suggestions, collapse = " ")
  suggestions <- paste(bias_text, interaction_text, sep = " ")

  result <- tibble::tibble(
    stage = "Anticipate",
    bias_mitigations = paste(
      names(bias_mitigations),
      bias_mitigations,
      sep = ": ",
      collapse = "; "
    ),
    interaction_principles = interaction_formatted,
    previous_layout = previous_stage$layout[1],
    previous_concepts = previous_stage$concepts[1],
    previous_accessibility = previous_stage$accessibility[1] %||% NA_character_,
    suggestions = suggestions,
    timestamp = Sys.time()
  )

  bid_message(
    "Stage 4 (Anticipate) completed.",
    paste0(
      "Bias mitigations: ",
      length(names(bias_mitigations)),
      " defined"
    ),
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
        bias_suggestions[1:min(3, length(bias_suggestions))],
        collapse = ", "
      )
    )
  )

  return(result)
}
