% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bid_structure.R
\name{bid_structure}
\alias{bid_structure}
\title{Document Dashboard Structure Stage in BID Framework}
\usage{
bid_structure(previous_stage, layout, concepts = NULL, accessibility = NULL)
}
\arguments{
\item{previous_stage}{A tibble or list output from an earlier BID stage
function.}

\item{layout}{A character string indicating the layout type (e.g.,
"dual_process", "grid", "card", "tabs", "breathable").}

\item{concepts}{A character vector of BID concepts applied in this stage.
Concepts can be provided in natural language (e.g., "Principle of
Proximity") or with underscores (e.g., "principle_of_proximity"). The
function uses fuzzy matching to identify the concepts. If NULL, will
attempt to detect relevant concepts from previous stages.}

\item{accessibility}{A list of accessibility considerations (optional).
Common parameters include: color_contrast, keyboard_navigation,
screen_reader, text_size, alternative_text, focus_indicators,
semantic_markup, and aria_labels.}
}
\value{
A bid_stage object containing the documented information for the
"Structure" stage.
}
\description{
This function documents the structure of the dashboard, including layout and
design elements such as proximity, dual-processing, and default effects.
It supports modern layout approaches like breathable layouts and visual
hierarchies, while ensuring accessibility considerations are properly
documented.
}
\examples{
interpret <- bid_notice(
  problem = "Users struggle with information overload",
  evidence = "Survey results indicate delays"
) |>
  bid_interpret(
    central_question = "How can we simplify data presentation?",
    data_story = list(
      hook = "Data is too complex",
      context = "Overloaded with charts",
      tension = "Confusing layout",
      resolution = "Introduce clear grouping"
    )
  )

# Basic usage with natural language concept names
bid_structure(
  previous_stage = interpret,
  layout = "dual_process",
  concepts = c("Principle of Proximity", "Default Effect")
)

}
