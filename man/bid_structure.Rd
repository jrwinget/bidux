% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bid_structure.R
\name{bid_structure}
\alias{bid_structure}
\title{Document Dashboard Structure Stage in BID Framework}
\usage{
bid_structure(previous_stage, layout, concepts)
}
\arguments{
\item{previous_stage}{A tibble or list output from \code{bid_interpret()}.}

\item{layout}{A character string indicating the layout type (e.g.,
"dual_process", "grid").}

\item{concepts}{A character vector of BID concepts applied in this stage.}
}
\value{
A tibble containing the documented information for the "Structure"
stage.
}
\description{
This function documents the structure of the dashboard, including layout and
design elements such as proximity, dual-processing, and default effects.
}
\examples{
interpret <- bid_interpret(
  bid_notice(
    problem = "Users struggle with information overload",
    evidence = "Survey results indicate delays"
  ),
  central_question = "How can we simplify data presentation?",
  data_story = list(
    hook = "Data is too complex",
    context = "Overloaded with charts",
    tension = "Confusing layout",
    resolution = "Introduce clear grouping"
  )
)
bid_structure(
  previous_stage = interpret,
  layout = "dual_process",
  concepts = c("principle_of_proximity", "default_effect")
)
}
