% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bid_validate.R
\name{bid_validate}
\alias{bid_validate}
\title{Document User Validation Stage in BID Framework}
\usage{
bid_validate(
  previous_stage,
  summary_panel = NULL,
  collaboration = NULL,
  next_steps = NULL
)
}
\arguments{
\item{previous_stage}{A tibble or list output from an earlier BID stage
function.}

\item{summary_panel}{A character string describing the final summary panel
or key insight presentation.}

\item{collaboration}{A character string describing how the dashboard enables
collaboration and sharing.}

\item{next_steps}{A character vector or string describing recommended next
steps for implementation and iteration.}
}
\value{
A tibble containing the documented information for the "Validate"
stage.
}
\description{
This function documents the validation stage, where the user tests and
refines the dashboard. It represents stage 5 in the BID framework.
}
\examples{
structure_input <- bid_notice(
  problem  = "Issue with dropdown menus",
  evidence = "User testing indicated delays"
) |>
  bid_interpret(
    central_question = "How can we improve selection efficiency?",
    data_story = list(
      hook = "Too many options",
      context = "Excessive choices",
      tension = "User frustration",
      resolution = "Simplify menu"
    )
  )

structure_result <- bid_structure(
  previous_stage = structure_input,
  layout         = "dual_process",
  concepts       = c("Principle of Proximity", "Default Effect")
)

anticipate <- bid_anticipate(
  previous_stage = structure_result,
  bias_mitigations = list(
    anchoring = "Provide reference points",
    framing   = "Use gain-framed messaging"
  )
)

bid_validate(
  previous_stage = anticipate,
  summary_panel = "Clear summary of key insights with action items",
  collaboration = "Team annotation and sharing features",
  next_steps = c(
    "Conduct user testing with target audience",
    "Implement accessibility improvements",
    "Add mobile responsiveness"
  )
)

}
