#' List all BID Framework Concepts
#'
#' @description
#' This function returns a tibble listing all the key concepts in the Behavior Insight Design framework,
#' including their descriptions, categories, references, and example applications.
#'
#' @return A tibble with concept details.
#' @examples
#' bid_concepts()
#' @export
bid_concepts <- function() {
  concepts <- tibble::tibble(
    concept = c(
      "Cognitive Load Theory", "Hick's Law", "Visual Hierarchies",
      "Data Storytelling Framework", "Processing Fluency", "Emotion & Fluency Effects",
      "Principle of Proximity", "Dual-Processing Theory", "Default Effect",
      "Aesthetic-Usability", "Beautiful-Is-Good Stereotype",
      "Anchoring Effect", "Framing & Loss Aversion", "Confirmation Bias",
      "Belief Perseverance", "Risk Perception",
      "Peak-End Rule", "Cooperation & Coordination"
    ),
    description = c(
      "Theory that suggests minimizing extraneous load to improve user understanding.",
      "Principle stating that increasing number of choices increases decision time.",
      "Design principle that uses layout and whitespace to guide attention.",
      "Framework for crafting a narrative with data to emphasize insights.",
      "The ease with which information is processed, affecting comprehension and trust.",
      "How emotional resonance influences interpretation of data.",
      "Grouping related elements to create a coherent visual structure.",
      "Model distinguishing between fast (System 1) and slow (System 2) thinking.",
      "How default selections can nudge user behavior.",
      "The effect where aesthetically pleasing designs are perceived as more usable.",
      "The assumption that attractive things are inherently good.",
      "Bias where initial values influence subsequent judgments.",
      "Bias in decision-making influenced by how choices are framed.",
      "Tendency to favor information that confirms pre-existing beliefs.",
      "Clinging to initial beliefs even when faced with contradictory evidence.",
      "Perception of risk that is often influenced by emotional reactions.",
      "Rule stating that people judge experiences by their peak and end moments.",
      "Collaborative processes that enhance group decision-making."
    ),
    category = c(
      "Stage 1", "Stage 1", "Stage 1",
      "Stage 2", "Stage 2", "Stage 2",
      "Stage 3", "Stage 3", "Stage 3",
      "Stage 3", "Stage 3",
      "Stage 4", "Stage 4", "Stage 4",
      "Stage 4", "Stage 4",
      "Stage 5", "Stage 5"
    ),
    reference = c(
      "Sweller (1988)", "Hick (1952)", "Tufte (1983)",
      "Matei & Hunter (2021)", "Alter & Oppenheimer (2009)", "Song & Schwarz (2009)",
      "Gestalt Principles", "Tversky & Kahneman (1979)", "Johnson & Goldstein (2003)",
      "Norman (2002)", "Dion et al. (1972)",
      "Tversky & Kahneman (1974)", "Tversky & Kahneman (1981)", "Nickerson (1998)",
      "Ross & Lepper (1980)", "Slovic (1987)",
      "Kahneman et al. (1993)", "Tindale & Kluwe (2015)"
    ),
    example = c(
      "Reducing clutter in dashboards",
      "Simplifying dropdown menus",
      "Emphasizing key metrics through layout",
      "Highlighting a data narrative",
      "Using simple visuals for clarity",
      "Applying subtle color cues for emotional impact",
      "Grouping related controls",
      "Providing quick overviews and detailed views",
      "Pre-selecting best practice options",
      "Creating visually appealing interfaces",
      "Designing attractive summary cards",
      "Using baseline comparisons",
      "Presenting data with gain-framed messaging",
      "Including alternative scenarios",
      "Showing counter-evidence",
      "Visualizing risk probabilities",
      "Ending with a compelling summary",
      "Enabling team annotations"
    )
  )
  
  concepts
}

#' Get Detailed Information for a BID Concept
#'
#' @description
#' This function retrieves detailed information about a specific concept within the BID framework.
#'
#' @param concept_name A character string specifying the name of the concept.
#' @return A tibble with details for the specified concept.
#' @examples
#' bid_concept("Cognitive Load Theory")
#' @export
bid_concept <- function(concept_name) {
  all_concepts <- bid_concepts()
  result <- dplyr::filter(all_concepts, concept == concept_name)
  if (nrow(result) == 0) {
    warning("Concept not found. Returning NULL.")
    return(NULL)
  }
  result
}
