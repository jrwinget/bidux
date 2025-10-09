#' Search BID Framework Concepts
#'
#' @description
#' Search for behavioral science and UX concepts used in the BID framework.
#' Returns concepts matching the search term along with their descriptions,
#' categories, and implementation guidance.
#'
#' @param search A character string to search for. If NULL or empty, returns all
#'        concepts.
#' @param fuzzy_match Logical indicating whether to use fuzzy string matching
#'        (default: TRUE)
#' @param max_distance Maximum string distance for fuzzy matching (default: 2)
#'
#' @return A tibble containing matching concepts with their details
#' @export
bid_concepts <- function(search = NULL, fuzzy_match = TRUE, max_distance = 2) {
  concepts_data <- get_concepts_data()

  if (is.null(search) || nchar(trimws(search)) == 0) {
    message("Returning all ", nrow(concepts_data), " concepts")
    return(concepts_data)
  }

  # direct search in concept names and descriptions
  search_terms <- tolower(trimws(unlist(strsplit(search, ","))))

  matches <- c()
  for (term in search_terms) {
    term <- trimws(term)
    if (nchar(term) > 0) {
      # exact matches in concept names or descriptions
      exact_matches <- which(
        grepl(term, tolower(concepts_data$concept)) |
          grepl(term, tolower(concepts_data$description))
      )
      matches <- c(matches, exact_matches)

      # fuzzy matching if enabled and no exact matches
      if (fuzzy_match && length(exact_matches) == 0) {
        distances <- stringdist::stringdistmatrix(
          term,
          tolower(concepts_data$concept),
          method = "jw"
        )
        fuzzy_matches <- which(distances <= max_distance/10)
        matches <- c(matches, fuzzy_matches)
      }
    }
  }

  matches <- unique(matches)

  if (length(matches) == 0) {
    message("No concepts found matching '", search, "'")
    return(concepts_data[0, ])
  }

  result <- concepts_data[matches, ]
  message("Found ", nrow(result), " concept(s) matching '", search, "'")
  return(result)
}

#' Get detailed information about a specific concept
#'
#' @description
#' Returns detailed information about a specific BID framework concept,
#' including implementation recommendations based on the concept's stage.
#'
#' @param concept_name A character string with the exact or partial concept name
#' @param add_recommendations Logical indicating whether to add stage-specific
#'        recommendations
#'
#' @return A tibble with detailed concept information
#' @export
bid_concept <- function(concept_name, add_recommendations = TRUE) {
  if (is.null(concept_name) || nchar(trimws(concept_name)) == 0) {
    message("Please provide a concept name")
    return(get_concepts_data()[0, ])
  }

  concepts_data <- get_concepts_data()
  concept_clean <- trimws(concept_name)

  # try exact match first
  exact_match <- which(tolower(concepts_data$concept) == tolower(concept_clean))

  if (length(exact_match) > 0) {
    result <- concepts_data[exact_match[1], ]
  } else {
    # try partial match
    partial_matches <- which(grepl(
      tolower(concept_clean),
      tolower(concepts_data$concept)
    ))

    if (length(partial_matches) > 0) {
      result <- concepts_data[partial_matches[1], ]
      message("Found partial match: ", result$concept[1])
    } else {
      message("Concept '", concept_name, "' not found")
      return(concepts_data[0, ])
    }
  }

  # add recommendations based on stage
  if (add_recommendations) {
    stage_recs <- switch(result$category[1],
      "Stage 1" = "NOTICE: Use this concept when identifying user problems and cognitive barriers",
      "Stage 2" = "INTERPRET: Apply this concept when developing data stories and central questions",
      "Stage 3" = "STRUCTURE: Implement this concept in dashboard layout and information organization",
      "Stage 4" = "ANTICIPATE: Consider this concept when designing interactions and mitigating biases",
      "Stage 5" = "VALIDATE: Use this concept in user empowerment and collaboration features",
      "All Stages" = "UNIVERSAL: This concept applies throughout the entire BID framework",
      "General guidance for concept application"
    )

    result$recommendations <- stage_recs
  }

  return(result)
}

#' Internal function to get concepts data from external files
#' @return A tibble with all BID framework concepts
#' @keywords internal
get_concepts_data <- function() {
  # reuse the unified loading pattern
  required_cols <- c(
    "concept",
    "description",
    "category",
    "reference",
    "example",
    "implementation_tips",
    "related_concepts"
  )

  # load external data and ensure it's returned as tibble
  data <- load_external_data(
    "bid_concepts_data.csv",
    required_cols,
    get_default_concepts_data,
    NULL
  )

  return(tibble::as_tibble(data))
}

#' Get default concepts data with lazy loading
#'
#' @description
#' Internal function to retrieve default BID framework concepts data using
#' lazy loading from external CSV file. This includes behavioral science
#' concepts, cognitive principles, and implementation guidance for each
#' stage of the BID framework.
#'
#' @return A tibble with default BID framework concepts
#' @keywords internal
get_default_concepts_data <- function() {
  # use lazy loading with fallback
  tryCatch(
    {
      load_concepts_data()
    },
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Failed to load external concepts data",
        "i" = "Using fallback concepts data",
        "x" = e$message
      ))
      get_fallback_concepts_data()
    }
  )
}

#' Validate loaded concepts data structure
#'
#' @param concepts_data Raw concepts data from CSV
#' @return Invisible NULL if valid, throws error otherwise
#' @keywords internal
validate_concepts_data_structure <- function(concepts_data) {
  required_columns <- c(
    "concept", "description", "category", "reference",
    "example", "implementation_tips", "related_concepts"
  )

  validate_data_frame(concepts_data, "concepts_data",
    required_columns = required_columns
  )

  if (nrow(concepts_data) == 0) {
    cli::cli_abort(standard_error_msg(
      "Concepts data file is empty",
      suggestions = "Ensure the CSV file contains concept data"
    ))
  }

  invisible(NULL)
}

#' Load concepts data from external file with caching
#'
#' @description
#' Loads concepts data from CSV file with caching for performance.
#' Uses memoise to ensure data is only loaded once per session.
#'
#' @return A tibble with concepts data
#' @keywords internal
load_concepts_data <- memoise::memoise(function() {
  concepts_file <- system.file("extdata", "bid_concepts_data.csv", package = "bidux")

  if (!file.exists(concepts_file)) {
    cli::cli_abort(standard_error_msg(
      "Concepts data file not found",
      context = glue::glue("Expected file: {concepts_file}"),
      suggestions = "Ensure package is properly installed"
    ))
  }

  concepts_data <- readr::read_csv(concepts_file, show_col_types = FALSE)
  validate_concepts_data_structure(concepts_data)
  concepts_data
})

#' Fallback concepts data for emergency use
#'
#' @description
#' Provides minimal fallback data if external file cannot be loaded.
#' Only used as last resort to prevent package failure.
#'
#' @return A tibble with minimal concepts data
#' @keywords internal
get_fallback_concepts_data <- function() {
  # minimal fallback for emergencies - only essential concepts
  tibble::tibble(
    concept = c(
      "Cognitive Load Theory",
      "Visual Hierarchies",
      "Progressive Disclosure",
      "User-Centric Design"
    ),
    description = c(
      "Theory that suggests minimizing extraneous load to improve user understanding.",
      "Design principle that uses layout and whitespace to guide attention.",
      "Technique of presenting complex information layer by layer.",
      "Design approach that prioritizes user needs and contexts throughout development."
    ),
    category = c("Stage 1", "Stage 1", "All Stages", "All Stages"),
    reference = c("Sweller (1988)", "Tufte (1983)", "Shneiderman (1996)", "Norman (2013)"),
    example = c(
      "Reducing clutter in dashboards",
      "Emphasizing key metrics through layout",
      "Using progressive disclosure in complex dashboards",
      "Conducting regular usability testing"
    ),
    implementation_tips = c(
      "Use tabs or collapsible sections to organize complex information.",
      "Use size, color, and position to indicate importance of elements.",
      "Use shiny::actionButton with shinyjs to reveal additional content.",
      "Conduct user interviews and usability testing throughout the development process."
    ),
    related_concepts = c(
      "Progressive Disclosure, Miller's Law, Visual Hierarchies",
      "Information Hierarchy, Pre-attentive Processing, Visual Hierarchy",
      "Cognitive Load Theory, Miller's Law, Visual Hierarchies",
      "User Personas, Gherkin Method, Data Storytelling Framework"
    )
  )
}
