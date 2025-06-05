#' Create a BID stage result object (internal constructor)
#' @param stage Character string indicating the stage name
#' @param data Tibble containing the stage data
#' @param metadata List containing additional metadata
#' @return Object of class 'bid_stage'
#' @keywords internal
new_bid_stage <- function(stage, data, metadata = list()) {
  if (!tibble::is_tibble(data)) {
    stop("data must be a tibble", call. = FALSE)
  }

  structure(
    data,
    class = c("bid_stage", class(data)),
    stage = stage,
    metadata = metadata,
    created = Sys.time()
  )
}

#' Validate BID stage object
#' @param x Object to validate
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_bid_stage <- function(x) {
  stage <- attr(x, "stage")
  if (is.null(stage) || !is.character(stage) || length(stage) != 1) {
    stop("BID stage object must have a single 'stage' attribute", call. = FALSE)
  }

  valid_stages <- c(
    "Notice",
    "Interpret",
    "Structure",
    "Anticipate",
    "Validate"
  )
  if (!stage %in% valid_stages) {
    stop(
      "Stage must be one of: ",
      paste(valid_stages, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"stage" %in% names(x)) {
    stop("BID stage object must contain a 'stage' column", call. = FALSE)
  }

  if (x$stage[1] != stage) {
    stop("Stage attribute must match stage column value", call. = FALSE)
  }

  if (!"timestamp" %in% names(x)) {
    stop("BID stage object must contain a 'timestamp' column", call. = FALSE)
  }

  TRUE
}

#' Constructor for BID stage objects
#' @param stage Character string indicating the stage name
#' @param data Tibble containing the stage data
#' @param metadata List containing additional metadata
#' @return Object of class 'bid_stage'
#' @export
bid_stage <- function(stage, data, metadata = list()) {
  x <- new_bid_stage(stage, data, metadata)
  validate_bid_stage(x)
  x
}

#' Check if object is a bid_stage
#' @param x Object to test
#' @return Logical indicating if object is bid_stage
#' @export
is_bid_stage <- function(x) {
  inherits(x, "bid_stage")
}

#' Get stage name from bid_stage object
#' @param x A bid_stage object
#' @return Character string with stage name
#' @export
get_stage <- function(x) {
  if (!is_bid_stage(x)) {
    stop("Object is not a bid_stage", call. = FALSE)
  }
  attr(x, "stage")
}

#' Get metadata from bid_stage object
#' @param x A bid_stage object
#' @return List with metadata
#' @export
get_metadata <- function(x) {
  if (!is_bid_stage(x)) {
    stop("Object is not a bid_stage", call. = FALSE)
  }
  attr(x, "metadata")
}

#' Print method for BID stage objects
#' @param x A bid_stage object
#' @param ... Additional arguments (unused)
#' @export
print.bid_stage <- function(x, ...) {
  stage <- attr(x, "stage")
  created <- attr(x, "created")
  metadata <- attr(x, "metadata")

  cat(
    cli::style_bold(cli::col_blue("BID Framework")),
    "-",
    cli::style_bold(stage),
    "Stage\n"
  )
  cat("Generated:", format(created, "%Y-%m-%d %H:%M:%S"), "\n")

  # Add metadata info if available
  if (!is.null(metadata) && length(metadata) > 0) {
    if (!is.null(metadata$stage_number) && !is.null(metadata$total_stages)) {
      progress <- round((metadata$stage_number / metadata$total_stages) * 100)
      cat(
        "Progress:",
        progress,
        "%",
        paste0("(", metadata$stage_number, "/", metadata$total_stages, ")"),
        "\n"
      )
    }
  }

  cat("\n")

  # Print key information based on stage
  if (stage == "Notice") {
    cat(cli::style_bold("Problem:"), x$problem[1], "\n")
    if (!is.na(x$theory[1])) {
      theory_text <- x$theory[1]
      if (
        !is.null(metadata$auto_suggested_theory) &&
          metadata$auto_suggested_theory
      ) {
        theory_text <- paste0(
          theory_text,
          " ",
          cli::style_italic("(auto-suggested)")
        )
      }
      cat(cli::style_bold("Theory:"), theory_text, "\n")
    }
    if (!is.na(x$evidence[1])) {
      cat(cli::style_bold("Evidence:"), x$evidence[1], "\n")
    }
    if (!is.na(x$target_audience[1])) {
      cat(cli::style_bold("Target Audience:"), x$target_audience[1], "\n")
    }
  } else if (stage == "Interpret") {
    cat(cli::style_bold("Central Question:"), x$central_question[1], "\n")
    if (!is.na(x$hook[1])) {
      cat(cli::style_bold("Story Hook:"), x$hook[1], "\n")
    }
    if (!is.null(metadata$story_completeness)) {
      completeness <- round(metadata$story_completeness * 100)
      cat(
        cli::style_bold("Story Completeness:"),
        paste0(completeness, "%"),
        "\n"
      )
    }
    if (!is.null(metadata$personas_count) && metadata$personas_count > 0) {
      cat(
        cli::style_bold("User Personas:"),
        metadata$personas_count,
        "defined\n"
      )
    }
  } else if (stage == "Structure") {
    cat(cli::style_bold("Layout:"), x$layout[1], "\n")
    if (!is.na(x$concepts[1])) {
      concepts_list <- strsplit(x$concepts[1], ",")[[1]]
      cat(
        cli::style_bold("Concepts:"),
        paste(trimws(concepts_list), collapse = ", "),
        "\n"
      )
    }
    if (!is.na(x$accessibility[1]) && x$accessibility[1] != "NA") {
      cat(cli::style_bold("Accessibility:"), "Guidelines defined\n")
    }
  } else if (stage == "Anticipate") {
    if (!is.na(x$bias_mitigations[1])) {
      bias_items <- strsplit(x$bias_mitigations[1], ";")[[1]]
      bias_count <- length(bias_items)
      cat(
        cli::style_bold("Bias Mitigations:"),
        bias_count,
        "strategies defined\n"
      )
    }
    if (
      !is.na(x$interaction_principles[1]) && x$interaction_principles[1] != "NA"
    ) {
      cat(cli::style_bold("Interaction Principles:"), "Defined\n")
    }
  } else if (stage == "Validate") {
    if (!is.na(x$summary_panel[1])) {
      cat(
        cli::style_bold("Summary Panel:"),
        truncate_text(x$summary_panel[1], 60),
        "\n"
      )
    }
    if (!is.na(x$next_steps[1])) {
      steps_list <- strsplit(x$next_steps[1], ";")[[1]]
      steps_count <- length(steps_list)
      cat(cli::style_bold("Next Steps:"), steps_count, "items defined\n")
    }
    if (!is.na(x$collaboration[1])) {
      cat(
        cli::style_bold("Collaboration:"),
        truncate_text(x$collaboration[1], 60),
        "\n"
      )
    }
  }

  if (!is.na(x$suggestions[1])) {
    cat("\n", cli::style_italic("Suggestions:"), x$suggestions[1], "\n")
  }

  cat("\n", cli::style_dim("Use summary() for detailed information"), "\n")
  invisible(x)
}

#' Summary method for BID stage objects
#' @param object A bid_stage object
#' @param ... Additional arguments (unused)
#' @export
summary.bid_stage <- function(object, ...) {
  stage <- attr(object, "stage")
  metadata <- attr(object, "metadata")
  created <- attr(object, "created")

  cat(
    cli::style_bold(cli::col_blue("=== BID Framework:")),
    cli::style_bold(stage),
    cli::style_bold("Stage Summary ===\n\n")
  )

  # Print metadata summary
  if (!is.null(metadata) && length(metadata) > 0) {
    cat(cli::style_bold("Metadata:\n"))
    for (name in names(metadata)) {
      value <- metadata[[name]]
      if (is.logical(value)) {
        value <- if (value) "Yes" else "No"
      } else if (is.numeric(value) && value < 1) {
        value <- paste0(round(value * 100), "%")
      }
      cat("  ", name, ":", value, "\n")
    }
    cat("\n")
  }

  # Print all data columns
  cat(cli::style_bold("Stage Data:\n"))
  for (col_name in names(object)) {
    if (col_name != "timestamp") {
      # Skip timestamp as it's shown in header
      value <- object[[col_name]][1]
      if (!is.na(value) && nchar(as.character(value)) > 0) {
        display_value <- if (nchar(as.character(value)) > 80) {
          truncate_text(as.character(value), 80)
        } else {
          as.character(value)
        }
        cat("  ", col_name, ":", display_value, "\n")
      }
    }
  }

  cat("\nGenerated:", format(created, "%Y-%m-%d %H:%M:%S"), "\n")
  invisible(object)
}

#' Convert bid_stage to tibble
#' @param x A bid_stage object
#' @param ... Additional arguments (unused)
#' @return A tibble
#' @export
as_tibble.bid_stage <- function(x, ...) {
  # Remove bid_stage class and return as regular tibble
  class(x) <- setdiff(class(x), "bid_stage")
  x
}

# ===== BID Result Collection Class =====

#' Create a BID result collection object (internal constructor)
#' @param stages List of bid_stage objects
#' @return Object of class 'bid_result'
#' @keywords internal
new_bid_result <- function(stages) {
  if (!is.list(stages)) {
    stop("stages must be a list", call. = FALSE)
  }

  structure(
    stages,
    class = c("bid_result", "list"),
    created = Sys.time()
  )
}

#' Validate BID result object
#' @param x Object to validate
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_bid_result <- function(x) {
  if (!is.list(x)) {
    stop("BID result object must be a list", call. = FALSE)
  }

  for (i in seq_along(x)) {
    if (!is_bid_stage(x[[i]])) {
      stop(
        "All elements in BID result must be bid_stage objects",
        call. = FALSE
      )
    }
  }

  TRUE
}

#' Constructor for BID result collection objects
#' @param stages List of bid_stage objects
#' @return Object of class 'bid_result'
#' @export
bid_result <- function(stages) {
  x <- new_bid_result(stages)
  validate_bid_result(x)
  x
}

#' Extract specific stage from bid_result
#' @param workflow A bid_result object
#' @param stage Character string with stage name
#' @return A bid_stage object or NULL if not found
#' @export
extract_stage <- function(workflow, stage) {
  if (!inherits(workflow, "bid_result")) {
    stop("workflow must be a bid_result object", call. = FALSE)
  }

  for (stage_obj in workflow) {
    if (get_stage(stage_obj) == stage) {
      return(stage_obj)
    }
  }

  NULL
}

#' Check if workflow is complete (has all 5 stages)
#' @param x A bid_result object
#' @return Logical indicating if workflow is complete
#' @export
is_complete <- function(x) {
  if (!inherits(x, "bid_result")) {
    return(FALSE)
  }

  required_stages <- c(
    "Notice",
    "Interpret",
    "Structure",
    "Anticipate",
    "Validate"
  )
  present_stages <- sapply(x, get_stage)

  all(required_stages %in% present_stages)
}

#' Print method for BID result objects
#' @param x A bid_result object
#' @param ... Additional arguments (unused)
#' @export
print.bid_result <- function(x, ...) {
  created <- attr(x, "created")

  cat(cli::style_bold(cli::col_blue("BID Framework Workflow")), "\n")
  cat("Created:", format(created, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Stages completed:", length(x), "of 5\n")

  if (is_complete(x)) {
    cat(cli::col_green("Workflow Complete\n"))
  } else {
    completion_pct <- round((length(x) / 5) * 100)
    cat("Progress: ", completion_pct, "%\n", sep = "")
  }

  cat("\n", cli::style_bold("Stages:\n"))

  for (i in seq_along(x)) {
    stage_obj <- x[[i]]
    stage_name <- get_stage(stage_obj)
    stage_time <- stage_obj$timestamp[1]

    cat(
      "  ",
      i,
      ". ",
      cli::style_bold(stage_name),
      " (",
      format(stage_time, "%H:%M:%S"),
      ")\n",
      sep = ""
    )

    # Show key info for each stage
    if (stage_name == "Notice") {
      cat("     Problem:", truncate_text(stage_obj$problem[1], 50), "\n")
    } else if (stage_name == "Interpret") {
      cat(
        "     Question:",
        truncate_text(stage_obj$central_question[1], 50),
        "\n"
      )
    } else if (stage_name == "Structure") {
      cat("     Layout:", stage_obj$layout[1], "\n")
    } else if (stage_name == "Anticipate") {
      bias_count <- length(strsplit(stage_obj$bias_mitigations[1], ";")[[1]])
      cat("     Biases addressed:", bias_count, "\n")
    } else if (stage_name == "Validate") {
      cat("     Summary:", truncate_text(stage_obj$summary_panel[1], 50), "\n")
    }
  }

  cat("\n", cli::style_dim("Use summary() for detailed information"), "\n")
  invisible(x)
}

#' Summary method for BID result objects
#' @param object A bid_result object
#' @param ... Additional arguments (unused)
#' @export
summary.bid_result <- function(object, ...) {
  cat(cli::style_bold(cli::col_blue(
    "=== BID Framework Workflow Summary ===\n\n"
  )))

  cat("Total stages:", length(object), "\n")
  cat("Complete workflow:", if (is_complete(object)) "Yes" else "No", "\n")

  if (length(object) > 0) {
    first_stage <- object[[1]]$timestamp[1]
    last_stage <- object[[length(object)]]$timestamp[1]
    duration <- as.numeric(difftime(last_stage, first_stage, units = "mins"))
    cat("Duration:", round(duration, 1), "minutes\n")
  }

  cat("\n", cli::style_bold("Stage Details:\n"))

  for (i in seq_along(object)) {
    stage_obj <- object[[i]]
    cat("\n", cli::style_bold(paste0(i, ". ", get_stage(stage_obj))), "\n")

    # Print key details without full summary
    important_cols <- switch(
      get_stage(stage_obj),
      "Notice" = c("problem", "theory", "evidence"),
      "Interpret" = c("central_question", "hook"),
      "Structure" = c("layout", "concepts"),
      "Anticipate" = c("bias_mitigations", "interaction_principles"),
      "Validate" = c("summary_panel", "next_steps")
    )

    for (col in important_cols) {
      if (col %in% names(stage_obj) && !is.na(stage_obj[[col]][1])) {
        value <- truncate_text(stage_obj[[col]][1], 60)
        cat("   ", col, ":", value, "\n")
      }
    }
  }

  invisible(object)
}
