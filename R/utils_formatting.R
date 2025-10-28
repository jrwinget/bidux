#' Function for truncating text in messages
#'
#' @param text Object to check
#' @param max_length Maximum length before truncation
#'
#' @return Truncated text string
#'
#' @keywords internal
#' @noRd
truncate_text <- function(text, max_length) {
  if (is.null(text) || is.na(text)) {
    return("")
  }

  text_str <- as.character(text)
  if (nchar(text_str) <= max_length) {
    return(text_str)
  }

  # truncate to (max_length - 3), then append "..."
  if (max_length <= 3) {
    return(strrep(".", min(3, max_length)))
  }
  truncated <- substr(text_str, 1, max_length - 3)
  paste0(truncated, "...")
}

#' Generic next steps formatting
#'
#' @param next_steps Next steps to format (character vector or single string)
#'
#' @return Formatted next steps string
#'
#' @keywords internal
#' @noRd
format_next_steps <- function(next_steps) {
  if (is.null(next_steps)) {
    return(NA_character_)
  }

  if (is.character(next_steps)) {
    if (length(next_steps) == 1) {
      if (grepl(";", next_steps)) {
        return(next_steps)
      } else {
        return(next_steps)
      }
    } else {
      return(paste(next_steps, collapse = "; "))
    }
  }

  next_steps_char <- as.character(next_steps)
  return(paste(next_steps_char, collapse = "; "))
}

#' Generic next steps parsing
#'
#' @param next_steps_formatted Formatted next steps string
#'
#' @return Character vector of next steps
#'
#' @keywords internal
#' @noRd
parse_next_steps <- function(next_steps_formatted) {
  if (is.na(next_steps_formatted) || is.null(next_steps_formatted)) {
    return(character(0))
  }

  if (grepl(";", next_steps_formatted)) {
    return(trimws(unlist(strsplit(next_steps_formatted, ";"))))
  } else {
    return(next_steps_formatted)
  }
}

#' Generic formatting function for accessibility storage
#'
#' @param accessibility Accessibility data to format
#'
#' @return Formatted accessibility string or NA_character_
#'
#' @keywords internal
#' @noRd
format_accessibility_for_storage <- function(accessibility) {
  if (!is.null(accessibility)) {
    if (is.list(accessibility)) {
      jsonlite::toJSON(accessibility, auto_unbox = TRUE)
    } else {
      as.character(accessibility)
    }
  } else {
    NA_character_
  }
}

#' Format telemetry references for validation steps
#'
#' @param telemetry_refs Telemetry references to format
#'
#' @return Character vector of formatted references
#'
#' @keywords internal
#' @noRd
.format_telemetry_refs_for_validation <- function(telemetry_refs) {
  if (is.null(telemetry_refs) || length(telemetry_refs) == 0) {
    return(character(0))
  }

  # handle different input formats
  if (is.character(telemetry_refs)) {
    # simple character vector
    refs_text <- paste(telemetry_refs, collapse = ", ")
    return(
      paste0(
        "Track specific metrics identified in telemetry analysis: ",
        refs_text
      )
    )
  } else if (is.list(telemetry_refs)) {
    # named list with more structured references
    formatted_refs <- character(0)

    for (ref_name in names(telemetry_refs)) {
      ref_value <- telemetry_refs[[ref_name]]
      if (!is.null(ref_value) && nchar(as.character(ref_value)) > 0) {
        formatted_refs <- c(
          formatted_refs,
          paste0("Monitor ", ref_name, ": ", as.character(ref_value))
        )
      }
    }

    if (length(formatted_refs) > 0) {
      return(c(
        "Validate telemetry-identified issues with specific tracking:",
        formatted_refs
      ))
    }
  }

  return(character(0))
}
