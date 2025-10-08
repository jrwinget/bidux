# ==============================================================================
# MESSAGING UTILITIES
# ==============================================================================
#
# Functions for user-facing messages, quiet mode, and output formatting.
#

#' Create standardized message output
#'
#' @param title The title or heading for the message
#' @param ... Character strings to include as bullet points
#' @param quiet Logical indicating whether to suppress message output.
#'        If NULL, uses getOption("bidux.quiet", FALSE)
#'
#' @return NULL invisibly, used for side effect of printing message
#'
#' @keywords internal
#' @noRd
bid_message <- function(title, ..., quiet = NULL) {
  # check quiet mode: function parameter > global option > default
  is_quiet <- quiet %||% getOption("bidux.quiet", FALSE)

  # if quiet mode is enabled, return without outputting anything
  if (is_quiet) {
    return(invisible(NULL))
  }

  # if title is NULL or empty, do nothing
  if (is.null(title) || (is.character(title) && nchar(trimws(title)) == 0)) {
    return(invisible(NULL))
  }

  # extract bullet points from ... but exclude the quiet parameter
  dots <- list(...)
  if ("quiet" %in% names(dots)) {
    dots <- dots[names(dots) != "quiet"]
  }
  bullet_points <- unlist(dots, use.names = FALSE)

  # filter out bullet points that are NULL, NA, or zero‐length
  valid_bullets <- bullet_points[
    !vapply(bullet_points, is.null, logical(1)) &
      !vapply(bullet_points, function(x) all(is.na(x)), logical(1)) &
      (nchar(trimws(as.character(bullet_points))) > 0)
  ]

  if (length(valid_bullets) == 0) {
    return(invisible(NULL))
  }

  msg <- paste0(
    title,
    "\n",
    paste0("  - ", valid_bullets, collapse = "\n")
  )
  cat(msg, "\n")
  invisible(NULL)
}

#' Set global quiet mode for bidux functions
#'
#' @description
#' Convenience function to set the global quiet option for all bidux functions.
#' When quiet mode is enabled, most informational messages are suppressed.
#'
#' @param quiet Logical indicating whether to enable quiet mode.
#'        When TRUE, most bidux messages are suppressed.
#'
#' @return The previous value of the quiet option (invisibly)
#'
#' @examples
#' # Enable quiet mode
#' bid_set_quiet(TRUE)
#'
#' # Disable quiet mode
#' bid_set_quiet(FALSE)
#'
#' @export
bid_set_quiet <- function(quiet = TRUE) {
  old_value <- getOption("bidux.quiet", FALSE)
  options(bidux.quiet = quiet)
  invisible(old_value)
}

#' Get current quiet mode setting
#'
#' @description
#' Check whether bidux is currently in quiet mode.
#'
#' @return Logical indicating whether quiet mode is enabled
#'
#' @examples
#' # Check current quiet setting
#' bid_get_quiet()
#'
#' @export
bid_get_quiet <- function() {
  getOption("bidux.quiet", FALSE)
}

#' Temporarily suppress bidux messages
#'
#' @description
#' Execute code with bidux messages temporarily suppressed.
#'
#' @param code Code to execute with messages suppressed
#'
#' @return The result of evaluating code
#'
#' @examples
#' # Run analysis quietly without changing global setting
#' result <- bid_with_quiet({
#'   bid_interpret(
#'     central_question = "How can we improve user engagement?",
#'     data_story = list(hook = "Users are leaving", resolution = "Fix issues")
#'   )
#' })
#'
#' @export
bid_with_quiet <- function(code) {
  old_quiet <- getOption("bidux.quiet", FALSE)
  on.exit(options(bidux.quiet = old_quiet))
  options(bidux.quiet = TRUE)
  force(code)
}

#' Normalize text formatting for consistent output
#'
#' @param text Character string to normalize
#' @param capitalize_first Logical indicating whether to capitalize first letter
#' @param remove_trailing_punct Logical indicating whether to remove trailing
#'        punctuation
#'
#' @return Normalized character string
#'
#' @keywords internal
#' @noRd
normalize_text <- function(
    text,
    capitalize_first = TRUE,
    remove_trailing_punct = TRUE) {
  if (is.null(text) || length(text) == 0 || all(nchar(trimws(text))) == 0) {
    return(text)
  }

  # trim whitespace
  text <- trimws(text)

  # remove trailing punctuation if requested
  if (remove_trailing_punct) {
    text <- gsub("[.!?]+$", "", text)
  }

  # capitalize first letter if requested
  if (capitalize_first && nchar(text) > 0) {
    substring(text, 1, 1) <- toupper(substring(text, 1, 1))
  }

  return(text)
}

#' Format suggestion text with proper capitalization and punctuation
#'
#' @param suggestions Character vector of suggestion texts
#' @param separator Character string to use for joining
#'
#' @return Properly formatted character string
#'
#' @keywords internal
#' @noRd
format_suggestions <- function(suggestions, separator = ", ") {
  if (length(suggestions) == 0) {
    return("")
  }

  # normalize each suggestion
  normalized_suggestions <- vapply(suggestions, function(s) {
    normalize_text(s, capitalize_first = TRUE, remove_trailing_punct = TRUE)
  }, character(1))

  # join with separator
  paste(normalized_suggestions, collapse = separator)
}

#' Quiet-aware wrapper for cli::cli_alert_info
#'
#' @param ... Arguments passed to cli::cli_alert_info
#' @param quiet Logical indicating whether to suppress the alert.
#'        If NULL, uses getOption("bidux.quiet", FALSE)
#'
#' @return NULL invisibly
#'
#' @keywords internal
#' @noRd
bid_alert_info <- function(..., quiet = NULL) {
  # check quiet mode: function parameter > global option > default
  is_quiet <- quiet %||% getOption("bidux.quiet", FALSE)

  # if quiet mode is enabled, return without outputting anything
  if (is_quiet) {
    return(invisible(NULL))
  }

  # otherwise, show the alert
  cli::cli_alert_info(...)
  invisible(NULL)
}
