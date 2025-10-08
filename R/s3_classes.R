#' Create a data story object
#'
#' @description
#' Creates a structured data story object for use in bid_interpret() and other
#' bidux functions. Replaces nested list structures with a validated S3 class.
#'
#' @param context Character string describing the data context
#' @param variables List of variable descriptions or relationships
#' @param relationships List describing data relationships
#' @param metadata Optional metadata list
#'
#' @return A bid_data_story S3 object
#'
#' @examples
#' \dontrun{
#' story <- new_data_story(
#'   context = "user engagement analysis",
#'   variables = list(clicks = "number of clicks", sessions = "session count"),
#'   relationships = list(engagement = "clicks per session")
#' )
#' }
#'
#' @export
new_data_story <- function(context, variables = list(), relationships = list(), metadata = NULL) {
  validate_character_param(context, "context", required = TRUE)

  if (!is.list(variables)) {
    cli::cli_abort(standard_error_msg(
      "Parameter 'variables' must be a list",
      context = glue::glue("You provided: {class(variables)[1]}")
    ))
  }

  if (!is.list(relationships)) {
    cli::cli_abort(standard_error_msg(
      "Parameter 'relationships' must be a list",
      context = glue::glue("You provided: {class(relationships)[1]}")
    ))
  }

  structure(
    list(
      context = context,
      variables = variables,
      relationships = relationships,
      metadata = metadata,
      created_at = Sys.time()
    ),
    class = c("bid_data_story", "list")
  )
}

#' Validate data story object
#'
#' @param x Object to validate
#'
#' @return TRUE if valid, FALSE otherwise
#'
#' @keywords internal
#' @noRd
validate_data_story <- function(x) {
  if (!inherits(x, "bid_data_story")) {
    return(FALSE)
  }

  required_fields <- c("context", "variables", "relationships")
  if (!all(required_fields %in% names(x))) {
    return(FALSE)
  }

  if (!is.character(x$context) || length(x$context) != 1) {
    return(FALSE)
  }

  if (!is.list(x$variables) || !is.list(x$relationships)) {
    return(FALSE)
  }

  return(TRUE)
}

#' Print method for data story objects
#'
#' @param x A bid_data_story object
#' @param ... Additional arguments (unused)
#'
#' @export
print.bid_data_story <- function(x, ...) {
  cat("bidux data story:\n")
  cat("  context:", x$context, "\n")
  cat("  variables:", length(x$variables), "defined\n")
  cat("  relationships:", length(x$relationships), "defined\n")
  if (!is.null(x$created_at)) {
    cat("  created:", format(x$created_at), "\n")
  }
}

#' Create user personas tibble
#'
#' @description
#' Creates a structured user personas tibble for use in bidux functions.
#' Replaces nested list structures with a validated data.frame structure.
#'
#' @param personas_df Data.frame with required columns: name, goals, pain_points, technical_level
#'
#' @return A bid_user_personas S3 object (inherits from data.frame)
#'
#' @examples
#' \dontrun{
#' personas <- new_user_personas(data.frame(
#'   name = c("data analyst", "product manager"),
#'   goals = c("quick insights", "strategic overview"),
#'   pain_points = c("complex tools", "data delays"),
#'   technical_level = c("intermediate", "beginner")
#' ))
#' }
#'
#' @export
new_user_personas <- function(personas_df) {
  validate_data_frame(personas_df, "personas_df")

  required_cols <- c("name", "goals", "pain_points", "technical_level")
  validate_data_frame(personas_df, "personas_df", required_columns = required_cols)

  # validate technical_level values (case-insensitive)
  valid_levels <- c("beginner", "intermediate", "advanced", "expert")
  # normalize to lowercase for validation
  normalized_levels <- tolower(personas_df$technical_level)
  invalid_levels <- setdiff(normalized_levels, valid_levels)
  if (length(invalid_levels) > 0) {
    original_invalid <- personas_df$technical_level[normalized_levels %in% invalid_levels]
    cli::cli_abort(standard_error_msg(
      "Invalid technical_level values found",
      context = glue::glue("Invalid values: {paste(original_invalid, collapse = ', ')}"),
      suggestions = glue::glue("Valid levels: {paste(valid_levels, collapse = ', ')}")
    ))
  }
  # update to normalized lowercase values
  personas_df$technical_level <- normalized_levels

  structure(
    tibble::as_tibble(personas_df),
    class = c("bid_user_personas", "tbl_df", "tbl", "data.frame"),
    created_at = Sys.time()
  )
}

#' Validate user personas object
#'
#' @param x Object to validate
#'
#' @return TRUE if valid, FALSE otherwise
#'
#' @keywords internal
#' @noRd
validate_user_personas <- function(x) {
  if (!inherits(x, "bid_user_personas")) {
    return(FALSE)
  }

  required_cols <- c("name", "goals", "pain_points", "technical_level")
  if (!all(required_cols %in% names(x))) {
    return(FALSE)
  }

  if (nrow(x) == 0) {
    return(FALSE)
  }

  return(TRUE)
}

#' Print method for user personas objects
#'
#' @param x A bid_user_personas object
#' @param ... Additional arguments passed to print.data.frame
#'
#' @export
print.bid_user_personas <- function(x, ...) {
  cat("bidux user personas (", nrow(x), " personas):\n", sep = "")
  NextMethod("print")
}

#' Create bias mitigations tibble
#'
#' @description
#' Creates a structured bias mitigations tibble for use in bidux functions.
#' Replaces nested list structures with a validated data.frame structure.
#'
#' @param mitigations_df Data.frame with required columns: bias_type, mitigation_strategy, confidence_level
#'
#' @return A bid_bias_mitigations S3 object (inherits from data.frame)
#'
#' @examples
#' \dontrun{
#' mitigations <- new_bias_mitigations(data.frame(
#'   bias_type = c("confirmation_bias", "selection_bias"),
#'   mitigation_strategy = c("seek_disconfirming_evidence", "randomize_sample"),
#'   confidence_level = c(0.8, 0.7)
#' ))
#' }
#'
#' @export
new_bias_mitigations <- function(mitigations_df) {
  validate_data_frame(mitigations_df, "mitigations_df")

  required_cols <- c("bias_type", "mitigation_strategy", "confidence_level")
  validate_data_frame(mitigations_df, "mitigations_df", required_columns = required_cols)

  # validate confidence_level is numeric between 0 and 1
  if (!is.numeric(mitigations_df$confidence_level)) {
    cli::cli_abort(standard_error_msg(
      "confidence_level must be numeric",
      context = glue::glue("You provided: {class(mitigations_df$confidence_level)[1]}")
    ))
  }

  invalid_confidence <- mitigations_df$confidence_level < 0 | mitigations_df$confidence_level > 1
  if (any(invalid_confidence)) {
    cli::cli_abort(standard_error_msg(
      "confidence_level values must be between 0 and 1",
      context = glue::glue("Invalid values found: {paste(mitigations_df$confidence_level[invalid_confidence], collapse = ', ')}")
    ))
  }

  structure(
    tibble::as_tibble(mitigations_df),
    class = c("bid_bias_mitigations", "tbl_df", "tbl", "data.frame"),
    created_at = Sys.time()
  )
}

#' Validate bias mitigations object
#'
#' @param x Object to validate
#'
#' @return TRUE if valid, FALSE otherwise
#'
#' @keywords internal
#' @noRd
validate_bias_mitigations <- function(x) {
  if (!inherits(x, "bid_bias_mitigations")) {
    return(FALSE)
  }

  required_cols <- c("bias_type", "mitigation_strategy", "confidence_level")
  if (!all(required_cols %in% names(x))) {
    return(FALSE)
  }

  if (nrow(x) == 0) {
    return(FALSE)
  }

  return(TRUE)
}

#' Print method for bias mitigations objects
#'
#' @param x A bid_bias_mitigations object
#' @param ... Additional arguments passed to print.data.frame
#'
#' @export
print.bid_bias_mitigations <- function(x, ...) {
  cat("bidux bias mitigations (", nrow(x), " strategies):\n", sep = "")
  NextMethod("print")
}

#' Migrate old list-based data story to new S3 class
#'
#' @param old_list Legacy list structure
#'
#' @return bid_data_story object
#'
#' @keywords internal
#' @noRd
migrate_data_story <- function(old_list) {
  if (!is.list(old_list)) {
    cli::cli_abort(standard_error_msg(
      "data_story must be a list or bid_data_story object",
      suggestions = "Use new_data_story() constructor for new code"
    ))
  }

  # provide meaningful defaults for empty values and map old structure
  context <- old_list$context %||% "Legacy data story migration"

  # handle non-character context values (convert to character)
  if (!is.character(context)) {
    context <- "Legacy data story migration"
  }

  # handle empty string context
  if (is.character(context) && nchar(trimws(context)) == 0) {
    context <- "Legacy data story migration"
  }

  # map old structure to new structure, preserving legacy fields in variables/relationships
  variables <- old_list$variables %||% list()
  relationships <- old_list$relationships %||% list()

  # map legacy fields to appropriate locations
  if ("hook" %in% names(old_list)) {
    variables$hook <- old_list$hook
  }
  if ("tension" %in% names(old_list)) {
    variables$tension <- old_list$tension
  }
  if ("resolution" %in% names(old_list)) {
    relationships$resolution <- old_list$resolution
  }

  # preserve other fields in metadata
  excluded_fields <- c("context", "variables", "relationships", "hook", "tension", "resolution")
  metadata <- old_list[!names(old_list) %in% excluded_fields]

  new_data_story(
    context = context,
    variables = variables,
    relationships = relationships,
    metadata = metadata
  )
}

#' Migrate old list-based user personas to new S3 class
#'
#' @param old_list Legacy list of lists structure
#'
#' @return bid_user_personas object
#'
#' @keywords internal
#' @noRd
migrate_user_personas <- function(old_list) {
  if (!is.list(old_list)) {
    cli::cli_abort(standard_error_msg(
      "user_personas must be a list or bid_user_personas object",
      suggestions = "Use new_user_personas() constructor for new code"
    ))
  }

  # convert list of lists to data.frame
  personas_df <- do.call(rbind.data.frame, lapply(old_list, function(p) {
    data.frame(
      name = p$name %||% "unnamed",
      goals = p$goals %||% p$background %||% "",
      pain_points = p$pain_points %||% p$concerns %||% "",
      technical_level = p$technical_level %||% "intermediate",
      stringsAsFactors = FALSE
    )
  }))

  new_user_personas(personas_df)
}

#' Migrate old list-based bias mitigations to new S3 class
#'
#' @param old_list Legacy named list structure
#'
#' @return bid_bias_mitigations object
#'
#' @keywords internal
#' @noRd
migrate_bias_mitigations <- function(old_list) {
  if (!is.list(old_list)) {
    cli::cli_abort(standard_error_msg(
      "bias_mitigations must be a list or bid_bias_mitigations object",
      suggestions = "Use new_bias_mitigations() constructor for new code"
    ))
  }

  # handle different old formats
  if (is.null(names(old_list))) {
    # unnamed list - assume it's already structured
    cli::cli_abort(standard_error_msg(
      "Cannot migrate unnamed list to bias_mitigations",
      suggestions = "Provide a named list with bias types as names"
    ))
  }

  # convert named list to data.frame
  mitigations_df <- data.frame(
    bias_type = names(old_list),
    mitigation_strategy = as.character(unlist(old_list)),
    confidence_level = 0.7, # default confidence
    stringsAsFactors = FALSE
  )

  new_bias_mitigations(mitigations_df)
}

#' Create consistent result structure (DRY principle)
#'
#' @description
#' Standardized result creation to reduce duplication across BID stage functions.
#' Creates a tibble or data.frame with consistent structure and S3 class.
#'
#' @param data_list List of data elements
#' @param class_name S3 class name to apply
#' @param attributes Named list of attributes to set
#' @param return_tibble Whether to return tibble if available
#'
#' @return Object with specified S3 class
#'
#' @keywords internal
#' @noRd
create_bid_result <- function(data_list, class_name, attributes = list(), return_tibble = TRUE) {

  # add timestamp if not present
  if (!"timestamp" %in% names(data_list)) {
    data_list$timestamp <- rep(Sys.time(), length(data_list[[1]]))
  }

  # create tibble or data.frame
  if (return_tibble && requireNamespace("tibble", quietly = TRUE)) {
    result <- tibble::tibble(!!!data_list)
  } else {
    result <- data.frame(data_list, stringsAsFactors = FALSE)
  }

  # apply S3 class
  class(result) <- c(class_name, class(result))

  # set attributes
  for (attr_name in names(attributes)) {
    attr(result, attr_name) <- attributes[[attr_name]]
  }

  result
}
