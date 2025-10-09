#' Create a data story object
#'
#' @description
#' Creates a structured data story object for use in bid_interpret() and other
#' bidux functions. The flattened API (hook, context, tension, resolution) is
#' recommended for most users. The nested format (variables, relationships) is
#' still supported for backward compatibility.
#'
#' @param hook Character string for the story hook (attention-grabbing opening)
#' @param context Character string describing the data context
#' @param tension Character string describing the problem or tension
#' @param resolution Character string describing the resolution or next steps
#' @param ... Optional additional fields (audience, metrics, visual_approach, etc.)
#' @param variables DEPRECATED. List of variable descriptions (use flat arguments instead)
#' @param relationships DEPRECATED. List describing data relationships (use flat arguments instead)
#'
#' @return A bid_data_story S3 object
#'
#' @examples
#' # Recommended: flat API
#' story <- new_data_story(
#'   hook = "User engagement is declining",
#'   context = "Our dashboard usage has dropped 30% this quarter",
#'   tension = "We don't know if it's UX issues or changing user needs",
#'   resolution = "Analyze telemetry to identify friction points"
#' )
#'
#' # With optional fields
#' story_detailed <- new_data_story(
#'   hook = "Revenue dashboards are underutilized",
#'   context = "Only 40% of sales team uses the new revenue dashboard",
#'   tension = "Critical metrics are being missed",
#'   resolution = "Redesign with behavioral science principles",
#'   audience = "Sales team",
#'   metrics = "adoption_rate, time_to_insight"
#' )
#'
#' @export
new_data_story <- function(hook = NULL, context = NULL, tension = NULL, resolution = NULL,
                           ..., variables = NULL, relationships = NULL) {
  # detect if using deprecated nested format
  using_nested_format <- !is.null(variables) || !is.null(relationships)

  if (using_nested_format) {
    # backward compatibility: user provided variables/relationships
    cli::cli_warn(c(
      "!" = "Using deprecated nested format for data_story",
      "i" = "The flat API is now recommended: new_data_story(hook, context, tension, resolution)",
      "i" = "Nested format (variables, relationships) will be removed in bidux 0.4.0"
    ))

    # validate old parameters
    if (is.null(context)) {
      cli::cli_abort(standard_error_msg(
        "Parameter 'context' is required",
        suggestions = "Provide context even when using nested format"
      ))
    }
    validate_character_param(context, "context", required = TRUE)

    if (!is.null(variables) && !is.list(variables)) {
      cli::cli_abort(standard_error_msg(
        "Parameter 'variables' must be a list",
        context = glue::glue("You provided: {class(variables)[1]}")
      ))
    }

    if (!is.null(relationships) && !is.list(relationships)) {
      cli::cli_abort(standard_error_msg(
        "Parameter 'relationships' must be a list",
        context = glue::glue("You provided: {class(relationships)[1]}")
      ))
    }

    # preserve old structure for backward compatibility
    variables <- variables %||% list()
    relationships <- relationships %||% list()

    structure(
      list(
        context = context,
        variables = variables,
        relationships = relationships,
        metadata = list(...),
        created_at = Sys.time()
      ),
      class = c("bid_data_story", "list")
    )
  } else {
    # new flat format: hook, context, tension, resolution
    # at least context is required
    if (is.null(context)) {
      cli::cli_abort(standard_error_msg(
        "Parameter 'context' is required",
        suggestions = c(
          "Provide context: new_data_story(context = 'your context')",
          "Or provide full story: new_data_story(hook, context, tension, resolution)"
        )
      ))
    }

    validate_character_param(context, "context", required = TRUE)

    # validate other fields if provided
    if (!is.null(hook)) validate_character_param(hook, "hook", required = FALSE)
    if (!is.null(tension)) validate_character_param(tension, "tension", required = FALSE)
    if (!is.null(resolution)) validate_character_param(resolution, "resolution", required = FALSE)

    # capture additional fields from ...
    additional_fields <- list(...)

    # create flat structure stored internally as before but exposed as flat
    structure(
      list(
        hook = hook,
        context = context,
        tension = tension,
        resolution = resolution,
        metadata = additional_fields,
        created_at = Sys.time()
      ),
      class = c("bid_data_story", "list")
    )
  }
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

  # check for new flat format (hook, context, tension, resolution)
  has_flat_format <- "hook" %in% names(x) || "tension" %in% names(x) || "resolution" %in% names(x)

  # check for old nested format (variables, relationships)
  has_nested_format <- "variables" %in% names(x) && "relationships" %in% names(x)

  # must have at least one valid format
  if (!has_flat_format && !has_nested_format) {
    return(FALSE)
  }

  # context is always required
  if (!"context" %in% names(x)) {
    return(FALSE)
  }

  if (!is.character(x$context) || length(x$context) != 1) {
    return(FALSE)
  }

  # validate flat format fields if present
  if (has_flat_format) {
    if (!is.null(x$hook) && (!is.character(x$hook) || length(x$hook) != 1)) {
      return(FALSE)
    }
    if (!is.null(x$tension) && (!is.character(x$tension) || length(x$tension) != 1)) {
      return(FALSE)
    }
    if (!is.null(x$resolution) && (!is.character(x$resolution) || length(x$resolution) != 1)) {
      return(FALSE)
    }
  }

  # validate nested format fields if present
  if (has_nested_format) {
    if (!is.list(x$variables) || !is.list(x$relationships)) {
      return(FALSE)
    }
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

  # check which format is being used
  using_flat_format <- !is.null(x$hook) || !is.null(x$tension) || !is.null(x$resolution) ||
                       (is.null(x$variables) && is.null(x$relationships))

  if (using_flat_format) {
    # new flat format display
    if (!is.null(x$hook)) cat("  hook:", x$hook, "\n")
    cat("  context:", x$context, "\n")
    if (!is.null(x$tension)) cat("  tension:", x$tension, "\n")
    if (!is.null(x$resolution)) cat("  resolution:", x$resolution, "\n")
    if (!is.null(x$metadata) && length(x$metadata) > 0) {
      cat("  additional fields:", length(x$metadata), "defined\n")
    }
  } else {
    # old nested format display (deprecated)
    cat("  context:", x$context, "\n")
    cat("  variables:", length(x$variables), "defined\n")
    cat("  relationships:", length(x$relationships), "defined\n")
    cat("  (using deprecated nested format)\n")
  }

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
