# structured telemetry pattern signal and concept matcher.
# a signal carries the measured structural features of a detected pattern,
# so the concept mapper can key off properties rather than prose keyword regex.
# this is the foundation for extending telemetry coverage beyond the stage-1
# concepts currently reachable via suggest_theory_from_mappings().

#' Allowed call symbols inside a feature predicate.
#' @keywords internal
#' @noRd
.pattern_signal_safe_ops <- c(
  "==", "!=", "<", "<=", ">", ">=",
  "&&", "||", "&", "|", "!",
  "+", "-", "*", "/", "^",
  "%in%",
  "(", "{", "c"
)

#' Test whether a value is a scalar atomic.
#' @keywords internal
#' @noRd
.is_scalar_atomic <- function(x) {
  is.atomic(x) && length(x) == 1L
}

#' Construct a telemetry pattern signal
#'
#' @description
#' Internal constructor for the structured output of a telemetry pattern
#' detector. Each signal carries a pattern type, optional subtype, a named
#' list of measured features (scalar atomics), and optional pre-built
#' evidence fragments. Downstream, `match_signal_to_concept()` resolves
#' the signal to a BID concept via the shipped mapping table.
#'
#' @param pattern_type Non-empty character scalar (e.g. `"unused_input"`).
#' @param subtype Optional character scalar (`NA_character_` or `NULL`
#'   when no subtype applies).
#' @param features Named list of scalar numeric / integer / logical /
#'   character values describing the detected pattern.
#' @param evidence_parts Optional character vector of pre-rendered
#'   evidence fragments for notice construction.
#'
#' @return An object of S3 class `"bid_pattern_signal"`.
#' @keywords internal
#' @noRd
bid_pattern_signal <- function(
    pattern_type,
    subtype = NA_character_,
    features = list(),
    evidence_parts = character()) {
  if (
    !is.character(pattern_type) || length(pattern_type) != 1L ||
      is.na(pattern_type) || !nzchar(trimws(pattern_type))
  ) {
    cli::cli_abort("`pattern_type` must be a non-empty character scalar.")
  }

  if (is.null(subtype)) {
    subtype <- NA_character_
  }
  if (!is.character(subtype) || length(subtype) != 1L) {
    cli::cli_abort("`subtype` must be NULL or a character scalar (NA allowed).")
  }

  if (!is.list(features)) {
    cli::cli_abort("`features` must be a named list.")
  }
  if (length(features) > 0L) {
    if (is.null(names(features)) || any(!nzchar(names(features)))) {
      cli::cli_abort("`features` must be a named list (all names non-empty).")
    }
    bad <- !vapply(features, .is_scalar_atomic, logical(1L))
    if (any(bad)) {
      cli::cli_abort(
        "All `features` values must be scalar atomic (length-1 numeric, integer, logical, or character)."
      )
    }
  }

  if (!is.character(evidence_parts)) {
    cli::cli_abort("`evidence_parts` must be a character vector.")
  }

  structure(
    list(
      pattern_type = pattern_type,
      subtype = subtype,
      features = features,
      evidence_parts = evidence_parts
    ),
    class = "bid_pattern_signal"
  )
}

#' Test whether an object is a `bid_pattern_signal`.
#' @keywords internal
#' @noRd
is_bid_pattern_signal <- function(x) {
  inherits(x, "bid_pattern_signal")
}

# ---- safe predicate evaluation ----------------------------------------------

#' Walk an expression and accept only whitelisted calls.
#'
#' @description
#' Returns TRUE if the expression consists only of atomics, names, and
#' calls to operators in `.pattern_signal_safe_ops`. Namespaced calls
#' (e.g. `base::system`) parse with a `::` head that is itself a call, so
#' they are rejected by the "call head must be a name in whitelist" rule.
#'
#' @keywords internal
#' @noRd
.is_safe_predicate_expr <- function(expr) {
  if (is.atomic(expr) || is.name(expr)) {
    return(TRUE)
  }
  if (!is.call(expr)) {
    return(FALSE)
  }
  fn <- expr[[1L]]
  if (!is.name(fn)) {
    # rejects `::`, `:::`, `(` as a computed function, etc.
    return(FALSE)
  }
  if (!as.character(fn) %in% .pattern_signal_safe_ops) {
    return(FALSE)
  }
  all(vapply(
    as.list(expr)[-1L],
    .is_safe_predicate_expr,
    logical(1L)
  ))
}

#' Evaluate a feature predicate against a signal's features.
#'
#' @description
#' NA / empty / whitespace-only predicates evaluate to TRUE so "default"
#' mapping rows act as catch-alls. Unparseable expressions, predicates
#' containing non-whitelisted calls, and evaluation errors (including
#' references to missing features) all yield FALSE.
#'
#' @keywords internal
#' @noRd
.eval_feature_predicate <- function(predicate, features) {
  if (
    is.null(predicate) || length(predicate) != 1L ||
      is.na(predicate) || !nzchar(trimws(predicate))
  ) {
    return(TRUE)
  }

  parsed <- tryCatch(
    parse(text = predicate),
    error = function(e) NULL
  )
  if (is.null(parsed) || length(parsed) != 1L) {
    return(FALSE)
  }

  expr <- parsed[[1L]]
  if (!.is_safe_predicate_expr(expr)) {
    return(FALSE)
  }

  # bind features in an env whose parent is baseenv() so base operators
  # (==, &&, %in%, etc.) resolve normally. namespaced calls were rejected
  # above, so the whitelist is the only escape hatch.
  env <- list2env(features, parent = baseenv())
  result <- tryCatch(eval(expr, envir = env), error = function(e) FALSE)
  isTRUE(result)
}

# ---- concept matcher --------------------------------------------------------

#' Package-historical fallback returned when no mapping row matches or
#' all matching rows have unusable confidence values.
#' @keywords internal
#' @noRd
.pattern_signal_fallback <- function() {
  list(
    concept = "Cognitive Load Theory",
    stage = "Notice",
    confidence = 0.5,
    rationale = "Sweller (1988); package fallback when no mapping row matches",
    subtype_matched = NA_character_,
    is_fallback = TRUE
  )
}

#' Resolve a pattern signal to a BID concept
#'
#' @description
#' Walks the pattern-to-concept mapping table, filters to rows whose
#' `pattern_type` matches the signal, keeps rows whose `feature_predicate`
#' evaluates to TRUE against the signal's features, then returns the
#' highest-confidence surviving row. Ties on confidence break on original
#' row order (stable first-match).
#'
#' If no row matches the signal's `pattern_type`, returns the package
#' fallback (Cognitive Load Theory, confidence 0.5, `is_fallback = TRUE`).
#'
#' @param signal A `bid_pattern_signal`.
#' @param mappings Optional data frame / tibble with columns
#'   `pattern_type`, `subtype`, `feature_predicate`, `concept`, `stage`,
#'   `confidence` (and optional `rationale_citation`). When `NULL`, loads
#'   the shipped mappings via `load_pattern_concept_mappings()`.
#'
#' @return A list with elements `concept`, `stage`, `confidence`,
#'   `rationale`, `subtype_matched`, `is_fallback`.
#' @keywords internal
#' @noRd
match_signal_to_concept <- function(signal, mappings = NULL) {
  if (!is_bid_pattern_signal(signal)) {
    cli::cli_abort("`signal` must be a `bid_pattern_signal` object.")
  }

  if (is.null(mappings)) {
    mappings <- load_pattern_concept_mappings()
  }

  pattern_rows <- mappings[
    !is.na(mappings$pattern_type) &
      mappings$pattern_type == signal$pattern_type, ,
    drop = FALSE
  ]
  if (nrow(pattern_rows) == 0L) {
    return(.pattern_signal_fallback())
  }

  matches <- vapply(
    seq_len(nrow(pattern_rows)),
    function(i) {
      .eval_feature_predicate(
        pattern_rows$feature_predicate[i],
        signal$features
      )
    },
    logical(1L)
  )
  surviving <- pattern_rows[matches, , drop = FALSE]
  if (nrow(surviving) == 0L) {
    return(.pattern_signal_fallback())
  }

  # coerce confidence so user-supplied mappings with character columns
  # ("0.8") still rank correctly; non-numeric entries become NA and are
  # filtered below.
  conf <- suppressWarnings(as.numeric(surviving$confidence))
  if (all(is.na(conf))) {
    # which.max() on all-NA returns integer(0), which would silently yield a
    # 0-row winner. treat all-NA confidence as "no usable rows".
    return(.pattern_signal_fallback())
  }

  # highest confidence wins; which.max returns first index on ties.
  winner <- surviving[which.max(conf), , drop = FALSE]

  subtype_val <- if ("subtype" %in% names(winner)) winner$subtype else NA

  list(
    concept = as.character(winner$concept),
    stage = if ("stage" %in% names(winner)) as.character(winner$stage) else NA_character_,
    confidence = as.numeric(winner$confidence),
    rationale = if ("rationale_citation" %in% names(winner)) {
      as.character(winner$rationale_citation)
    } else {
      NA_character_
    },
    subtype_matched = if (is.na(subtype_val)) NA_character_ else as.character(subtype_val),
    is_fallback = FALSE
  )
}
