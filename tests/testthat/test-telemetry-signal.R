# tests for R/telemetry_signal.R and the pattern-concept loader in
# R/mappings.R. covers: constructor validation, safe-predicate ast
# whitelist, matcher logic (pattern filtering, predicate evaluation,
# confidence ranking, fallback), and the loader's custom / default paths.

# ==============================================================================
# HELPERS
# ==============================================================================

sample_mappings_df <- function() {
  data.frame(
    pattern_type = c(
      "unused_input",
      "unused_input",
      "unused_input",
      "delayed_interaction"
    ),
    subtype = c(
      "never_touched",
      "default_kept",
      NA_character_,
      NA_character_
    ),
    feature_predicate = c(
      "sessions_used == 0",
      "sessions_used > 0 && value_variance == 0",
      NA_character_,
      NA_character_
    ),
    concept = c(
      "Affordance",
      "Default Effect",
      "Cognitive Load Theory",
      "Cognitive Load Theory"
    ),
    stage = c("Structure", "Structure", "Notice", "Notice"),
    confidence = c(0.85, 0.85, 0.7, 0.8),
    rationale_citation = c(
      "Norman (1988)",
      "Samuelson & Zeckhauser (1988)",
      "Sweller (1988)",
      "Sweller (1988)"
    ),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# CONSTRUCTOR
# ==============================================================================

test_that("bid_pattern_signal constructs with valid inputs", {
  s <- bid_pattern_signal(
    pattern_type = "unused_input",
    subtype = "never_touched",
    features = list(sessions_used = 0L, usage_rate = 0),
    evidence_parts = c("0 of 10 sessions touched input")
  )
  expect_s3_class(s, "bid_pattern_signal")
  expect_identical(s$pattern_type, "unused_input")
  expect_identical(s$subtype, "never_touched")
  expect_identical(s$features$sessions_used, 0L)
  expect_identical(s$evidence_parts, "0 of 10 sessions touched input")
})

test_that("bid_pattern_signal defaults subtype to NA_character_ and features to empty list", {
  s <- bid_pattern_signal("unused_input")
  expect_true(is.na(s$subtype))
  expect_identical(s$features, list())
  expect_identical(s$evidence_parts, character())
})

test_that("bid_pattern_signal coerces NULL subtype to NA", {
  s <- bid_pattern_signal("unused_input", subtype = NULL)
  expect_true(is.na(s$subtype))
})

test_that("bid_pattern_signal rejects invalid pattern_type", {
  expect_error(bid_pattern_signal(pattern_type = 1), "pattern_type")
  expect_error(bid_pattern_signal(pattern_type = ""), "pattern_type")
  expect_error(bid_pattern_signal(pattern_type = "   "), "pattern_type")
  expect_error(bid_pattern_signal(pattern_type = NA_character_), "pattern_type")
  expect_error(bid_pattern_signal(pattern_type = c("a", "b")), "pattern_type")
})

test_that("bid_pattern_signal rejects invalid subtype", {
  expect_error(bid_pattern_signal("x", subtype = 1), "subtype")
  expect_error(bid_pattern_signal("x", subtype = c("a", "b")), "subtype")
})

test_that("bid_pattern_signal rejects non-list features", {
  expect_error(bid_pattern_signal("x", features = c(1, 2)), "features")
  expect_error(bid_pattern_signal("x", features = "not a list"), "features")
})

test_that("bid_pattern_signal requires named features", {
  expect_error(bid_pattern_signal("x", features = list(1, 2)), "named")
  expect_error(
    bid_pattern_signal("x", features = stats::setNames(list(1), "")),
    "named"
  )
})

test_that("bid_pattern_signal rejects non-scalar feature values", {
  expect_error(
    bid_pattern_signal("x", features = list(a = 1:3)),
    "scalar"
  )
  expect_error(
    bid_pattern_signal("x", features = list(a = list(1))),
    "scalar"
  )
})

test_that("bid_pattern_signal rejects non-character evidence_parts", {
  expect_error(bid_pattern_signal("x", evidence_parts = 1:3), "evidence_parts")
})

test_that("is_bid_pattern_signal identifies instances and rejects others", {
  s <- bid_pattern_signal("unused_input")
  expect_true(is_bid_pattern_signal(s))
  expect_false(is_bid_pattern_signal(list()))
  expect_false(is_bid_pattern_signal("foo"))
  expect_false(is_bid_pattern_signal(NULL))
})

# ==============================================================================
# SAFE PREDICATE EVALUATOR
# ==============================================================================

test_that(".eval_feature_predicate returns TRUE for NA / empty / whitespace predicates", {
  expect_true(.eval_feature_predicate(NA_character_, list()))
  expect_true(.eval_feature_predicate("", list()))
  expect_true(.eval_feature_predicate("   ", list()))
  expect_true(.eval_feature_predicate(NULL, list()))
})

test_that(".eval_feature_predicate evaluates comparisons against features", {
  f <- list(sessions_used = 0, usage_rate = 0.02)
  expect_true(.eval_feature_predicate("sessions_used == 0", f))
  expect_false(.eval_feature_predicate("sessions_used > 0", f))
  expect_true(.eval_feature_predicate("usage_rate < 0.05", f))
  expect_true(.eval_feature_predicate("usage_rate <= 0.02", f))
  expect_true(.eval_feature_predicate("sessions_used != 5", f))
})

test_that(".eval_feature_predicate supports logical operators", {
  f <- list(a = 1, b = 2)
  expect_true(.eval_feature_predicate("a == 1 && b == 2", f))
  expect_false(.eval_feature_predicate("a == 1 && b == 3", f))
  expect_true(.eval_feature_predicate("a == 1 || b == 99", f))
  expect_true(.eval_feature_predicate("!(a == 99)", f))
  expect_true(.eval_feature_predicate("a == 1 & b == 2", f))
  expect_false(.eval_feature_predicate("a == 99 | b == 99", f))
})

test_that(".eval_feature_predicate supports arithmetic", {
  f <- list(x = 10, y = 2)
  expect_true(.eval_feature_predicate("x / y == 5", f))
  expect_true(.eval_feature_predicate("x - y * 2 == 6", f))
  expect_true(.eval_feature_predicate("(x + y) / 2 == 6", f))
})

test_that(".eval_feature_predicate supports %in% with c()", {
  f <- list(subtype = "a")
  expect_true(.eval_feature_predicate('subtype %in% c("a", "b")', f))
  expect_false(.eval_feature_predicate('subtype %in% c("c", "d")', f))
})

test_that(".eval_feature_predicate rejects non-whitelisted function calls", {
  f <- list(x = 1)
  expect_false(.eval_feature_predicate('system("echo hi")', f))
  expect_false(.eval_feature_predicate('Sys.getenv("HOME")', f))
  expect_false(.eval_feature_predicate('file.remove("x")', f))
  expect_false(.eval_feature_predicate("quote(x)", f))
  expect_false(.eval_feature_predicate('eval(parse(text = "1+1"))', f))
  expect_false(.eval_feature_predicate("sum(1, 2)", f))
})

test_that(".eval_feature_predicate rejects namespaced calls", {
  f <- list(x = 1)
  expect_false(.eval_feature_predicate('base::system("ls")', f))
  expect_false(.eval_feature_predicate("base:::list2env(list())", f))
  expect_false(.eval_feature_predicate('utils::download.file("x","y")', f))
})

test_that(".eval_feature_predicate returns FALSE for unparseable input", {
  expect_false(.eval_feature_predicate("a === b", list(a = 1, b = 2)))
  expect_false(.eval_feature_predicate("((", list()))
  expect_false(.eval_feature_predicate("a + ", list(a = 1)))
})

test_that(".eval_feature_predicate returns FALSE when a referenced feature is missing", {
  expect_false(.eval_feature_predicate("missing_feature == 1", list(a = 1)))
})

test_that(".eval_feature_predicate ignores extra features not referenced", {
  f <- list(a = 1, unused = 99)
  expect_true(.eval_feature_predicate("a == 1", f))
})

# ==============================================================================
# CONCEPT MATCHER
# ==============================================================================

test_that("match_signal_to_concept picks subtype-specific row when predicate matches", {
  mappings <- sample_mappings_df()
  sig <- bid_pattern_signal(
    pattern_type = "unused_input",
    subtype = "never_touched",
    features = list(sessions_used = 0, value_variance = 0)
  )
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Affordance")
  expect_identical(result$stage, "Structure")
  expect_equal(result$confidence, 0.85)
  expect_identical(result$subtype_matched, "never_touched")
  expect_false(result$is_fallback)
})

test_that("match_signal_to_concept falls through to the default row when subtype predicates fail", {
  mappings <- sample_mappings_df()
  sig <- bid_pattern_signal(
    pattern_type = "unused_input",
    features = list(sessions_used = 3, value_variance = 0.5)
  )
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Cognitive Load Theory")
  expect_equal(result$confidence, 0.7)
  expect_true(is.na(result$subtype_matched))
  expect_false(result$is_fallback)
})

test_that("match_signal_to_concept only considers rows matching pattern_type", {
  mappings <- sample_mappings_df()
  sig <- bid_pattern_signal("delayed_interaction")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Cognitive Load Theory")
  expect_equal(result$confidence, 0.8)
})

test_that("match_signal_to_concept returns fallback when no row matches pattern_type", {
  mappings <- sample_mappings_df()
  sig <- bid_pattern_signal("unknown_pattern")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Cognitive Load Theory")
  expect_equal(result$confidence, 0.5)
  expect_true(result$is_fallback)
})

test_that("match_signal_to_concept picks highest confidence on multi-match", {
  mappings <- data.frame(
    pattern_type = rep("unused_input", 3L),
    subtype = c("a", "b", NA_character_),
    feature_predicate = c("x == 1", "x == 1", NA_character_),
    concept = c("Low", "High", "Default"),
    stage = rep("Notice", 3L),
    confidence = c(0.6, 0.9, 0.5),
    rationale_citation = c("ref_a", "ref_b", "ref_default"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input", features = list(x = 1))
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "High")
  expect_equal(result$confidence, 0.9)
})

test_that("match_signal_to_concept breaks confidence ties on first-match", {
  mappings <- data.frame(
    pattern_type = rep("unused_input", 2L),
    subtype = c("first", "second"),
    feature_predicate = c("x == 1", "x == 1"),
    concept = c("FirstWin", "SecondWin"),
    stage = rep("Notice", 2L),
    confidence = c(0.8, 0.8),
    rationale_citation = c("a", "b"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input", features = list(x = 1))
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "FirstWin")
  expect_identical(result$subtype_matched, "first")
})

test_that("match_signal_to_concept accepts NULL mappings and loads built-in defaults", {
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig)
  expect_true(is.character(result$concept))
  expect_true(result$confidence > 0)
  expect_false(result$is_fallback)
})

test_that("match_signal_to_concept validates that input is a bid_pattern_signal", {
  expect_error(match_signal_to_concept(list(x = 1)), "bid_pattern_signal")
  expect_error(match_signal_to_concept("foo"), "bid_pattern_signal")
})

test_that("match_signal_to_concept works with tibble mappings", {
  skip_if_not_installed("tibble")
  mappings_tbl <- tibble::as_tibble(sample_mappings_df())
  sig <- bid_pattern_signal(
    "unused_input",
    features = list(sessions_used = 0, value_variance = 0)
  )
  result <- match_signal_to_concept(sig, mappings = mappings_tbl)
  expect_identical(result$concept, "Affordance")
})

test_that("match_signal_to_concept returns fallback when all matching rows have NA confidence", {
  mappings <- data.frame(
    pattern_type = rep("unused_input", 2L),
    subtype = c("a", NA_character_),
    feature_predicate = c(NA_character_, NA_character_),
    concept = c("Bad", "AlsoBad"),
    stage = rep("Notice", 2L),
    confidence = c(NA_real_, NA_real_),
    rationale_citation = c("a", "b"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_true(result$is_fallback)
  expect_identical(result$concept, "Cognitive Load Theory")
})

test_that("match_signal_to_concept returns fallback for empty mappings", {
  mappings <- sample_mappings_df()[0, ]
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_true(result$is_fallback)
})

test_that("match_signal_to_concept coerces character confidence values", {
  mappings <- data.frame(
    pattern_type = rep("unused_input", 2L),
    subtype = c("low", "high"),
    feature_predicate = c(NA_character_, NA_character_),
    concept = c("LowConf", "HighConf"),
    stage = rep("Notice", 2L),
    confidence = c("0.5", "0.9"),
    rationale_citation = c("a", "b"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "HighConf")
})

test_that("match_signal_to_concept treats non-numeric confidence as NA", {
  mappings <- data.frame(
    pattern_type = rep("unused_input", 2L),
    subtype = c("a", "b"),
    feature_predicate = c(NA_character_, NA_character_),
    concept = c("Useful", "Garbage"),
    stage = rep("Notice", 2L),
    confidence = c("0.7", "not-a-number"),
    rationale_citation = c("a", "b"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Useful")
})

test_that("match_signal_to_concept tolerates mappings without a subtype column", {
  mappings <- data.frame(
    pattern_type = "unused_input",
    feature_predicate = NA_character_,
    concept = "NoSubtype",
    stage = "Notice",
    confidence = 0.9,
    rationale_citation = "ref",
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "NoSubtype")
  expect_true(is.na(result$subtype_matched))
})

test_that("match_signal_to_concept handles mappings missing optional columns", {
  sparse <- data.frame(
    pattern_type = "unused_input",
    subtype = NA_character_,
    feature_predicate = NA_character_,
    concept = "SparseConcept",
    confidence = 0.9,
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input")
  result <- match_signal_to_concept(sig, mappings = sparse)
  expect_identical(result$concept, "SparseConcept")
  expect_true(is.na(result$stage))
  expect_true(is.na(result$rationale))
  expect_false(result$is_fallback)
})

test_that(".is_safe_predicate_expr rejects backtick-quoted namespaced names", {
  # reviewer flagged this as a potential bypass; verify it is rejected
  # because the backticked string is not in the operator whitelist.
  expr1 <- parse(text = '`base::system`("ls")')[[1L]]
  expect_false(.is_safe_predicate_expr(expr1))
  expr2 <- parse(text = '`utils::download.file`("a","b")')[[1L]]
  expect_false(.is_safe_predicate_expr(expr2))
})

test_that(".is_safe_predicate_expr rejects exotic expression types (defensive)", {
  # pairlist is not atomic, not a name, and not a call — triggers the
  # defensive "not a call" branch.
  expect_false(.is_safe_predicate_expr(pairlist(1)))
})

test_that("match_signal_to_concept returns fallback when all predicates evaluate FALSE", {
  # no default (NA) row, so signal falls off the end of surviving rows.
  mappings <- data.frame(
    pattern_type = rep("unused_input", 2L),
    subtype = c("a", "b"),
    feature_predicate = c("x == 1", "x == 2"),
    concept = c("A", "B"),
    stage = rep("Notice", 2L),
    confidence = c(0.8, 0.8),
    rationale_citation = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  sig <- bid_pattern_signal("unused_input", features = list(x = 99))
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_true(result$is_fallback)
})

test_that(".is_safe_predicate_expr rejects string-form calls to non-whitelisted heads", {
  # R's parser normalizes `"=="(a, b)` to `a == b` (normal form), which is
  # safe. but a string-form call to a non-whitelisted function normalizes
  # to a regular call whose head name is not in the whitelist, so the
  # whitelist rejects it.
  expr <- parse(text = '"system"("ls")')[[1L]]
  expect_false(.is_safe_predicate_expr(expr))
})

test_that(".is_safe_predicate_expr rejects computed function heads", {
  # `get("system")("ls")` has a call (not a name) as the head.
  expr <- parse(text = 'get("system")("ls")')[[1L]]
  expect_false(.is_safe_predicate_expr(expr))
})

test_that(".eval_feature_predicate returns FALSE for multi-statement input", {
  expect_false(.eval_feature_predicate("1; 2", list()))
})

test_that(".eval_feature_predicate returns FALSE when result is non-logical", {
  # eval returns "hello" (character), not TRUE; isTRUE() rejects it.
  expect_false(.eval_feature_predicate("'hello'", list()))
  expect_false(.eval_feature_predicate("1 + 1", list()))
})

test_that("match_signal_to_concept tolerates NA pattern_type rows in mappings", {
  mappings <- rbind(
    sample_mappings_df(),
    data.frame(
      pattern_type = NA_character_,
      subtype = NA_character_,
      feature_predicate = NA_character_,
      concept = "Garbage",
      stage = "Notice",
      confidence = 0.99,
      rationale_citation = "na row",
      stringsAsFactors = FALSE
    )
  )
  sig <- bid_pattern_signal("delayed_interaction")
  result <- match_signal_to_concept(sig, mappings = mappings)
  expect_identical(result$concept, "Cognitive Load Theory")
})

# ==============================================================================
# MAPPING LOADER
# ==============================================================================

test_that("load_pattern_concept_mappings reads built-in CSV with required columns", {
  m <- load_pattern_concept_mappings()
  required <- c(
    "pattern_type", "subtype", "feature_predicate",
    "concept", "stage", "confidence"
  )
  expect_true(all(required %in% names(m)))
  expect_gte(nrow(m), 5L)
})

test_that("load_pattern_concept_mappings built-in seed covers all five current patterns", {
  m <- load_pattern_concept_mappings()
  expected <- c(
    "unused_input", "delayed_interaction", "error_pattern",
    "navigation_dropoff", "confusion_pattern"
  )
  expect_true(all(expected %in% unique(m$pattern_type)))
})

test_that("load_pattern_concept_mappings accepts custom mappings", {
  custom <- sample_mappings_df()
  m <- load_pattern_concept_mappings(custom)
  expect_equal(nrow(m), nrow(custom))
  expect_identical(m$concept, custom$concept)
})

test_that("load_pattern_concept_mappings errors when custom is missing required columns", {
  bad <- data.frame(pattern_type = "x", concept = "y")
  expect_error(load_pattern_concept_mappings(bad), "columns")
})

test_that("get_default_pattern_concept_mappings returns a valid fallback shape", {
  m <- get_default_pattern_concept_mappings()
  required <- c(
    "pattern_type", "subtype", "feature_predicate",
    "concept", "stage", "confidence"
  )
  expect_true(all(required %in% names(m)))
  expect_equal(nrow(m), 5L)
  expect_true(all(m$confidence > 0 & m$confidence <= 1))
})

# ==============================================================================
# INTEGRATION: signal → mapping → concept round-trip via shipped CSV
# ==============================================================================

test_that("each existing pattern resolves to a concept via the shipped mapping", {
  patterns <- c(
    "unused_input", "delayed_interaction", "error_pattern",
    "navigation_dropoff", "confusion_pattern"
  )
  for (p in patterns) {
    sig <- bid_pattern_signal(p)
    result <- match_signal_to_concept(sig)
    expect_false(
      result$is_fallback,
      info = sprintf("pattern %s should have a non-fallback mapping", p)
    )
    expect_true(
      nchar(result$concept) > 0,
      info = sprintf("pattern %s should resolve to a non-empty concept", p)
    )
  }
})
