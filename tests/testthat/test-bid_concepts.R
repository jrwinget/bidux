library(testthat)
library(tibble)

# bid_concepts ----

test_that("bid_concepts returns a tibble with expected columns", {
  result <- bid_concepts()

  expect_s3_class(result, "tbl_df")
  expected_cols <- c(
    "concept", "description", "category",
    "reference", "example", "implementation_tips"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("bid_concepts returns the full list when no search is provided", {
  result <- bid_concepts()
  expect_true(nrow(result) >= 41)
})

test_that("bid_concepts search argument filters results correctly", {
  result <- bid_concepts("cognitive")
  expect_true(all(grepl("cognitive", tolower(result$concept))))
})

test_that("bid_concepts search is case-insensitive", {
  result_upper <- bid_concepts("HICK")
  result_lower <- bid_concepts("hick")
  expect_equal(nrow(result_upper), nrow(result_lower))
})

test_that("bid_concepts returns an empty tibble if search term does not match any concept", {
  result <- bid_concepts("nonexistentconcept")
  expect_equal(nrow(result), 0)
})

test_that("bid_concepts handles multiple search terms correctly", {
  result_comma <- bid_concepts("cognitive, hierarchy")

  expect_s3_class(result_comma, "tbl_df")
  expect_true(nrow(result_comma) > 0)
  expect_true(
    all(
      grepl("cognitive|hierarchy", tolower(
        paste(
          result_comma$concept,
          result_comma$description,
          result_comma$implementation_tips
        )
      ), perl = TRUE)
    )
  )

  result_space <- bid_concepts("visual proximity")

  expect_s3_class(result_space, "tbl_df")
  expect_true(nrow(result_space) > 0)
  expect_true(
    all(
      grepl("visual|proximity", tolower(
        paste(result_space$concept, result_space$description, result_space$implementation_tips)
      ), perl = TRUE)
    )
  )
})

test_that("bid_concepts handles edge case search parameters", {
  result_empty <- bid_concepts("")

  expect_s3_class(result_empty, "tbl_df")
  expect_equal(nrow(result_empty), nrow(bid_concepts()))

  result_whitespace <- bid_concepts("   ")

  expect_s3_class(result_whitespace, "tbl_df")
  expect_equal(nrow(result_whitespace), nrow(bid_concepts()))

  result_short <- bid_concepts("UI")

  expect_s3_class(result_short, "tbl_df")
  expect_true(nrow(result_short) > 0)

  long_term <- paste(rep("longterm", 20), collapse = "")
  result_long <- bid_concepts(long_term)

  expect_s3_class(result_long, "tbl_df")
  expect_true(nrow(result_long) >= 0)
})

test_that("bid_concepts returns results ordered by relevance", {
  result <- bid_concepts("cognitive")

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 1)

  primary_match_positions <- grep("cognitive", tolower(result$concept))
  secondary_match_positions <- grep("cognitive", tolower(result$description))

  if (
    length(primary_match_positions) > 0 &&
      length(secondary_match_positions) > 0
  ) {
    expect_true(min(primary_match_positions) < min(secondary_match_positions))
  }
})

test_that("bid_concepts properly scores and ranks search results by relevance", {
  results <- bid_concepts("visual hierarchy")
  expect_true(
    grepl("visual hierarchy", tolower(results$concept[1]), fixed = TRUE) ||
      any(
        sapply(
          results$concept[1:3],
          function(x) grepl("visual hierarchy", tolower(x), fixed = TRUE)
        )
      )
  )
})

# bid_concept ----

test_that("bid_concept handles exact, partial, and fuzzy matches", {
  result_exact <- bid_concept("cognitive load theory")

  expect_s3_class(result_exact, "tbl_df")
  expect_true(nrow(result_exact) > 0)
  expect_equal(tolower(result_exact$concept[1]), "cognitive load theory")

  result_partial <- bid_concept("load theory")

  expect_s3_class(result_partial, "tbl_df")
  expect_true(nrow(result_partial) > 0)
  expect_match(result_partial$concept[1], "Load Theory", ignore.case = TRUE)

  result_single <- bid_concept("proximity")

  expect_s3_class(result_single, "tbl_df")
  expect_true(nrow(result_single) > 0)
  expect_match(result_single$concept[1], "Proximity", ignore.case = TRUE)

  result_fuzzy <- bid_concept("cognitve lod theory") # intentional typos

  expect_s3_class(result_fuzzy, "tbl_df")
  expect_true(nrow(result_fuzzy) > 0)
  expect_match(result_fuzzy$concept[1], "Cognitive", ignore.case = TRUE)
})

test_that("bid_concept handles edge cases", {
  result_empty <- bid_concept("")

  expect_null(result_empty)
  expect_false(is.null(attr(result_empty, "suggestions")))

  result_whitespace <- bid_concept("   ")

  expect_null(result_whitespace)
  expect_false(is.null(attr(result_whitespace, "suggestions")))

  result_short <- bid_concept("UI")

  expect_null(result_short)
  expect_false(is.null(attr(result_short, "suggestions")))

  result_nonmatching <- bid_concept("xyzabc123")

  expect_null(result_nonmatching)
  expect_false(is.null(attr(result_nonmatching, "suggestions")))
})

test_that("bid_concept finds concepts by related concepts", {
  concepts <- bid_concepts()
  concept_with_relations <- concepts[
    !is.na(concepts$related_concepts) &
      concepts$related_concepts != "",
  ][1, ]

  related_concept <- strsplit(
    concept_with_relations$related_concepts,
    ", "
  )[[1]][1]

  result <- bid_concept(related_concept)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)

  found <- FALSE
  if (tolower(result$concept[1]) == tolower(related_concept)) {
    found <- TRUE
  } else if (
    !is.na(result$related_concepts[1]) &&
      grepl(
        concept_with_relations$concept,
        result$related_concepts[1],
        ignore.case = TRUE
      )
  ) {
    found <- TRUE
  }

  expect_true(found)
})

test_that("bid_concept handles synonyms appropriately", {
  synonym_pairs <- list(
    c("choice", "option"),
    c("visual", "display"),
    c("hierarchy", "organization"),
    c("cognitive", "mental"),
    c("proximity", "closeness")
  )

  for (pair in synonym_pairs) {
    result1 <- bid_concept(pair[1])

    if (!is.null(result1) && nrow(result1) > 0) {
      result2 <- bid_concept(pair[2])

      if (!is.null(result2) && nrow(result2) > 0) {
        same_concept <- identical(result1$concept[1], result2$concept[1])

        related <- FALSE
        if (!is.na(result1$related_concepts[1]) &&
          grepl(
            result2$concept[1],
            result1$related_concepts[1],
            ignore.case = TRUE
          )
        ) {
          related <- TRUE
        } else if (
          !is.na(result2$related_concepts[1]) &&
            grepl(
              result1$concept[1],
              result2$related_concepts[1],
              ignore.case = TRUE
            )
        ) {
          related <- TRUE
        }

        expect_true(
          same_concept || related,
          info = paste(
            "Terms", pair[1], "and", pair[2],
            "should find same or related concepts"
          )
        )
      }
    }
  }
})

test_that("bid_concept handles concepts with special characters", {
  result <- bid_concept("Hick's Law")
  expect_s3_class(result, "tbl_df")
  expect_true(grepl("Hick", result$concept[1], fixed = TRUE))
})
