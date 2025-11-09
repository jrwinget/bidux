# Example: Using the flattened suggestions tibble format
# This example demonstrates how to access and work with the new suggestions_tbl output

library(bidux)

# Create a simple BID workflow
interpret_result <- bid_interpret(
  central_question = "How can we reduce information overload in the dashboard?",
  data_story = list(
    hook = "Users are overwhelmed by too many options",
    context = "Complex analytics dashboard with many filters",
    tension = "Power users need advanced features but beginners get confused",
    resolution = "Progressive disclosure with smart defaults"
  )
)

notice_result <- interpret_result |>
  bid_notice(
    problem = "New users struggle with the complex interface",
    evidence = "High abandonment rate among first-time users (45%)",
    theory = "Cognitive Load Theory"
  )

# Generate structure suggestions
structure_result <- bid_structure(previous_stage = notice_result)

# Access the nested format (backward compatible)
cat("\n=== Nested Format (Original) ===\n")
cat("Number of concept groups:", length(structure_result$suggestions), "\n")
for (group in structure_result$suggestions) {
  cat("\nConcept:", group$concept, "\n")
  cat("Number of suggestions:", length(group$suggestions), "\n")
  for (i in seq_along(group$suggestions)) {
    sug <- group$suggestions[[i]]
    cat("  ", i, ".", sug$title, "(score:", sug$score, ")\n")
  }
}

# Access the flattened tibble format (NEW!)
cat("\n=== Flattened Tibble Format (New) ===\n")
suggestions_tbl <- structure_result$suggestions_tbl[[1]]
print(suggestions_tbl)

# Filter by difficulty
cat("\n=== Easy Suggestions ===\n")
easy_suggestions <- suggestions_tbl[suggestions_tbl$difficulty == "Easy", ]
print(easy_suggestions[, c("title", "difficulty", "score", "category")])

cat("\n=== Medium Suggestions ===\n")
medium_suggestions <- suggestions_tbl[suggestions_tbl$difficulty == "Medium", ]
print(medium_suggestions[, c("title", "difficulty", "score", "category")])

cat("\n=== Hard Suggestions ===\n")
hard_suggestions <- suggestions_tbl[suggestions_tbl$difficulty == "Hard", ]
print(hard_suggestions[, c("title", "difficulty", "score", "category")])

# Filter by category
cat("\n=== Suggestions by Category ===\n")
categories <- unique(suggestions_tbl$category)
for (cat_name in categories) {
  cat("\n", cat_name, ":\n", sep = "")
  cat_suggestions <- suggestions_tbl[suggestions_tbl$category == cat_name, ]
  print(cat_suggestions[, c("title", "score")])
}

# Filter by score threshold
cat("\n=== High Priority Suggestions (score >= 0.90) ===\n")
high_priority <- suggestions_tbl[suggestions_tbl$score >= 0.90, ]
print(high_priority[, c("title", "score", "difficulty", "category")])

# Get top 5 suggestions overall
cat("\n=== Top 5 Suggestions ===\n")
top_5 <- head(suggestions_tbl[order(suggestions_tbl$score, decreasing = TRUE), ], 5)
print(top_5[, c("concept", "title", "score", "difficulty")])

# Export to CSV for external analysis
# write.csv(suggestions_tbl, "suggestions_analysis.csv", row.names = FALSE)
cat("\n\nTip: You can export the tibble to CSV for further analysis:\n")
cat("  write.csv(suggestions_tbl, 'suggestions.csv', row.names = FALSE)\n")
