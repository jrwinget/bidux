test_that("assign_difficulty correctly categorizes suggestions", {
  # easy suggestion
  easy_suggestion <- list(
    title = "Add help text",
    details = "Add contextual help text to the interface",
    components = c("shiny::helpText")
  )
  expect_equal(assign_difficulty(easy_suggestion), "Easy")

  # medium suggestion
  medium_suggestion <- list(
    title = "Implement filters",
    details = "Create filter controls for the data",
    components = c("shiny::selectInput", "shiny::dateRangeInput", "shiny::actionButton")
  )
  expect_equal(assign_difficulty(medium_suggestion), "Medium")

  # hard suggestion
  hard_suggestion <- list(
    title = "Build complex navigation",
    details = "Implement a complex navigation system with multiple conditional steps",
    components = c(
      "shiny::navbarPage",
      "shiny::conditionalPanel",
      "bslib::accordion",
      "shiny::tabsetPanel"
    )
  )
  expect_equal(assign_difficulty(hard_suggestion), "Hard")

  # hard based on complexity keywords
  complex_suggestion <- list(
    title = "Advanced integration",
    details = "Implement system integration with multiple advanced features",
    components = c("shiny::observeEvent", "shiny::reactive")
  )
  expect_equal(assign_difficulty(complex_suggestion), "Hard")
})

test_that("assign_category correctly categorizes suggestions", {
  # navigation category
  nav_suggestion <- list(
    title = "Improve navigation",
    details = "Add tab navigation for better structure"
  )
  expect_equal(assign_category(nav_suggestion, "Progressive Disclosure"), "Navigation")

  # layout category
  layout_suggestion <- list(
    title = "Group related content",
    details = "Use consistent spacing and layout containers"
  )
  expect_equal(assign_category(layout_suggestion, "Visual Hierarchy"), "Layout")

  # content category
  content_suggestion <- list(
    title = "Improve labels",
    details = "Use descriptive text and clear information"
  )
  expect_equal(assign_category(content_suggestion, "Information Scent"), "Content")

  # interaction category
  interaction_suggestion <- list(
    title = "Add filter controls",
    details = "Provide interactive filter inputs for users"
  )
  expect_equal(assign_category(interaction_suggestion, "User Control"), "Interaction")

  # visual design category
  visual_suggestion <- list(
    title = "Establish visual hierarchy",
    details = "Use size, color, and visual emphasis to guide attention"
  )
  expect_equal(assign_category(visual_suggestion, "Visual Hierarchy"), "Visual Design")

  # guidance category
  guidance_suggestion <- list(
    title = "Add onboarding help",
    details = "Provide tutorial and guidance for new users"
  )
  expect_equal(assign_category(guidance_suggestion, "User Onboarding"), "Guidance")

  # fallback to concept-based categorization
  cognitive_load_suggestion <- list(
    title = "Simplify interface",
    details = "Reduce options to minimize burden"
  )
  expect_equal(
    assign_category(cognitive_load_suggestion, "Cognitive Load Theory"),
    "Complexity Management"
  )
})

test_that("flatten_suggestions_to_tibble creates correct structure", {
  # create mock suggestion groups
  suggestion_groups <- list(
    list(
      concept = "Cognitive Load Theory",
      suggestions = list(
        list(
          title = "Limit initial choices",
          details = "Show only core filters by default",
          components = c("bslib::accordion", "shiny::conditionalPanel"),
          rationale = "Reduces cognitive load",
          score = 0.92
        ),
        list(
          title = "Provide smart defaults",
          details = "Pre-select commonly used settings",
          components = c("shiny::selectInput"),
          rationale = "Reduces decision fatigue",
          score = 0.85
        )
      )
    ),
    list(
      concept = "Visual Hierarchy",
      suggestions = list(
        list(
          title = "Establish clear priority",
          details = "Use size and color to guide attention",
          components = c("bslib::card", "bslib::value_box"),
          rationale = "Helps users identify key information",
          score = 0.90
        )
      )
    )
  )

  result <- flatten_suggestions_to_tibble(suggestion_groups)

  # check structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 8)

  # check column names
  expected_cols <- c(
    "concept",
    "title",
    "details",
    "components",
    "rationale",
    "score",
    "difficulty",
    "category"
  )
  expect_equal(names(result), expected_cols)

  # check column types
  expect_type(result$concept, "character")
  expect_type(result$title, "character")
  expect_type(result$details, "character")
  expect_type(result$components, "character")
  expect_type(result$rationale, "character")
  expect_type(result$score, "double")
  expect_type(result$difficulty, "character")
  expect_type(result$category, "character")

  # check that components are comma-separated strings
  expect_type(result$components, "character")
  # first row has 2 components, should have comma
  expect_true(grepl(",", result$components[1]))

  # check that rows are sorted by score (descending)
  expect_equal(result$score, sort(result$score, decreasing = TRUE))
  expect_equal(result$score[1], 0.92)
  expect_equal(result$score[3], 0.85)

  # check that difficulty and category are assigned
  expect_true(all(result$difficulty %in% c("Easy", "Medium", "Hard")))
  expect_true(all(nchar(result$category) > 0))
})

test_that("flatten_suggestions_to_tibble handles empty input", {
  result <- flatten_suggestions_to_tibble(list())

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 8)
  expect_equal(
    names(result),
    c(
      "concept",
      "title",
      "details",
      "components",
      "rationale",
      "score",
      "difficulty",
      "category"
    )
  )
})

test_that("bid_structure includes suggestions_tbl column", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users are overwhelmed by too many options",
    evidence = "Cognitive load issues reported",
    central_question = "How to reduce overload?",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  # check that suggestions_tbl exists
  expect_true("suggestions_tbl" %in% names(result))

  # check that it's a list column
  expect_true(is.list(result$suggestions_tbl))

  # extract the tibble
  suggestions_tbl <- result$suggestions_tbl[[1]]

  # check structure
  expect_s3_class(suggestions_tbl, "tbl_df")
  expect_gt(nrow(suggestions_tbl), 0)
  expect_equal(ncol(suggestions_tbl), 8)

  # check required columns
  expected_cols <- c(
    "concept",
    "title",
    "details",
    "components",
    "rationale",
    "score",
    "difficulty",
    "category"
  )
  expect_equal(names(suggestions_tbl), expected_cols)

  # verify all rows have valid data
  expect_true(all(nchar(suggestions_tbl$concept) > 0))
  expect_true(all(nchar(suggestions_tbl$title) > 0))
  expect_true(all(nchar(suggestions_tbl$details) > 0))
  expect_true(all(nchar(suggestions_tbl$components) > 0))
  expect_true(all(nchar(suggestions_tbl$rationale) > 0))
  expect_true(all(suggestions_tbl$score >= 0 & suggestions_tbl$score <= 1))
  expect_true(all(suggestions_tbl$difficulty %in% c("Easy", "Medium", "Hard")))
  expect_true(all(nchar(suggestions_tbl$category) > 0))
})

test_that("bid_structure maintains backward compatibility with suggestions column", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Users need overview and detail modes",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  # check that both formats are available
  expect_true("suggestions" %in% names(result))
  expect_true("suggestions_tbl" %in% names(result))

  # check nested format structure
  suggestions <- result$suggestions
  expect_true(is.list(suggestions))
  expect_gt(length(suggestions), 0)

  # each group should have concept and suggestions
  for (group in suggestions) {
    expect_true("concept" %in% names(group))
    expect_true("suggestions" %in% names(group))
    expect_true(is.character(group$concept))
    expect_true(is.list(group$suggestions))
  }

  # verify tibble has same number of suggestions as nested format
  suggestions_tbl <- result$suggestions_tbl[[1]]
  total_nested <- sum(sapply(suggestions, function(g) length(g$suggestions)))
  expect_equal(nrow(suggestions_tbl), total_nested)
})

test_that("suggestions_tbl can be filtered by difficulty", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Complex interface with multiple issues",
    evidence = "Users report confusion and cognitive overload",
    central_question = "How to simplify?",
    hook = "First-time users struggle",
    context = "Too many advanced options",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  suggestions_tbl <- result$suggestions_tbl[[1]]

  # filter by difficulty
  easy <- suggestions_tbl[suggestions_tbl$difficulty == "Easy", ]
  medium <- suggestions_tbl[suggestions_tbl$difficulty == "Medium", ]
  hard <- suggestions_tbl[suggestions_tbl$difficulty == "Hard", ]

  # should have suggestions in different difficulty levels
  # (at least one difficulty level should have suggestions)
  total_suggestions <- nrow(easy) + nrow(medium) + nrow(hard)
  expect_equal(total_suggestions, nrow(suggestions_tbl))

  # check that filtering works
  expect_true(all(easy$difficulty == "Easy") || nrow(easy) == 0)
  expect_true(all(medium$difficulty == "Medium") || nrow(medium) == 0)
  expect_true(all(hard$difficulty == "Hard") || nrow(hard) == 0)
})

test_that("suggestions_tbl can be filtered by category", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Multiple UX issues across interface",
    evidence = "Navigation issues and visual hierarchy problems",
    central_question = "How to improve overall design?",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  suggestions_tbl <- result$suggestions_tbl[[1]]

  # get unique categories
  categories <- unique(suggestions_tbl$category)
  expect_gt(length(categories), 0)

  # filter by first category
  first_category <- categories[1]
  filtered <- suggestions_tbl[suggestions_tbl$category == first_category, ]

  expect_gt(nrow(filtered), 0)
  expect_true(all(filtered$category == first_category))
})

test_that("suggestions_tbl ordering matches score ranking", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Dashboard needs improvements",
    evidence = "User feedback indicates issues",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  suggestions_tbl <- result$suggestions_tbl[[1]]

  # verify descending order by score
  if (nrow(suggestions_tbl) > 1) {
    for (i in 1:(nrow(suggestions_tbl) - 1)) {
      expect_gte(suggestions_tbl$score[i], suggestions_tbl$score[i + 1])
    }
  }
})

test_that("components are properly comma-separated in tibble", {
  previous_stage <- tibble::tibble(
    stage = "Interpret",
    problem = "Interface needs component recommendations",
    timestamp = Sys.time()
  )

  suppressMessages(
    result <- bid_structure(previous_stage)
  )

  suggestions_tbl <- result$suggestions_tbl[[1]]

  # check that components column exists and is character
  expect_true("components" %in% names(suggestions_tbl))
  expect_type(suggestions_tbl$components, "character")

  # verify comma separation for multi-component suggestions
  multi_component <- suggestions_tbl[grepl(",", suggestions_tbl$components), ]
  if (nrow(multi_component) > 0) {
    # should be able to split by comma
    components_list <- strsplit(multi_component$components[1], ", ")[[1]]
    expect_gt(length(components_list), 1)
    expect_true(all(nchar(trimws(components_list)) > 0))
  }
})
