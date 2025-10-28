test_that("new_data_story creates valid objects with flat API", {
  # recommended flat API
  story_flat <- new_data_story(
    hook = "User engagement declining",
    context = "Dashboard usage dropped 30%",
    tension = "Don't know if UX or user needs",
    resolution = "Analyze telemetry"
  )

  expect_s3_class(story_flat, "bid_data_story")
  expect_equal(story_flat$hook, "User engagement declining")
  expect_equal(story_flat$context, "Dashboard usage dropped 30%")
  expect_equal(story_flat$tension, "Don't know if UX or user needs")
  expect_equal(story_flat$resolution, "Analyze telemetry")
  expect_true("created_at" %in% names(story_flat))

  # flat API with additional fields
  story_extended <- new_data_story(
    hook = "Revenue dashboards underutilized",
    context = "Only 40% of sales team uses dashboard",
    tension = "Critical metrics missed",
    resolution = "Redesign with behavioral science",
    audience = "Sales team",
    metrics = "adoption_rate, time_to_insight"
  )

  expect_equal(story_extended$metadata$audience, "Sales team")
  expect_equal(story_extended$metadata$metrics, "adoption_rate, time_to_insight")
})

test_that("new_data_story backward compatible with nested API", {
  # deprecated nested API with warning
  expect_warning(
    story_nested <- new_data_story(
      context = "Test context",
      variables = list(hook = "Test hook", metric = "engagement"),
      relationships = list(impact = "User satisfaction increases")
    ),
    "deprecated nested format"
  )

  expect_s3_class(story_nested, "bid_data_story")
  expect_equal(story_nested$context, "Test context")
  expect_equal(story_nested$variables$hook, "Test hook")
  expect_equal(story_nested$relationships$impact, "User satisfaction increases")
  expect_true("created_at" %in% names(story_nested))

  # nested API with metadata
  expect_warning(
    story_meta <- new_data_story(
      context = "Test with metadata",
      variables = list(),
      relationships = list(),
      source = "survey",
      confidence = 0.9
    ),
    "deprecated nested format"
  )

  expect_equal(story_meta$metadata$source, "survey")
  expect_equal(story_meta$metadata$confidence, 0.9)
})

test_that("new_data_story validates inputs", {
  # Test invalid context
  expect_error(
    new_data_story(context = NULL),
    "Parameter 'context' is required"
  )

  expect_error(
    new_data_story(context = c("multiple", "values")),
    "must be a single character string"
  )

  expect_error(
    new_data_story(context = 123),
    "must be a character string"
  )

  # Test invalid variables
  expect_error(
    new_data_story(context = "test", variables = "not a list"),
    "Parameter 'variables' must be a list"
  )

  # Test invalid relationships
  expect_error(
    new_data_story(context = "test", relationships = "not a list"),
    "Parameter 'relationships' must be a list"
  )
})

test_that("validate_data_story works correctly", {
  # Valid object
  valid_story <- new_data_story(context = "Valid story")
  expect_true(validate_data_story(valid_story))

  # Invalid class
  expect_false(validate_data_story(list(context = "test")))

  # Missing required fields
  invalid_story <- list(variables = list(), relationships = list())
  class(invalid_story) <- c("bid_data_story", "list")
  expect_false(validate_data_story(invalid_story))

  # Invalid context type
  invalid_context <- list(
    context = c("multiple", "values"),
    variables = list(),
    relationships = list()
  )
  class(invalid_context) <- c("bid_data_story", "list")
  expect_false(validate_data_story(invalid_context))
})

test_that("new_user_personas creates valid objects", {
  personas_df <- data.frame(
    name = c("Alice", "Bob"),
    goals = c("Quick analysis", "Strategic overview"),
    pain_points = c("Complex UI", "Too much detail"),
    technical_level = c("intermediate", "beginner"),
    stringsAsFactors = FALSE
  )

  personas <- new_user_personas(personas_df)

  expect_s3_class(personas, "bid_user_personas")
  expect_s3_class(personas, "tbl_df")
  expect_equal(nrow(personas), 2)
  expect_equal(personas$name[1], "Alice")
  expect_equal(personas$technical_level[2], "beginner")
  expect_true("created_at" %in% names(attributes(personas)))
})

test_that("new_user_personas validates inputs", {
  # Test invalid data frame
  expect_error(
    new_user_personas("not a data frame"),
    "must be a data.frame"
  )

  # Test missing required columns
  incomplete_df <- data.frame(name = "Alice", stringsAsFactors = FALSE)
  expect_error(
    new_user_personas(incomplete_df),
    "missing required columns"
  )

  # Test invalid technical levels
  invalid_levels_df <- data.frame(
    name = "Alice",
    goals = "Quick analysis",
    pain_points = "Complex UI",
    technical_level = "invalid_level",
    stringsAsFactors = FALSE
  )
  expect_error(
    new_user_personas(invalid_levels_df),
    "Invalid technical_level values"
  )

  # Test case insensitive technical levels
  mixed_case_df <- data.frame(
    name = "Alice",
    goals = "Quick analysis",
    pain_points = "Complex UI",
    technical_level = "Advanced",
    stringsAsFactors = FALSE
  )
  personas <- new_user_personas(mixed_case_df)
  expect_equal(personas$technical_level[1], "advanced")
})

test_that("validate_user_personas works correctly", {
  # valid object (legacy list format)
  valid_personas <- list(
    list(
      name = "Alice",
      goals = "Goals",
      pain_points = "Pain points",
      technical_level = "intermediate"
    )
  )
  expect_true(validate_user_personas(valid_personas))

  # invalid class
  expect_error(
    validate_user_personas(data.frame(name = "Alice")),
    "must be a list"
  )

  # invalid persona structure
  invalid_personas <- list(
    list(goals = "missing name")
  )
  expect_error(
    validate_user_personas(invalid_personas),
    "missing required fields"
  )
})

test_that("new_bias_mitigations creates valid objects", {
  mitigations_df <- data.frame(
    bias_type = c("anchoring", "confirmation"),
    mitigation_strategy = c("Show multiple references", "Present contradicting evidence"),
    confidence_level = c(0.8, 0.7),
    stringsAsFactors = FALSE
  )

  mitigations <- new_bias_mitigations(mitigations_df)

  expect_s3_class(mitigations, "bid_bias_mitigations")
  expect_s3_class(mitigations, "tbl_df")
  expect_equal(nrow(mitigations), 2)
  expect_equal(mitigations$bias_type[1], "anchoring")
  expect_equal(mitigations$confidence_level[1], 0.8)
})

test_that("new_bias_mitigations validates inputs", {
  # Test invalid confidence levels
  invalid_confidence_df <- data.frame(
    bias_type = "anchoring",
    mitigation_strategy = "Show references",
    confidence_level = 1.5,
    stringsAsFactors = FALSE
  )
  expect_error(
    new_bias_mitigations(invalid_confidence_df),
    "confidence_level values must be between 0 and 1"
  )

  # Test non-numeric confidence levels
  non_numeric_df <- data.frame(
    bias_type = "anchoring",
    mitigation_strategy = "Show references",
    confidence_level = "high",
    stringsAsFactors = FALSE
  )
  expect_error(
    new_bias_mitigations(non_numeric_df),
    "confidence_level must be numeric"
  )
})

test_that("migrate_data_story handles legacy formats", {
  # Test basic legacy format
  legacy_story <- list(
    context = "Legacy context",
    hook = "Legacy hook",
    tension = "Legacy tension",
    resolution = "Legacy resolution"
  )

  migrated <- migrate_data_story(legacy_story)

  expect_s3_class(migrated, "bid_data_story")
  expect_equal(migrated$context, "Legacy context")
  expect_equal(migrated$variables$hook, "Legacy hook")
  expect_equal(migrated$variables$tension, "Legacy tension")
  expect_equal(migrated$relationships$resolution, "Legacy resolution")

  # Test empty context handling
  empty_context_story <- list(context = "")
  migrated_empty <- migrate_data_story(empty_context_story)
  expect_equal(migrated_empty$context, "Legacy data story migration")

  # Test non-character context
  non_char_story <- list(context = list(complex = "structure"))
  migrated_non_char <- migrate_data_story(non_char_story)
  expect_equal(migrated_non_char$context, "Legacy data story migration")

  # Test invalid input
  expect_error(
    migrate_data_story("not a list"),
    "data_story must be a list"
  )
})

test_that("migrate_user_personas handles legacy formats", {
  # Test legacy list format
  legacy_personas <- list(
    list(
      name = "Alice",
      goals = "Quick insights",
      pain_points = "Complex interface",
      technical_level = "intermediate"
    ),
    list(
      name = "Bob",
      background = "Strategic overview", # old field name
      concerns = "Too much detail", # old field name
      technical_level = "beginner"
    )
  )

  migrated <- migrate_user_personas(legacy_personas)

  expect_s3_class(migrated, "bid_user_personas")
  expect_equal(nrow(migrated), 2)
  expect_equal(migrated$name[1], "Alice")
  expect_equal(migrated$goals[2], "Strategic overview") # mapped from background
  expect_equal(migrated$pain_points[2], "Too much detail") # mapped from concerns

  # Test with invalid input
  expect_error(
    migrate_user_personas("not a list"),
    "user_personas must be a list"
  )
})

test_that("migrate_bias_mitigations handles legacy formats", {
  # Test legacy named list format
  legacy_mitigations <- list(
    anchoring = "Show multiple reference points",
    framing = "Use positive language",
    confirmation = "Present contradicting evidence"
  )

  migrated <- migrate_bias_mitigations(legacy_mitigations)

  expect_s3_class(migrated, "bid_bias_mitigations")
  expect_equal(nrow(migrated), 3)
  expect_true("anchoring" %in% migrated$bias_type)
  expect_true("Show multiple reference points" %in% migrated$mitigation_strategy)
  expect_true(all(migrated$confidence_level == 0.7)) # default confidence

  # Test with unnamed list (should error)
  expect_error(
    migrate_bias_mitigations(list("strategy1", "strategy2")),
    "Cannot migrate unnamed list"
  )

  # Test with invalid input
  expect_error(
    migrate_bias_mitigations("not a list"),
    "bias_mitigations must be a list"
  )
})

test_that("print methods work correctly", {
  # Test print.bid_data_story
  story <- new_data_story(
    context = "Test story",
    variables = list(hook = "Test hook"),
    relationships = list(impact = "Test impact")
  )

  output <- capture.output(print(story))
  expect_true(any(grepl("bidux data story", output)))
  expect_true(any(grepl("Test story", output)))

  # Test print.bid_user_personas
  personas <- new_user_personas(data.frame(
    name = "Alice",
    goals = "Goals",
    pain_points = "Pain points",
    technical_level = "intermediate",
    stringsAsFactors = FALSE
  ))

  output_personas <- capture.output(print(personas))
  expect_true(any(grepl("bidux user personas", output_personas)))
  expect_true(any(grepl("1 personas", output_personas)))

  # Test print.bid_bias_mitigations
  mitigations <- new_bias_mitigations(data.frame(
    bias_type = "anchoring",
    mitigation_strategy = "Show references",
    confidence_level = 0.8,
    stringsAsFactors = FALSE
  ))

  output_mitigations <- capture.output(print(mitigations))
  expect_true(any(grepl("bidux bias mitigations", output_mitigations)))
  expect_true(any(grepl("1 strategies", output_mitigations)))
})
