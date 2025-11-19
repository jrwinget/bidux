# Document Dashboard Structure Stage in BID Framework

This function documents the structure of the dashboard with automatic
layout selection and generates ranked, concept-grouped actionable UI/UX
suggestions. Layout is intelligently chosen based on content analysis of
previous stages using deterministic heuristics. Returns structured
recommendations with specific component pointers and implementation
rationales.

## Usage

``` r
bid_structure(
  previous_stage,
  concepts = NULL,
  telemetry_flags = NULL,
  quiet = NULL,
  ...
)
```

## Arguments

- previous_stage:

  A tibble or list output from an earlier BID stage function.

- concepts:

  A character vector of additional BID concepts to include. Concepts can
  be provided in natural language (e.g., "Principle of Proximity") or
  with underscores (e.g., "principle_of_proximity"). The function uses
  fuzzy matching to identify the concepts. If NULL, will detect relevant
  concepts from previous stages automatically.

- telemetry_flags:

  Optional named list of telemetry flags from bid_flags(). Used to
  adjust layout choice and suggestion scoring based on observed user
  behavior patterns.

- quiet:

  Logical indicating whether to suppress informational messages. If
  NULL, uses getOption("bidux.quiet", FALSE).

- ...:

  Additional parameters. If `layout` is provided via `...`, the function
  will abort with a helpful error message.

## Value

A bid_stage object containing:

- stage:

  "Structure"

- layout:

  Auto-selected layout type

- suggestions:

  List of concept groups with ranked suggestions (nested format)

- suggestions_tbl:

  Flattened tibble with all suggestions, includes columns: concept,
  title, details, components, rationale, score, difficulty, category

- concepts:

  Comma-separated string of all concepts used

## Details

**Layout Auto-Selection**: For backwards compatibility with versions \<
0.3.0; to be removed in 0.4.0. Uses deterministic heuristics to analyze
content from previous stages and select the most appropriate layout:

- **breathable**: For information overload/confusion patterns

- **dual_process**: For overview vs detail needs

- **grid**: For grouping/comparison requirements

- **card**: For modular/chunked content

- **tabs**: For categorical organization (unless telemetry shows issues)

**Suggestion Engine**: Generates ranked, actionable recommendations
grouped by UX concepts. Each suggestion includes specific R dashboard
components (Shiny, bslib, DT, plotly, etc.), implementation details, and
rationale. Suggestions are scored based on relevance, layout
appropriateness, and contextual factors. Component suggestions work with
both Shiny applications and Quarto dashboards, with shiny-prefixed
components (i.e., `shiny::`) requiring Shiny runtime.

## Examples

``` r
notice_result <- bid_interpret(
  central_question = "How can we simplify data presentation?",
  data_story = list(
    hook = "Data is too complex",
    context = "Overloaded with charts",
    tension = "Confusing layout",
    resolution = "Introduce clear grouping"
  )
) |>
  bid_notice(
    problem = "Users struggle with information overload",
    evidence = "Survey results indicate delays"
  )
#> Warning: ! Using deprecated list format for data_story parameter
#> ℹ Please use new_data_story() constructor for new code
#> ℹ Legacy format will be automatically migrated
#> Warning: ! Using deprecated nested format for data_story
#> ℹ The flat API is now recommended: new_data_story(hook, context, tension,
#>   resolution)
#> ℹ Nested format (variables, relationships) will be removed in bidux 0.4.0
#> Stage 1 (Interpret) completed.
#>   - Central question: How can we simplify data presentation?
#>   - Your data story is incomplete (25%). Consider adding these missing elements: hook, tension, resolution.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined 
#> Auto-suggested theory: Processing Fluency (confidence: 70%)
#> Stage 2 (Notice) completed. (40% complete)
#>   - Problem: Users struggle with information overload
#>   - Theory: Processing Fluency (auto-suggested)
#>   - Evidence: Survey results indicate delays
#>   - Theory confidence: 70%
#>   - Next: Use bid_anticipate() for Stage 3 

# Auto-selected layout with concept-grouped suggestions
structure_result <- bid_structure(previous_stage = notice_result)
#> ℹ Auto-selected layout: breathable
#> ℹ Detected information overload patterns; choosing 'breathable' to reduce cognitive load.
#> Warning: Layout auto-selection is deprecated and will be removed in bidux 0.4.0. The BID framework will focus on concept-based suggestions instead. Existing code will continue to work until 0.4.0.
#> ℹ Tip: Learn more about any concept via bid_concept("<concept>").
#> Stage 4 (Structure) completed.
#>   - Auto-selected layout: breathable
#>   - Concept groups generated: 4
#>   - Total concepts: 4 
print(structure_result$layout) # Auto-selected layout
#> [1] "breathable" "breathable" "breathable" "breathable"
print(structure_result$suggestions) # Ranked suggestions by concept (nested)
#> [[1]]
#> [[1]]$concept
#> [1] "Cognitive Load Theory"
#> 
#> [[1]]$suggestions
#> [[1]]$suggestions[[1]]
#> [[1]]$suggestions[[1]]$title
#> [1] "Limit initial choices"
#> 
#> [[1]]$suggestions[[1]]$details
#> [1] "Show only core filters by default; defer advanced options to secondary views or accordions."
#> 
#> [[1]]$suggestions[[1]]$components
#> [1] "bslib::accordion"            "shiny::conditionalPanel"    
#> [3] "shiny::updateSelectizeInput"
#> 
#> [[1]]$suggestions[[1]]$rationale
#> [1] "Reduces initial cognitive load for new users while preserving functionality."
#> 
#> [[1]]$suggestions[[1]]$score
#> [1] 1
#> 
#> 
#> [[1]]$suggestions[[2]]
#> [[1]]$suggestions[[2]]$title
#> [1] "Use progressive complexity"
#> 
#> [[1]]$suggestions[[2]]$details
#> [1] "Start with simple views and allow users to add complexity incrementally."
#> 
#> [[1]]$suggestions[[2]]$components
#> [1] "shiny::tabsetPanel"  "bslib::accordion"    "shiny::actionButton"
#> 
#> [[1]]$suggestions[[2]]$rationale
#> [1] "Prevents overwhelming users with too many options at once."
#> 
#> [[1]]$suggestions[[2]]$score
#> [1] 0.91
#> 
#> 
#> [[1]]$suggestions[[3]]
#> [[1]]$suggestions[[3]]$title
#> [1] "Provide smart defaults"
#> 
#> [[1]]$suggestions[[3]]$details
#> [1] "Pre-select commonly used filters and settings to reduce decision fatigue."
#> 
#> [[1]]$suggestions[[3]]$components
#> [1] "shiny::selectInput"   "shiny::checkboxInput" "bslib::input_switch" 
#> 
#> [[1]]$suggestions[[3]]$rationale
#> [1] "Leverages the Default Effect to reduce cognitive burden."
#> 
#> [[1]]$suggestions[[3]]$score
#> [1] 0.88
#> 
#> 
#> 
#> 
#> [[2]]
#> [[2]]$concept
#> [1] "Visual Hierarchy"
#> 
#> [[2]]$suggestions
#> [[2]]$suggestions[[1]]
#> [[2]]$suggestions[[1]]$title
#> [1] "Establish clear information priority"
#> 
#> [[2]]$suggestions[[1]]$details
#> [1] "Use size, color, and spacing to guide attention to key metrics first."
#> 
#> [[2]]$suggestions[[1]]$components
#> [1] "bslib::card"      "shiny::h1"        "shiny::h2"        "bslib::value_box"
#> 
#> [[2]]$suggestions[[1]]$rationale
#> [1] "Helps users quickly identify what matters most in the interface."
#> 
#> [[2]]$suggestions[[1]]$score
#> [1] 0.9
#> 
#> 
#> [[2]]$suggestions[[2]]
#> [[2]]$suggestions[[2]]$title
#> [1] "Group related content visually"
#> 
#> [[2]]$suggestions[[2]]$details
#> [1] "Use consistent spacing and visual containers to show relationships."
#> 
#> [[2]]$suggestions[[2]]$components
#> [1] "bslib::layout_columns" "bslib::card"           "shiny::fluidRow"      
#> 
#> [[2]]$suggestions[[2]]$rationale
#> [1] "Leverages Gestalt principles to reduce cognitive processing."
#> 
#> [[2]]$suggestions[[2]]$score
#> [1] 0.87
#> 
#> 
#> 
#> 
#> [[3]]
#> [[3]]$concept
#> [1] "Progressive Disclosure"
#> 
#> [[3]]$suggestions
#> [[3]]$suggestions[[1]]
#> [[3]]$suggestions[[1]]$title
#> [1] "Use collapsible advanced filters"
#> 
#> [[3]]$suggestions[[1]]$details
#> [1] "Place seldom-used filters in accordion sections or 'Show more' toggles."
#> 
#> [[3]]$suggestions[[1]]$components
#> [1] "bslib::accordion"        "shiny::conditionalPanel"
#> 
#> [[3]]$suggestions[[1]]$rationale
#> [1] "Reveals complexity on demand without overwhelming the interface."
#> 
#> [[3]]$suggestions[[1]]$score
#> [1] 0.88
#> 
#> 
#> [[3]]$suggestions[[2]]
#> [[3]]$suggestions[[2]]$title
#> [1] "Implement drill-down navigation"
#> 
#> [[3]]$suggestions[[2]]$details
#> [1] "Allow users to start with summaries and progressively reveal details."
#> 
#> [[3]]$suggestions[[2]]$components
#> [1] "shiny::actionButton"  "DT::datatable"        "reactable::reactable"
#> 
#> [[3]]$suggestions[[2]]$rationale
#> [1] "Matches user mental models of exploration from general to specific."
#> 
#> [[3]]$suggestions[[2]]$score
#> [1] 0.84
#> 
#> 
#> 
#> 
#> [[4]]
#> [[4]]$concept
#> [1] "Processing Fluency"
#> 
#> [[4]]$suggestions
#> [[4]]$suggestions[[1]]
#> [[4]]$suggestions[[1]]$title
#> [1] "Apply Processing Fluency principles"
#> 
#> [[4]]$suggestions[[1]]$details
#> [1] "Consider how Processing Fluency applies to your breathable layout design."
#> 
#> [[4]]$suggestions[[1]]$components
#> [1] "bslib::card"           "shiny::fluidRow"       "bslib::layout_columns"
#> 
#> [[4]]$suggestions[[1]]$rationale
#> [1] "Systematic application of Processing Fluency can improve user experience."
#> 
#> [[4]]$suggestions[[1]]$score
#> [1] 0.75
#> 
#> 
#> 
#> 

# Access flattened tibble format for easier manipulation
suggestions_flat <- structure_result$suggestions_tbl[[1]]
print(suggestions_flat)
#> # A tibble: 8 × 8
#>   concept           title details components rationale score difficulty category
#>   <chr>             <chr> <chr>   <chr>      <chr>     <dbl> <chr>      <chr>   
#> 1 Cognitive Load T… Limi… Show o… bslib::ac… Reduces …  1    Hard       Interac…
#> 2 Cognitive Load T… Use … Start … shiny::ta… Prevents…  0.91 Hard       Complex…
#> 3 Visual Hierarchy  Esta… Use si… bslib::ca… Helps us…  0.9  Hard       Visual …
#> 4 Cognitive Load T… Prov… Pre-se… shiny::se… Leverage…  0.88 Medium     Interac…
#> 5 Progressive Disc… Use … Place … bslib::ac… Reveals …  0.88 Medium     Interac…
#> 6 Visual Hierarchy  Grou… Use co… bslib::la… Leverage…  0.87 Medium     Visual …
#> 7 Progressive Disc… Impl… Allow … shiny::ac… Matches …  0.84 Medium     Navigat…
#> 8 Processing Fluen… Appl… Consid… bslib::ca… Systemat…  0.75 Medium     Layout  

# Filter by difficulty
easy_suggestions <- suggestions_flat[suggestions_flat$difficulty == "Easy", ]

# Filter by category
layout_suggestions <- suggestions_flat[suggestions_flat$category == "Layout", ]

summary(structure_result)
#> === BID Framework: Structure Stage Summary ===
#> 
#> Metadata:
#>    layout_type : breathable 
#>    auto_selected_layout : Yes 
#>    concepts_count : 4 
#>    suggestion_groups_count : 4 
#>    stage_number : 4 
#>    stage_number_previous : 3 
#>    total_stages : 5 
#> 
#> Stage Data:
#>    stage : Structure 
#>    layout : breathable 
#>    concepts : Cognitive Load Theory, Visual Hierarchy, Progressive Disclosure, Processing F... 
#>    previous_problem : Users struggle with information overload 
#>    previous_theory : Processing Fluency 
#>    suggestions : list(concept = "Cognitive Load Theory", suggestions = list(list(title = "Limi... 
#>    suggestions_tbl : list(concept = c("Cognitive Load Theory", "Cognitive Load Theory", "Visual Hi... 
#> 
#> Generated: 2025-11-19 21:31:17 
```
