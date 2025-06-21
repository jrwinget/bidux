
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bidux <a href="https://github.com/jrwinget/bid-framework"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bidux)](https://cran.r-project.org/package=bidux)
[![R-CMD-check](https://github.com/jrwinget/bidux/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrwinget/bidux/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jrwinget/bidux/graph/badge.svg)](https://app.codecov.io/gh/jrwinget/bidux)
[![Downloads](https://cranlogs.r-pkg.org/badges/bidux)](https://cranlogs.r-pkg.org/badges/bidux)
[![Code
Size](https://img.shields.io/github/languages/code-size/jrwinget/bidux)](https://github.com/jrwinget/bidux)
<!-- badges: end -->

The `{bidux}` package helps Shiny developers implement the Behavior
Insight Design (BID) framework in their workflow. BID is a 5-stage
process that incorporates psychological principles into UI/UX design:

1.  **Notice** the Problem - Identify friction points using principles
    of cognitive load and visual hierarchies
2.  **Interpret** the User’s Need - Create compelling data stories and
    define user personas
3.  **Structure** the Dashboard - Apply layout patterns and
    accessibility considerations
4.  **Anticipate** User Behavior - Mitigate cognitive biases and
    implement effective interaction hints
5.  **Validate** & Empower the User - Provide summary insights and
    collaborative features

This structured approach helps developers create more intuitive,
user-friendly dashboards by systematically applying behavioral
psychology concepts to their design process.

## Installation

You can install bidux from CRAN:

``` r
install.packages("bidux")
```

Or install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("jrwinget/bidux")
```

## Core Functions

The package provides documentation functions for each stage of the BID
process:

``` r
library(bidux)

# Stage 1: Notice the Problem
notice_result <- bid_notice(
  problem = "Users can't find the most important metrics",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics",
  theory = "Visual Hierarchies",  # Optional - will be suggested if omitted
  target_audience = "Data analysts with varying technical skills"
)
#> Stage 1 (Notice) completed. (20% complete)
#>   - Problem: Users can't find the most important metrics
#>   - Theory: Visual Hierarchies
#>   - Evidence: User testing showed 70% of users spending >30s looking fo...
#>   - Next: Use bid_interpret() for Stage 2

# Stage 2: Interpret the User's Need
interpret_result <- bid_interpret(
  previous_stage = notice_result,
  central_question = "How are sales trending against targets?",
  data_story = list(
    hook = "Sales are trending below target in Q2",
    context = "Previous quarters exceeded targets",
    tension = "What's causing the Q2 decline?",
    resolution = "Identify underperforming product categories",
    audience = "Marketing team",
    metrics = c("YTD Sales", "Quarter Growth Rate", "Regional Performance"),
    visual_approach = "Comparative visualization with clear color coding"
  ),
  user_personas = list(
    list(
      name = "Sales Manager",
      goals = "Track team performance",
      pain_points = "Too much data to sift through",
      technical_level = "Intermediate"
    )
  )
)
#> Stage 2 (Interpret) completed.
#>   - Central question: How are sales trending against targets?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - User personas: 1 defined

# Stage 3: Structure the Dashboard
structure_result <- bid_structure(
  previous_stage = interpret_result,
  layout = "dual_process",  # Options: dual_process, grid, card, tabs, breathable
  concepts = c("principle_of_proximity", "default_effect", "breathable_layouts"),
  accessibility = list(
    color_contrast = "WCAG AA compliant",
    keyboard_navigation = "All elements focusable",
    screen_reader = "Charts include descriptive alt text"
  )
)
#> Stage 3 (Structure) completed.
#>   - Layout: dual_process
#>   - Concepts: Principle of Proximity, Default Effect, Breathable Layouts
#>   - Accessibility considerations included: color_contrast, keyboard_navigation, screen_reader

# Stage 4: Anticipate User Behavior
anticipate_result <- bid_anticipate(
  previous_stage = structure_result,
  bias_mitigations = list(
    anchoring = "Using context-aware reference points",
    framing = "Providing both positive and negative framings",
    confirmation_bias = "Including alternative scenarios"
  ),
  interaction_principles = list(
    hover_effects = "Show details on hover",
    selection_feedback = "Highlight active filters",
    progressive_disclosure = "Reveal advanced options progressively"
  )
)
#> Stage 4 (Anticipate) completed.
#>   - Bias mitigations: 3 defined
#>   - Interaction principles: 3 defined
#>   - Key suggestions: anchoring mitigation: Always show reference points like previous period, budget, or industry average., framing mitigation: Toggle between progress (65% complete) and gap (35% remaining) framing., confirmation_bias mitigation: Consider how this bias affects user decisions.

# Stage 5: Validate & Empower
validate_result <- bid_validate(
  previous_stage = anticipate_result,
  summary_panel = "Key insights panel with actionable takeaways",
  collaboration = "Team annotation and sharing capabilities",
  next_steps = c(
    "Review underperforming categories",
    "Schedule team discussion",
    "Update forecast models"
  )
)
#> Stage 5 (Validate) completed.
#>   - Summary panel: Key insights panel with actionable takeaways
#>   - Collaboration: Team annotation and sharing capabilities
#>   - Next steps: 3 items defined
#>   - Include user testing in your next steps
```

## Concept Dictionary

The package includes a comprehensive dictionary of behavioral psychology
concepts:

``` r
# List all concepts
all_concepts <- bid_concepts()
all_concepts
#> # A tibble: 41 × 7
#>    concept            description category reference example implementation_tips
#>    <chr>              <chr>       <chr>    <chr>     <chr>   <chr>              
#>  1 Cognitive Load Th… Theory tha… Stage 1  Sweller … Reduci… Use tabs or collap…
#>  2 Hick's Law         Principle … Stage 1  Hick (19… Simpli… Reduce dropdown op…
#>  3 Visual Hierarchies Design pri… Stage 1  Tufte (1… Emphas… Use size, color, a…
#>  4 Data Storytelling… Framework … Stage 2  Matei & … Highli… Start with a clear…
#>  5 Processing Fluency The ease w… Stage 2  Alter & … Using … Use clean, consist…
#>  6 Emotion & Fluency… How emotio… Stage 2  Song & S… Applyi… Consider subtle co…
#>  7 Principle of Prox… Grouping r… Stage 3  Gestalt … Groupi… Place related cont…
#>  8 Dual-Processing T… Model dist… Stage 3  Tversky … Provid… Provide both KPI s…
#>  9 Default Effect     How defaul… Stage 3  Johnson … Pre-se… Pre-select the mos…
#> 10 Aesthetic-Usabili… The effect… Stage 3  Norman (… Creati… Ensure visual cohe…
#> # ℹ 31 more rows
#> # ℹ 1 more variable: related_concepts <chr>

# Search for specific concepts
cognitive_concepts <- bid_concepts("cognitive")
cognitive_concepts
#> # A tibble: 4 × 7
#>   concept             description category reference example implementation_tips
#>   <chr>               <chr>       <chr>    <chr>     <chr>   <chr>              
#> 1 Cognitive Load The… Theory tha… Stage 1  Sweller … Reduci… Use tabs or collap…
#> 2 Progressive Disclo… Technique … Stage 1  Krug (20… Expand… Use shiny::actionB…
#> 3 Cognitive Dimensio… Framework … Stage 1  Green & … Evalua… Evaluate your UI a…
#> 4 Breathable Layouts  Using whit… Stage 3  White (2… Using … Use bslib::card_bo…
#> # ℹ 1 more variable: related_concepts <chr>

visual_concepts <- bid_concepts("visual, hierarchy") # Multiple search terms
visual_concepts
#> # A tibble: 11 × 7
#>    concept            description category reference example implementation_tips
#>    <chr>              <chr>       <chr>    <chr>     <chr>   <chr>              
#>  1 Visual Hierarchies Design pri… Stage 1  Tufte (1… Emphas… Use size, color, a…
#>  2 Principle of Prox… Grouping r… Stage 3  Gestalt … Groupi… Place related cont…
#>  3 Gestalt Principles Visual per… Stage 3  Wertheim… Arrang… Use layout_column_…
#>  4 Pre-attentive Pro… Visual pro… Stage 2  Healey (… Using … Leverage reactable…
#>  5 Change Blindness   Failure to… Stage 4  Rensink … Using … Use transitions or…
#>  6 Visual Hierarchy   Organizing… Stage 1  Djamasbi… Creati… Apply different fo…
#>  7 Breathable Layouts Using whit… Stage 3  White (2… Using … Use bslib::card_bo…
#>  8 Interaction Hints  Visual cue… Stage 4  Norman (… Adding… Add CSS hover stat…
#>  9 Visual Feedback    Immediate … Stage 4  Shneider… Highli… Use shinyFeedback …
#> 10 Accessibility Con… Ensuring s… Stage 3  WCAG 2.1  Ensuri… Test color combina…
#> 11 Information Hiera… Organizati… Stage 3  Djamasbi… Making… Use font size, wei…
#> # ℹ 1 more variable: related_concepts <chr>

# Get detailed information about a specific concept
anchoring_info <- bid_concept("anchoring effect")
anchoring_info
#> # A tibble: 1 × 8
#>   concept          description    category reference example implementation_tips
#>   <chr>            <chr>          <chr>    <chr>     <chr>   <chr>              
#> 1 Anchoring Effect Bias where in… Stage 4  Tversky … Using … Always show refere…
#> # ℹ 2 more variables: related_concepts <chr>, recommendations <chr>
```

## UI Component Suggestions

Get concrete implementation ideas for various UI packages:

``` r
# Get bslib component suggestions
bid_suggest_components(structure_result, package = "bslib")
#> # A tibble: 10 × 7
#>    package component          description bid_stage_relevance cognitive_concepts
#>    <chr>   <chr>              <chr>       <chr>               <chr>             
#>  1 bslib   layout_columns     Create fle… Stage 3             Visual Hierarchy,…
#>  2 bslib   layout_column_wrap Create res… Stage 3             Cognitive Load Th…
#>  3 bslib   card               Organize c… Stage 3,Stage 5     Principle of Prox…
#>  4 bslib   card_body          Control ca… Stage 3             Breathable Layout…
#>  5 bslib   page_sidebar       Build dash… Stage 3             Information Hiera…
#>  6 bslib   value_box          Display ke… Stage 2,Stage 3,St… Visual Hierarchy,…
#>  7 bslib   nav_panel          Create tab… Stage 1,Stage 3     Cognitive Load Th…
#>  8 bslib   accordion          Implement … Stage 1,Stage 3     Progressive Discl…
#>  9 bslib   input_dark_mode    Allow user… Stage 3,Stage 5     Aesthetic-Usabili…
#> 10 bslib   card_header        Add header… Stage 3             Information Hiera…
#> # ℹ 2 more variables: use_cases <chr>, relevance <dbl>

# Get shiny component suggestions
bid_suggest_components(notice_result, package = "shiny")
#> # A tibble: 10 × 7
#>    package component         description  bid_stage_relevance cognitive_concepts
#>    <chr>   <chr>             <chr>        <chr>               <chr>             
#>  1 shiny   tabsetPanel       Organize re… Stage 1,Stage 3     Information Hiera…
#>  2 shiny   conditionalPanel  Show/hide U… Stage 1,Stage 4     Cognitive Load Th…
#>  3 shiny   renderUI          Generate UI… Stage 1,Stage 4     Progressive Discl…
#>  4 shiny   uiOutput          Display dyn… Stage 1,Stage 4     Progressive Discl…
#>  5 shiny   reactive          Create reac… Stage 1,Stage 4     Processing Fluenc…
#>  6 shiny   isolate           Control rea… Stage 1,Stage 4     Processing Fluenc…
#>  7 shiny   wellPanel         Group relat… Stage 3             Principle of Prox…
#>  8 shiny   fluidRow          Create resp… Stage 3             Visual Hierarchy,…
#>  9 shiny   column            Control pre… Stage 3             Visual Hierarchy,…
#> 10 shiny   updateSelectInput Dynamically… Stage 4             Default Effect,Co…
#> # ℹ 2 more variables: use_cases <chr>, relevance <dbl>

# Get reactable component suggestions
bid_suggest_components(anticipate_result, package = "reactable")
#> # A tibble: 1 × 7
#>   package component description bid_stage_relevance cognitive_concepts use_cases
#>   <chr>   <chr>     <chr>       <chr>               <chr>              <chr>    
#> 1 reacta… getReact… Access tab… Stage 4,Stage 5     Norman's Stages o… table in…
#> # ℹ 1 more variable: relevance <dbl>

# Get echarts4r component suggestions
bid_suggest_components(validate_result, package = "echarts4r")
#> # A tibble: 0 × 7
#> # ℹ 7 variables: package <chr>, component <chr>, description <chr>,
#> #   bid_stage_relevance <chr>, cognitive_concepts <chr>, use_cases <chr>,
#> #   relevance <dbl>

# Get suggestions from all supported packages
bid_suggest_components(validate_result)
#> # A tibble: 12 × 7
#>    package component        description   bid_stage_relevance cognitive_concepts
#>    <chr>   <chr>            <chr>         <chr>               <chr>             
#>  1 shiny   tabsetPanel      Organize rel… Stage 1,Stage 3     Information Hiera…
#>  2 shiny   conditionalPanel Show/hide UI… Stage 1,Stage 4     Cognitive Load Th…
#>  3 shiny   renderUI         Generate UI … Stage 1,Stage 4     Progressive Discl…
#>  4 shiny   uiOutput         Display dyna… Stage 1,Stage 4     Progressive Discl…
#>  5 shiny   reactive         Create react… Stage 1,Stage 4     Processing Fluenc…
#>  6 shiny   isolate          Control reac… Stage 1,Stage 4     Processing Fluenc…
#>  7 bslib   nav_panel        Create tabbe… Stage 1,Stage 3     Cognitive Load Th…
#>  8 bslib   accordion        Implement co… Stage 1,Stage 3     Progressive Discl…
#>  9 shiny   wellPanel        Group relate… Stage 3             Principle of Prox…
#> 10 bslib   card             Organize con… Stage 3,Stage 5     Principle of Prox…
#> 11 bslib   layout_columns   Create flexi… Stage 3             Visual Hierarchy,…
#> 12 DT      datatable        Display data… Stage 2,Stage 3,St… Information Hiera…
#> # ℹ 2 more variables: use_cases <chr>, relevance <dbl>
```

## Comprehensive Reporting

Generate documentation for your BID implementation:

``` r
# Generate a report in various formats
text_report <- bid_report(validate_result)
html_report <- bid_report(validate_result, format = "html")
md_report <- bid_report(validate_result, format = "markdown", include_diagrams = TRUE)
```

## Example Workflow

``` r
library(shiny)
library(bidux)

# Document the entire BID process
bid_process <- bid_notice(
  problem = "Users can't find the most important metrics",
  evidence = "User testing showed 70% of users spending >30s looking for key metrics"
) |>
  bid_interpret(
    central_question = "How are sales trending against targets?",
    data_story = list(
      hook = "Sales are trending below target in Q2",
      context = "Previous quarters exceeded targets",
      tension = "What's causing the Q2 decline?",
      resolution = "Identify underperforming product categories"
    )
  ) |>
  bid_structure(
    layout = "dual_process",
    concepts = c("principle_of_proximity", "default_effect")
  ) |>
  bid_anticipate(
    bias_mitigations = list(
      anchoring = "Using context-aware reference points",
      framing = "Providing both positive and negative framings"
    )
  ) |>
  bid_validate(
    summary_panel = "Key insights summary",
    collaboration = "Team annotation features"
  )
#> Auto-suggested theory: Cognitive Load Theory (confidence: 90%)
#> Stage 1 (Notice) completed. (20% complete)
#>   - Problem: Users can't find the most important metrics
#>   - Theory: Cognitive Load Theory (auto-suggested)
#>   - Evidence: User testing showed 70% of users spending >30s looking fo...
#>   - Theory confidence: 90%
#>   - Next: Use bid_interpret() for Stage 2 
#> Stage 2 (Interpret) completed.
#>   - Central question: How are sales trending against targets?
#>   - Your data story has all key elements. Focus on making each component compelling and relevant.
#>   - Your central question is appropriately scoped.
#>   - No user personas defined
#> Stage 3 (Structure) completed.
#>   - Layout: dual_process
#>   - Concepts: Principle of Proximity, Default Effect
#>   - No accessibility considerations specified
#> Stage 4 (Anticipate) completed.
#>   - Bias mitigations: 2 defined
#>   - Interaction principles: 2 defined
#>   - Key suggestions: anchoring mitigation: Always show reference points like previous period, budget, or industry average., framing mitigation: Toggle between progress (65% complete) and gap (35% remaining) framing., Consider also addressing these common biases: confirmation
#> Stage 5 (Validate) completed.
#>   - Summary panel: Key insights summary
#>   - Collaboration: Team annotation features
#>   - Next steps: 5 items defined
#>   - Validation stage is well-defined. Focus on implementation and user testing.
bid_process
#> # A tibble: 1 × 14
#>   stage    summary_panel        collaboration           next_steps previous_bias
#>   <chr>    <chr>                <chr>                   <chr>      <chr>        
#> 1 Validate Key insights summary Team annotation featur… Conduct u… anchoring: U…
#> # ℹ 9 more variables: previous_interaction <json>, previous_layout <chr>,
#> #   previous_concepts <chr>, previous_accessibility <chr>,
#> #   previous_central_question <chr>, previous_problem <chr>,
#> #   previous_theory <chr>, suggestions <chr>, timestamp <dttm>

# Generate implementation suggestions
bid_suggest_components(bid_process, "bslib")
#> # A tibble: 4 × 7
#>   package component description bid_stage_relevance cognitive_concepts use_cases
#>   <chr>   <chr>     <chr>       <chr>               <chr>              <chr>    
#> 1 bslib   nav_panel Create tab… Stage 1,Stage 3     Cognitive Load Th… content …
#> 2 bslib   accordion Implement … Stage 1,Stage 3     Progressive Discl… FAQ sect…
#> 3 bslib   card      Organize c… Stage 3,Stage 5     Principle of Prox… Content …
#> 4 bslib   layout_c… Create fle… Stage 3             Visual Hierarchy,… precise …
#> # ℹ 1 more variable: relevance <dbl>

# Create a report
report <- bid_report(bid_process, format = "html")
```

## Learn More

Check out the vignettes for more information:

- `vignette("introduction-to-bid")` - Overview of the BID framework
- `vignette("getting-started")` - Quick start guide with implementation
  examples
- `vignette("concepts-reference")` - Detailed guide to implementing key
  concepts

Visit the [package website](https://jrwinget.github.io/bidux/) for
complete documentation and examples.
