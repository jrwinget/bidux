# Quick UX Suggestions for Shiny Developers

Provides a streamlined, single-step workflow for Shiny developers who
need quick UX suggestions without going through the full 5-stage BID
framework. This function internally leverages the BID framework stages
but presents results in a simple, actionable format.

Unlike the full BID workflow (Interpret -\> Notice -\> Anticipate -\>
Structure -\> Validate), this function provides immediate suggestions
based on a problem description. Use this for rapid prototyping or when
you need quick guidance. For comprehensive UX redesign projects, use the
full BID workflow.

## Usage

``` r
bid_quick_suggest(
  problem,
  context = NULL,
  package = NULL,
  limit = 10,
  min_score = 0.7,
  quiet = NULL
)
```

## Arguments

- problem:

  Required. A character string describing the UX problem. Examples:
  "Users can't find the download button", "Information overload on
  dashboard", "Mobile interface is hard to navigate".

- context:

  Optional. Additional context about the application or users. This
  helps refine suggestions to your specific situation.

- package:

  Optional. Filter suggestions to specific package ("bslib", "shiny",
  "reactable", "DT", etc.). If NULL, returns suggestions for all
  relevant packages.

- limit:

  Optional. Maximum number of suggestions to return (default: 10). Set
  to Inf to return all suggestions.

- min_score:

  Optional. Minimum relevance score 0-1 (default: 0.7). Higher values
  return only the most relevant suggestions.

- quiet:

  Optional. Logical indicating whether to suppress informational
  messages. If NULL, uses getOption("bidux.quiet", FALSE).

## Value

A tibble with columns:

- title:

  Brief actionable description of the suggestion

- details:

  Specific implementation guidance

- components:

  Shiny/bslib component recommendations (character vector)

- concept:

  UX concept the suggestion is based on

- score:

  Relevance score (0-1, higher is more relevant)

- difficulty:

  Implementation difficulty (easy/moderate/advanced)

- rationale:

  1-2 sentence explanation of why this helps

## Details

**How it works:**

The function analyzes your problem description using keyword matching
and semantic analysis to:

1.  Identify relevant UX concepts (cognitive load, navigation, visual
    hierarchy, etc.)

2.  Detect appropriate layout patterns (grid, card, breathable, etc.)

3.  Generate ranked suggestions with specific component recommendations

4.  Filter and sort by relevance score

**Problem Analysis Keywords:**

- "overload", "overwhelm", "too many" -\> Cognitive Load Theory

- "find", "search", "navigate" -\> Information Scent

- "cluttered", "messy", "disorganized" -\> Visual Hierarchy

- "mobile", "touch", "responsive" -\> Fitts's Law

- "confusing", "unclear", "complex" -\> Progressive Disclosure

**When to use this vs full BID workflow:**

- Use `bid_quick_suggest()`: Quick fixes, prototyping, single issues

- Use full workflow: Comprehensive redesigns, complex projects, team
  collaboration

## Examples

``` r
# Basic usage
suggestions <- bid_quick_suggest(
  problem = "Users can't find the download button"
)
#> ℹ Analyzing your UX problem
#> ℹ Detected 2 relevant UX concepts
#> ℹ Suggested layout pattern: breathable
#> ℹ Generating actionable suggestions
#> Quick suggestions ready
#>   - Found 9 suggestions (avg relevance: 0.89)
#>   - Top concept: Cognitive Load Theory
#>   - Use bid_concept() to learn more about any concept 
print(suggestions)
#> # A tibble: 9 × 7
#>   title                    details components concept score difficulty rationale
#>   <chr>                    <chr>   <list>     <chr>   <dbl> <chr>      <chr>    
#> 1 Limit initial choices    Show o… <chr [3]>  Cognit…  1    moderate   Reduces …
#> 2 Use progressive complex… Start … <chr [3]>  Cognit…  0.96 easy       Prevents…
#> 3 Provide smart defaults   Pre-se… <chr [3]>  Cognit…  0.93 moderate   Leverage…
#> 4 Use descriptive labels … Make n… <chr [3]>  Inform…  0.92 easy       Strong i…
#> 5 Establish clear informa… Use si… <chr [4]>  Visual…  0.9  easy       Helps us…
#> 6 Use collapsible advance… Place … <chr [2]>  Progre…  0.88 moderate   Reveals …
#> 7 Group related content v… Use co… <chr [3]>  Visual…  0.87 moderate   Leverage…
#> 8 Implement drill-down na… Allow … <chr [3]>  Progre…  0.87 easy       Matches …
#> 9 Apply Fitts's Law princ… Consid… <chr [3]>  Fitts'…  0.7  moderate   Systemat…

# With additional context
suggestions <- bid_quick_suggest(
  problem = "Dashboard has too many charts and metrics",
  context = "Financial analysts need quick insights but get overwhelmed",
  limit = 5
)
#> ℹ Analyzing your UX problem
#> ℹ Detected 1 relevant UX concepts
#> ℹ Suggested layout pattern: breathable
#> ℹ Generating actionable suggestions
#> Quick suggestions ready
#>   - Found 5 suggestions (avg relevance: 0.95)
#>   - Top concept: Cognitive Load Theory
#>   - Use bid_concept() to learn more about any concept 

# Filter to specific package
bslib_suggestions <- bid_quick_suggest(
  problem = "Mobile interface is hard to use",
  package = "bslib",
  min_score = 0.8
)
#> ℹ Analyzing your UX problem
#> ℹ Detected 1 relevant UX concepts
#> ℹ Suggested layout pattern: breathable
#> ℹ Generating actionable suggestions
#> Quick suggestions ready
#>   - Found 6 suggestions for package 'bslib' (avg relevance: 0.91)
#>   - Top concept: Cognitive Load Theory
#>   - Use bid_concept() to learn more about any concept 

# Navigation issues
nav_suggestions <- bid_quick_suggest(
  problem = "Users get lost in multi-tab interface",
  context = "Application has 10+ tabs with nested content"
)
#> ℹ Analyzing your UX problem
#> ℹ Detected 1 relevant UX concepts
#> ℹ Suggested layout pattern: tabs
#> ℹ Generating actionable suggestions
#> Quick suggestions ready
#>   - Found 8 suggestions (avg relevance: 0.9)
#>   - Top concept: Cognitive Load Theory
#>   - Use bid_concept() to learn more about any concept 

# Information overload
overload_suggestions <- bid_quick_suggest(
  problem = "Too many filters and options on the sidebar",
  context = "Beginners find the interface overwhelming"
)
#> ℹ Analyzing your UX problem
#> ℹ Detected 4 relevant UX concepts
#> ℹ Suggested layout pattern: breathable
#> ℹ Generating actionable suggestions
#> Quick suggestions ready
#>   - Found 10 suggestions (avg relevance: 0.9)
#>   - Top concept: Cognitive Load Theory
#>   - Use bid_concept() to learn more about any concept 
```
