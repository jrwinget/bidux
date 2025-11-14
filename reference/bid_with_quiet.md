# Temporarily suppress bidux messages

Execute code with bidux messages temporarily suppressed.

## Usage

``` r
bid_with_quiet(code)
```

## Arguments

- code:

  Code to execute with messages suppressed

## Value

The result of evaluating code

## Examples

``` r
# Run analysis quietly without changing global setting
result <- bid_with_quiet({
  bid_interpret(
    central_question = "How can we improve user engagement?",
    data_story = list(hook = "Users are leaving", resolution = "Fix issues")
  )
})
#> Warning: ! Using deprecated list format for data_story parameter
#> ℹ Please use new_data_story() constructor for new code
#> ℹ Legacy format will be automatically migrated
#> Warning: ! Using deprecated nested format for data_story
#> ℹ The flat API is now recommended: new_data_story(hook, context, tension,
#>   resolution)
#> ℹ Nested format (variables, relationships) will be removed in bidux 0.4.0
```
