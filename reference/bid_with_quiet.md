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
    data_story = new_data_story(
      hook = "Users are leaving",
      context = "User engagement declining",
      resolution = "Fix issues"
    )
  )
})
```
