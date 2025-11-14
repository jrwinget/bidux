# Summary method for BID result objects

Summary method for BID result objects

## Usage

``` r
# S3 method for class 'bid_result'
summary(object, ...)
```

## Arguments

- object:

  A bid_result object

- ...:

  Additional arguments

## Value

Returns the input `bid_result` object invisibly (class:
`c("bid_result", "list")`). The method is called for its side effects:
printing a detailed workflow analysis to the console including
completion statistics, duration metrics, and comprehensive
stage-by-stage breakdowns with key data from each BID framework stage.
The invisible return facilitates method chaining while focusing on
comprehensive console reporting.
