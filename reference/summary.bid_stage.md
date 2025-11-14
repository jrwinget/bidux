# Summary method for BID stage objects

Summary method for BID stage objects

## Usage

``` r
# S3 method for class 'bid_stage'
summary(object, ...)
```

## Arguments

- object:

  A bid_stage object

- ...:

  Additional arguments

## Value

Returns the input `bid_stage` object invisibly (class:
`c("bid_stage", "tbl_df", "tbl", "data.frame")`). The method is called
for its side effects: printing a comprehensive summary to the console
including stage metadata, all non-empty data columns, and timestamp
information. The invisible return enables method chaining while
prioritizing the detailed console output display.
