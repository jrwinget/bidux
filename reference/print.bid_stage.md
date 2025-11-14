# Print method for BID stage objects

Print method for BID stage objects

## Usage

``` r
# S3 method for class 'bid_stage'
print(x, ...)
```

## Arguments

- x:

  A bid_stage object

- ...:

  Additional arguments

## Value

Returns the input `bid_stage` object invisibly (class:
`c("bid_stage", "tbl_df", "tbl", "data.frame")`). The method is called
for its side effects: printing a formatted summary of the BID stage to
the console, including stage progress, key stage-specific information,
and usage suggestions. The invisible return allows for method chaining
while maintaining the primary purpose of console output.
