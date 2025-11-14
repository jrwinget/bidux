# Print method for BID result objects

Print method for BID result objects

## Usage

``` r
# S3 method for class 'bid_result'
print(x, ...)
```

## Arguments

- x:

  A bid_result object

- ...:

  Additional arguments

## Value

Returns the input `bid_result` object invisibly (class:
`c("bid_result", "list")`). The method is called for its side effects:
printing a workflow overview to the console showing completion status,
stage progression, and key information from each completed BID stage.
The invisible return supports method chaining while emphasizing the
console summary output.
