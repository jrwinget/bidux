# Set global quiet mode for bidux functions

Convenience function to set the global quiet option for all bidux
functions. When quiet mode is enabled, most informational messages are
suppressed.

## Usage

``` r
bid_set_quiet(quiet = TRUE)
```

## Arguments

- quiet:

  Logical indicating whether to enable quiet mode. When TRUE, most bidux
  messages are suppressed.

## Value

The previous value of the quiet option (invisibly)

## Examples

``` r
# Enable quiet mode
bid_set_quiet(TRUE)

# Disable quiet mode
bid_set_quiet(FALSE)
```
