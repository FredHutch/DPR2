# Construct path from data package directory (deprecated)

This function is a compatibility wrapper for legacy code and may be
removed in the future. Please use
[`dpr_path()`](https://fredhutch.github.io/DPR2/reference/dpr_path.md)
instead.

## Usage

``` r
project_path(...)
```

## Arguments

- ...:

  Trailing path components passed to
  [`file.path()`](https://rdrr.io/r/base/file.path.html). All arguments
  must be the same length or length one.

## Value

The normalized path with the additional path components appended. Throws
an error if no root is found.
