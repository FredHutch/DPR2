# Construct path from data package directory.

Construct path from data package directory.

## Usage

``` r
dpr_path(...)
```

## Arguments

- ...:

  Trailing path components passed to
  [`file.path()`](https://rdrr.io/r/base/file.path.html). All arguments
  must be the same length or length one.

## Value

The normalized path with the additional path components appended. Throws
an error if no root is found.
