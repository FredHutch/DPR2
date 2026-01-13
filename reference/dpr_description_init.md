# dpr_description_init

A function that provides a convenient way to initialize a package's
`DESCRIPTION` file by merging default settings with user specific values
to the `DESCRIPTION` file's key:value pairs. If no arguments are
provided, it will return the default values. See
[`dpr_description_defaults()`](https://fredhutch.github.io/DPR2/reference/dpr_description_defaults.md)
for a list of default values, or call this function with no arguments.

## Usage

``` r
dpr_description_init(...)
```

## Arguments

- ...:

  Arguments are treated as key value pairs for the package `DESCRIPTION`
  file.

## Value

A list of key:value pairs for generating a `DESCRIPTION` file.

## Author

jmtaylor
