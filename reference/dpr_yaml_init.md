# dpr_yaml_init

A function that provides a convenient way to initialize a YAML
configuration by merging default settings with user specific values to
the `datapackager.yml` file's key:value pairs. If no arguments are
provided, it will return the `datapackager.yml` file default values. See
[`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md)
for a list of default values, or call this function with no arguments.

## Usage

``` r
dpr_yaml_init(...)
```

## Arguments

- ...:

  Arguments are treated as key value pairs for the `datapackager.yml`
  file.

## Value

A list of key:value pairs for generating a `datapackager.yml` file.
