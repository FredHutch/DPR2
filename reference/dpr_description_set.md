# dpr_description_set

Write new `DESCRIPTION` file with new or modified key:value pairs.

## Usage

``` r
dpr_description_set(path = ".", ...)
```

## Arguments

- path:

  The full path to the data package. The default is the working
  directory.

- ...:

  datapackager.yml value overrides. When arguments are specified, those
  arguments are used as the YAML key value pairs instead of what is
  specified by the `datapackager.yml`. See
  [`?dpr_description_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_description_defaults.md)
  for the default values that can be set.

## Value

nothing
