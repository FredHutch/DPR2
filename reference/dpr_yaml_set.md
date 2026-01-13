# dpr_yaml_set

Write new `datapackager.yml` with new or modified key:value pairs.

## Usage

``` r
dpr_yaml_set(path = ".", ...)
```

## Arguments

- path:

  The full path to the data package. The default is the working
  directory.

- ...:

  `datapackager.yml` value overrides. When arguments are specified,
  those arguments are used as the YAML key value pairs instead of what
  is specified by the `datapackager.yml`. Note, any values can be set
  here, but only those returned by
  [`dpr_yaml_defaults()`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md)
  are used by DPR2. See
  [`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md)
  for information on those values.

## Value

nothing
