# dpr_yaml_get

Get the current `datapackager.yml` key:value pairs and temporarily add
to or modify those with with new values. All additions and modifications
happen only in memory, not on disc. To set on disc, see
[`?dpr_yaml_set`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_set.md).

## Usage

``` r
dpr_yaml_get(path = ".", ...)
```

## Arguments

- path:

  The full path to the data package. The default is the working
  directory.

- ...:

  `datapackager.yml` value overrides. When arguments are specified,
  those arguments are used as the YAML key value pairs instead of what
  is specified by the `datapackager.yml` file. For a list of those key
  value pairs and their purposes, see
  [`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md).

## Value

list

## Author

jmtaylor
