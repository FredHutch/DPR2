# dpr_render

Process and render all processing scripts defined in the
`datapackager.yml` configuration file. Does not build or install the
data package. For full package build and installation, use the
`dpr_build` function.

## Usage

``` r
dpr_render(path = ".", ...)
```

## Arguments

- path:

  The relative path to the data package. The default is the working
  directory.

- ...:

  `datapackager.yml` value overrides. When arguments are specified,
  those arguments are used as the YAML key value pairs instead of what
  is specified by the `datapackager.yml` file. For a list of those key
  value pairs and their purposes, see
  [`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md).

## Value

does not return anything but performs rendering, processing and
data-saving operations as defined in `datapackager.yml`

## Details

`dpr_render` is the primary process that renders processing scripts to
vignettes and data. This function can be run in two modes: isolate or
share. This mode is set in the `datapackager.yml` file's
`render_env_mode`'s value. When the `isolate` mode is set, each
processing script is run in a separate R session. When the `share` mode
is set, each process is run in the same session, which enables each
process to use variables defined by previous processing scripts for the
current `dpr_render` call.

## Author

jmtaylor
