# dpr_build

Build the data package. This includes processing and rendering all
processing scripts defined in the `datapackager.yml` configuration file
and additionally will build and install the data package if configured
to do so in the `datapackager.yml`.

## Usage

``` r
dpr_build(path = ".", ...)
```

## Arguments

- path:

  The relative path to the data package. The default is the working
  directory.

- ...:

  `datapakager.yml` value overrides. When arguments are specified, those
  arguments are used as the YAML key value pairs instead of what is
  specified by the `datapackager.yml` file. For a list of those key
  value pairs and their purposes, see
  [`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md).

## Value

does not return anything but performs rendering, processing and
data-saving operations as defined in datapackager.yml\`

## Details

`dpr_build` wraps many DPR2 processes in a single call: renders
processing scripts, updates data digest, builds package to an
installable tarball, and installs the tarball. Each of these processes
can be controlled from the `datapackager.yml` file. Only the processing
script rendering function is exported to users. See `dpr_render` for
more information regarding rendering. For more information regarding
configuration options, see
[`?dpr_yaml_defaults`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_defaults.md).

## Author

jmtaylor
