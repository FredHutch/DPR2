# dpr_add_scripts

Add processing scripts to list of scripts rendered when a package is
rendered or built.

## Usage

``` r
dpr_add_scripts(scripts, path = ".")
```

## Arguments

- scripts:

  the names of the scripts in the processing directory described in the
  datapackager.yml to be built when running `dpr_build`. scripts
  currently set to build.

- path:

  path to a data package

## Details

This function will add new processing scripts to be rendered. To remove
processing scripts, see
[`?dpr_rm_scripts`](https://fredhutch.github.io/DPR2/reference/dpr_rm_scripts.md).
To generate a table of scripts set to build, see `?dpr_processing`.
