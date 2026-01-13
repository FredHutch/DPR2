# dpr_scripts

Return a data frame of scripts currently set to build.

## Usage

``` r
dpr_scripts(path = ".")
```

## Arguments

- path:

  path to a DPR2 data package

## Value

a data frame displaying script names in the processing directory, and if
they are set to build or not

## Details

The data frame returned by `dpr_scripts` displays what processing
scripts are found in the processing directory (see, ?dpr_yaml_get) and
which of those will be rendered by calling `dpr_render` or `dpr_build`.
