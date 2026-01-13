# dpr_convert

Convert a repository from DataPackageR to DPR2.

## Usage

``` r
dpr_convert(path = ".")
```

## Arguments

- path:

  a path value to init at a specific path, when using the default the
  current working directory is used.

## Details

This is a mildly destructive process which converts, adds, and removes
files to take information from a DataPackageR repository and put it in
the DPR2 format. This function also removes those files that are no
longer needed from DataPackageR by DPR2. This includes files
`DATADIGEST`, `NEWS.md`, `Read-and-delete-me`, `R/documentation.R`, and
`data-raw/documentation.R`, in addition to the folder
`inst/extdata/Logfiles`, and any file in `inst/doc` with the same name
as an `R` or `Rmd` file found in the `data-raw` or`Logfiles` folders.
Modifications to the package `DESCRIPTION` file are also made, removing
its `Date`, and `DataVersion` fields.
