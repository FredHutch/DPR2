# dpr_compare_data_digest

A data digest comparison table, comparing the current data digest
checksums with the current file checksums in the data directory.

## Usage

``` r
dpr_compare_data_digest(path = ".")
```

## Arguments

- path:

  path to data package

## Value

a data.frame with the file name, corresponding data in-memory checksum,
existing data_digest checksum, and a boolean value indicating if they
are same or not

## Details

Compare the current checksums for the data listed in the data directory
with the checksums listed in the data digest. The data digest is only
updated when the package is built, not when it is simply rendered using
[`dpr_render()`](https://fredhutch.github.io/DPR2/reference/dpr_render.md).

## Author

jmtaylor
