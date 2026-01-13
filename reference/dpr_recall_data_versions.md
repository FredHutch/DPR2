# dpr_recall_data_version

Load a data version into memory by its hash. Only RDA files in the data
directory will be recalled.

## Usage

``` r
dpr_recall_data_versions(hashes, path = ".")
```

## Arguments

- hashes:

  the git hashes of the data to recall; partial hashes allowed from 1 to
  40 hexadecimal digits

- path:

  the path to the data package

## Value

a list with names of corresponding .rda object and the versions
generated from the object
