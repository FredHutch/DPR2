# dpr_data_digest

Return the current hashes recorded to the data digest directory
generated during the last build.

## Usage

``` r
dpr_data_digest(path = ".")
```

## Arguments

- path:

  path to data package

## Value

a data.frame with the names of the data files and the the corresponding
hash values extracted from the data digest
