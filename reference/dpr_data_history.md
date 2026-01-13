# dpr_data_history

Return the history of all the former and current files in the `data`
directory.

## Usage

``` r
dpr_data_history(path = ".", include_checksums = FALSE)
```

## Arguments

- path:

  path to data package

- include_checksums:

  a boolean value indicating if checksums should be included in the
  returned data.frame object; computing checksums is less performant

## Value

a data.frame object with the git hash, file name, author name, time of
creation and md5 checksum of the file
