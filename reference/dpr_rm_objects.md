# dpr_rm_objects

Remove objects set to be built by DPR2.

## Usage

``` r
dpr_rm_objects(objects, path = ".")
```

## Arguments

- objects:

  a character vector of object names to be built, adding them to the
  data directory after a processing script is rendered.

- path:

  path to a DPR2 data package

## Details

`dpr_add_objects` is mostly provided for backwards compatibility with
DataPackageR. It is recommended to use `dpr_save` in the processing
scripts directly to save files to the data directory rather than using
`dpr_add_objects`.
