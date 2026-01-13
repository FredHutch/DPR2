# dpr_add_objects

Add objects to be built by DPR2.

## Usage

``` r
dpr_add_objects(objects, path = ".")
```

## Arguments

- objects:

  the names of objects defined in processing scripts set to build.

- path:

  path to a DPR2 data package

## Details

`dpr_add_objects` is mostly provided for backwards compatibility with
DataPackageR. It is recommended to use `dpr_save` in the processing
scripts directly to save files to the data directory rather than using
`dpr_add_objects`.
