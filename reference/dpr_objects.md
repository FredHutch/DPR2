# dpr_objects

Return a data frame of objects currently set to build.

## Usage

``` r
dpr_objects(path = ".")
```

## Arguments

- path:

  path to a DPR2 data package

## Value

a data frame displaying object names in the data directory, and if they
are set to build or not

## Details

The data frame returned by `dpr_objects` displays what objects will be
saved from the processing environments to the `data` directory and
reports what objects are set to be built by DPR2. Objects are added to
this list by using `dpr_add_objects`. `dpr_add_objects` is mostly
provided for backwards compatibility with DataPackageR. It is
recommended to use `dpr_save` in the processing scripts directly to save
files to the data directory rather than using `dpr_add_objects`.
