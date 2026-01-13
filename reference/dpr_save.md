# dpr_save

`dpr_save` is vectorized so users may pass a character vector of object
names found in the calling environment to save to the `data` directory
when the package is built. All objects are saved as single object `Rda`
files by the object names that are passed.

## Usage

``` r
dpr_save(
  objects,
  path = dpr_path(),
  envir = parent.frame(),
  ascii = FALSE,
  compress = !ascii
)
```

## Arguments

- objects:

  Character vector of object names to be saved from the environment
  specified in `envir`.

- path:

  The path to the data package. Defaults to
  [`dpr_path()`](https://fredhutch.github.io/DPR2/reference/dpr_path.md).

- envir:

  The environment to search for objects to save. Defaults to calling
  environment.

- ascii:

  Argument passed to [`save()`](https://rdrr.io/r/base/save.html).
  Modifying this default is for advanced use only.

- compress:

  Argument passed to [`save()`](https://rdrr.io/r/base/save.html).
  Modifying this default is for advanced use only.

## Value

The original `objects` argument, invisibly.

## Details

A convenience function for writing data objects to the package data
directory.
