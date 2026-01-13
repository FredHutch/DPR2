# dpr_create

Initialize a data package by creating a structured directory skeleton
for a new R package. Sets up essential directories, initializes package
metadata in the `datapackager.yml` and `DESCRIPTION` files as described
by
[`dpr_yaml_init()`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_init.md)
and
[`dpr_description_init()`](https://fredhutch.github.io/DPR2/reference/dpr_description_init.md)
function calls.

## Usage

``` r
dpr_create(path = ".", yaml = dpr_yaml_init(), desc = dpr_description_init())
```

## Arguments

- path:

  A path to the data package.

- yaml:

  A returned list for
  [`dpr_yaml_init()`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_init.md)

- desc:

  A returned list for
  [`dpr_description_init()`](https://fredhutch.github.io/DPR2/reference/dpr_description_init.md)
