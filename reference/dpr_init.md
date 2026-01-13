# dpr_init

A wrapper for `dpr_create` that uses the parent directory as the
location to create the data package, setting the package name as the
current directory.

## Usage

``` r
dpr_init(
  path = ".",
  yaml = dpr_yaml_init(),
  desc = dpr_description_init(Package = basename(path))
)
```

## Arguments

- path:

  a path value to init at a specific path. The default path is the
  current working directory.

- yaml:

  A returned list from
  [`dpr_yaml_init()`](https://fredhutch.github.io/DPR2/reference/dpr_yaml_init.md)

- desc:

  A returned list from
  [`dpr_description_init()`](https://fredhutch.github.io/DPR2/reference/dpr_description_init.md).
  The default argument sets the package name to the name of the
  directory containing the data package.

## Author

jmtaylor
