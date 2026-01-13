# DataPackageR Compatibility

## Converting a DataPackageR data package to use DPR2

Sometimes users may encounter datapackages created by DataPackageR. DPR2
has the function `dpr_convert` which converts a DataPackageR package
into a DPR2 package. In this example we will copy our DataPackageR test
package to a temp directory to demonstrate the DPR2 conversion function.

To convert a DataPackageR package to use DPR2 simply run
`dpr_convert(path)`, setting the path to the DataPackageR package, with
the default path being the current working directory.

``` r
## load DPR2 
library(DPR2)

## convert DataPackageR package to the DPR2 format 
dpr_convert(dpr1_path)
#> Warning in dpr1_data_digest_convert(path): Object `mtcars_mod` in data
#> directory does not match md5 not found in DATADIGEST.
```

This conversion does data digest validation, where in the above example,
one of the tracked files has a different checksum than what is reported
in the digest.

Converting a package using
[`dpr_convert()`](https://fredhutch.github.io/DPR2/reference/dpr_convert.md)
deletes several files created by DataPackageR that DPR2 does not use:

- `DATADIGEST`
- `NEWS.md`
- `Read-and-delete-me`,
- `R/documentation.R`
- `data-raw/documentation.R`,
- `inst/extdata/Logfiles`,
- any file in `inst/doc` with the same name as an `R` or `Rmd` file
  found in the `data-raw` or`Logfiles`

[`dpr_convert()`](https://fredhutch.github.io/DPR2/reference/dpr_convert.md)
also modifies the package’s DataPackageR `DESCRIPTION` file, removing
its `Date`, and `DataVersion` fields.

Although the `DATADIGEST` file is deleted, DPR2 does maintain that
information, but uses a directory found in `inst` to do so. DPR2
provides the `dpr_data_digest` function for assembling the contents of
the new data digest format into a tabular object. See [Data
Versioning](https://fredhutch.github.io/DPR2/articles/data_versioning.md)
for more information regarding how to use the new data digest format.

``` r
## view the new data digest
dpr_data_digest(dpr1_path)
#>             name                  data_digest_md5
#> 1       iris.rda d3c5d071001b61a9f6131d3004fd0988
#> 2 mtcars_mod.rda 155cae107fc1e8b0ef6d0c4819a3c6a1
#> 3      trees.rda 0c2f6ee08f10e6377f07512045a606d3
```

`dpr_convert` also translates the orginal `DataPackageR` yaml file into
a DPR2 compatible format. Examining the `datapackager.yml` file, we see
the yaml configuration for the DataPackageR package is converted to DPR2
compatible settings.

``` r
## view the new yaml file
dpr_yaml_get(dpr1_path)
#> $source_data_directory
#> [1] "inst/extdata"
#> 
#> $purge_data_directory
#> [1] FALSE
#> 
#> $process_directory
#> [1] "data-raw"
#> 
#> $render_on_build
#> [1] TRUE
#> 
#> $render_env_mode
#> [1] "isolate"
#> 
#> $write_to_vignettes
#> [1] TRUE
#> 
#> $write_data_docs
#> [1] TRUE
#> 
#> $to_build_directory
#> [1] "inst/to_build"
#> 
#> $build_tarball
#> [1] FALSE
#> 
#> $install_on_build
#> [1] FALSE
#> 
#> $build_output
#> [1] "../"
#> 
#> $objects
#> [1] "iris"       "mtcars_mod"
#> 
#> $process_on_build
#> [1] "iris.R"   "mtcars.R"
#> 
#> $r_session_wait_timeout
#> [1] 3000
```

DataPackageR used the `data-raw` directory to store data processing
scripts. DPR2 uses a new `processing` directory to store those; however,
this is configurable in the DPR2 `datapackager.yml` file, and thus
converted data packages have this set to the default DataPackageR
`data-raw` path.

DPR2 maintains the DataPackageR functions `project_path`,
`project_data_path` and `project_extdata_path` to ensure compatibility
with scripts written for DataPackageR, but they are not recommended to
use for new scripts as these functions may be removed from future
versions of DPR2. Many DataPackageR packages may use these, so these
have been maintained to ensure minimal changes are required to those
processing scripts to function in a DPR2 package.
