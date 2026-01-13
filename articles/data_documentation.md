# Data Documentation

## Introduction

Clear documentation of data objects allows users to quickly understand
the structure, variables, and intended purpose of each dataset without
having to inspect the raw contents. This is especially important in
packages where data evolves over time or supports multiple analysis
pipelines. Well-documented data reduces onboarding time, minimizes
errors, and promotes consistent use and interpretation across workflows.

DPR2 offers features that automatically generate documentation for
`.rda` data objects stored in a package’s `data/` folder. This includes
creating `.R` files for each object under `R/`, as well as `.Rd` help
files in `man/` using `roxygen2`. Descriptive details are added based on
the object’s structure. For example, the number of rows and columns are
shown for data frames or matrices, and the length is shown for vectors
and lists.

Below, we explain how this works, demonstrate its behavior on example
objects, and cover important edge cases. Let’s begin by showing a
directory skeleton of an example DPR2 package `TestPKG` prior to
rendering to showcase the absence of the `man/` and `R/` folders.

``` bash
TestPKG
├── data
├── datapackager.yml
├── DESCRIPTION
├── inst
│   ├── data_digest
│   ├── extdata
│   └── to_build
│       ├── objects
│       └── scripts
│           └── mtcars.R_
├── NAMESPACE
└── processing
    └── mtcars.R
```

We now walk through an example using the built-in `mtcars` dataset. This
will show how DPR2 documents both a data frame and a structured list
object.

## Automated, simple documentation with DPR2

DPR2 automatically generates documentation for each `.rda` object in the
`data/` folder upon calling
[`dpr_render()`](https://fredhutch.github.io/DPR2/reference/dpr_render.md).
The `.R` and `.Rd` files are created by examining the object’s
structure, including its class, dimensions, and element names. Field
descriptions are inferred from column names in data frames or element
names in lists.

DPR2 leverages
[`dpr_compare_data_digest()`](https://fredhutch.github.io/DPR2/reference/dpr_compare_data_digest.md)
to compare the checksums of objects in the `data/` directory with their
recorded digests. This allows DPR2 to update `.R` documentation files
only when changes are detected in the underlying data. **By default,
DPR2 will overwrite `.R` and `.Rd` files if the corresponding data
object has changed, as determined by its checksum. If the object has not
changed, DPR2 will skip re-documentation.**

Below we walk through an example showcasing DPR2’s default documentation
behavior. We define and save two objects: an `mtcars_df` data frame that
has an added `efficiency` column, and a `mtcars_list` list containing
the raw data, summary statistics, column classes, and a correlation
matrix. These are saved to the `data/` folder using
[`dpr_save()`](https://fredhutch.github.io/DPR2/reference/dpr_save.md).

    library(DPR2)

    data('mtcars')
    mtcars_df <- mtcars
    mtcars_df$efficiency <- mtcars$mpg / mtcars$hp

    mtcars_list <- list(
      raw_data = mtcars,
      summary = summary(mtcars),
      column_classes = sapply(mtcars, class),
      correlation_matrix = cor(mtcars)
      )
    dpr_save('mtcars_df')
    dpr_save('mtcars_list')

``` r
str(mtcars_df)
#> 'data.frame':    32 obs. of  12 variables:
#>  $ mpg       : num  21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  $ cyl       : num  6 6 4 6 8 6 8 4 4 6 ...
#>  $ disp      : num  160 160 108 258 360 ...
#>  $ hp        : num  110 110 93 110 175 105 245 62 95 123 ...
#>  $ drat      : num  3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>  $ wt        : num  2.62 2.88 2.32 3.21 3.44 ...
#>  $ qsec      : num  16.5 17 18.6 19.4 17 ...
#>  $ vs        : num  0 0 1 1 0 1 0 1 1 1 ...
#>  $ am        : num  1 1 1 0 0 0 0 0 0 0 ...
#>  $ gear      : num  4 4 4 3 3 3 3 4 4 4 ...
#>  $ carb      : num  4 4 1 1 2 1 4 2 2 4 ...
#>  $ efficiency: num  0.191 0.191 0.245 0.195 0.107 ...
```

``` r
str(mtcars_list)
#> List of 4
#>  $ raw_data          :'data.frame':  32 obs. of  11 variables:
#>   ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>   ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
#>   ..$ disp: num [1:32] 160 160 108 258 360 ...
#>   ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>   ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>   ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
#>   ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
#>   ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>   ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
#>   ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
#>  $ summary           : 'table' chr [1:6, 1:11] "Min.   :10.40  " "1st Qu.:15.43  " "Median :19.20  " "Mean   :20.09  " ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:6] "" "" "" "" ...
#>   .. ..$ : chr [1:11] "     mpg" "     cyl" "     disp" "      hp" ...
#>  $ column_classes    : Named chr [1:11] "numeric" "numeric" "numeric" "numeric" ...
#>   ..- attr(*, "names")= chr [1:11] "mpg" "cyl" "disp" "hp" ...
#>  $ correlation_matrix: num [1:11, 1:11] 1 -0.852 -0.848 -0.776 0.681 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:11] "mpg" "cyl" "disp" "hp" ...
#>   .. ..$ : chr [1:11] "mpg" "cyl" "disp" "hp" ...
```

After calling
[`dpr_render()`](https://fredhutch.github.io/DPR2/reference/dpr_render.md),
DPR2 generates `.R` files under `R/` and `.Rd` files under `man/`.

``` bash
TestPKG
├── data
│   ├── mtcars_df.rda
│   └── mtcars_list.rda
├── man
│   ├── mtcars_df.Rd
│   └── mtcars_list.Rd
├── processing
│   └── mtcars.R
└── R
    ├── mtcars_df.R
    └── mtcars_list.R
```

Below is the content for `mtcars_df.R` and `mtcars_list.R` files.

    # This is data documentation created by DPR2. Do not delete this line.

    #' mtcars_df
    #'
    #' A detailed description of the data
    #'
    #' @format A data.frame with 32 rows and 12 columns with the following fields:
    #' \describe{
    #'   \item{mpg}{numeric}{}
    #'   \item{cyl}{numeric}{}
    #'   \item{disp}{numeric}{}
    #'   \item{hp}{numeric}{}
    #'   \item{drat}{numeric}{}
    #'   \item{wt}{numeric}{}
    #'   \item{qsec}{numeric}{}
    #'   \item{vs}{numeric}{}
    #'   \item{am}{numeric}{}
    #'   \item{gear}{numeric}{}
    #'   \item{carb}{numeric}{}
    #'   \item{efficiency}{numeric}{}
    #' }
    #' @source Generated from script _________________
    #' @seealso
    #' \link{}
    "mtcars_df"

    # This is data documentation created by DPR2. Do not delete this line.

    #' mtcars_list
    #'
    #' A detailed description of the data
    #'
    #' @format A list with 4 elements:
    #' \describe{
    #'   \item{raw_data}{data.frame}{}
    #'   \item{summary}{table}{}
    #'   \item{column_classes}{character}{}
    #'   \item{correlation_matrix}{matrix}{}
    #' }
    #' @source Generated from script _________________
    #' @seealso
    #' \link{}
    "mtcars_list"

Users can now access documentation with `?mtcars_df` or `?mtcars_list`,
just like any standard R data package.

## Advanced documentation with DPR2

Users are able to modify the `.R` file templates generated by DPR2 and
provide further details on their data objects such as context,
descriptions, and references. In the example below, we update the
`@source` tag to link the object back to its processing script.

``` r
mtcars_block <- readLines(file.path(path, "R", "mtcars_df.R"))
mtcars_block[grepl("@source", mtcars_block)] <- "#' @source Generated from script mtcars.R"
writeLines(mtcars_block, file.path(path, "R", "mtcars_df.R"))

roxygen2::roxygenize(path)
```

We now display the updated `.Rd` help file for `mtcars_df`:

    % Generated by roxygen2: do not edit by hand
    % Please edit documentation in R/mtcars_df.R
    \docType{data}
    \name{mtcars_df}
    \alias{mtcars_df}
    \title{mtcars_df}
    \format{
    A data.frame with 32 rows and 12 columns with the following fields:
    \describe{
      \item{mpg}{numeric}{}
      \item{cyl}{numeric}{}
      \item{disp}{numeric}{}
      \item{hp}{numeric}{}
      \item{drat}{numeric}{}
      \item{wt}{numeric}{}
      \item{qsec}{numeric}{}
      \item{vs}{numeric}{}
      \item{am}{numeric}{}
      \item{gear}{numeric}{}
      \item{carb}{numeric}{}
      \item{efficiency}{numeric}{}
    }
    }
    \source{
    Generated from script mtcars.R
    }
    \usage{
    mtcars_df
    }
    \description{
    A detailed description of the data
    }
    \seealso{
    \link{}
    }
    \keyword{datasets}

Since existing documentation files are overwritten when a checksum
change is detected, users are encouraged to separately save any custom
edits so they can be copied into the updated version if needed.

### Controlling what gets documented

As noted earlier, DPR2 uses
[`dpr_compare_data_digest()`](https://fredhutch.github.io/DPR2/reference/dpr_compare_data_digest.md)
to track changes to objects in the `data/` directory and determine how
documentation should be handled:

- If an existing `.rda` file has changed, the corresponding `.R` and
  `.Rd` files are automatically overwritten.
- If the `.rda` file is unchanged, DPR2 preserves existing documentation
  and does not overwrite `.R` or `.Rd` files.
- If an object is no longer present in the `data/` directory, DPR2
  deletes the corresponding `.R` and `.Rd` files during rendering.

This behavior keeps the documentation in sync with the underlying data
while avoiding unnecessary overwrites when no changes are detected.

If no new objects have been added and no existing objects have changed,
DPR2 outputs a message indicating that no data documentation was
generated. This lets users confirm that documentation is up to date
without triggering unnecessary updates.

For example, after re-running
[`dpr_render()`](https://fredhutch.github.io/DPR2/reference/dpr_render.md)
with no changes to the saved objects, users will see the following:

``` r
dpr_render(path)
```

    ## No new data object documentation created, as no objects have been modified and all objects are documented in `.R`.

In contrast, in the example below, `mtcars_df` was removed from the
processing script. DPR2 detects that the object is no longer present and
deletes both `R/mtcars_df.R` and `man/mtcars_df.Rd` the next time
[`dpr_render()`](https://fredhutch.github.io/DPR2/reference/dpr_render.md)
is called.

    ## No new data object documentation created, as no objects have been modified and all objects are documented in `.R`.

    library(DPR2)
    data('mtcars')
    mtcars_list <- list(
      raw_data = mtcars,
      summary = summary(mtcars),
      column_classes = sapply(mtcars, class),
      correlation_matrix = cor(mtcars)
    )
    dpr_save('mtcars_list')

``` bash
TestPKG
├── R
│   └── mtcars_list.R
├── data
│   └── mtcars_list.rda
├── man
│   └── mtcars_list.Rd
└── processing
    └── mtcars.R
```

Finally, it’s important to note that DPR2 will ***not*** write data
documentation for `.rda` files that have more than one object saved, or
if the object in the `.rda` file does not match the `.rda` file name.
This is due to the unclear mapping between the filename and object name.
Warning messages will be outputted to alert the user of this. To enforce
best practices, use
[`dpr_save()`](https://fredhutch.github.io/DPR2/reference/dpr_save.md),
not [`save()`](https://rdrr.io/r/base/save.html), to write RDA files.

    library(DPR2)

    data('mtcars')
    mtcars_df <- mtcars
    mtcars_df$efficiency <- mtcars$mpg / mtcars$hp

    # next line is not a best practice! Use dpr_save() instead.
    save('mtcars_df', file=dpr_path('data', 'my_df.rda'))
    dpr_save('mtcars')

``` r
dpr_render(path)
```

    ## Warning in generate_all_docs(path = path): 'my_df.rda' does not match data
    ## object name 'mtcars_df'. Will skip writing documentation for it.

    ## No new data object documentation created, as no objects have been modified and all objects are documented in `.R`.
