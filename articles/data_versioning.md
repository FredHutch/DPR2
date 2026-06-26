# Data Versioning

DPR2 offers features to facilitate tracking data versioning throughout a
project’s lifecycle. This vignette will cover those features.

One feature that DPR2 offers is the ability to print the history of data
objects from a version-controlled data package and compare them to the
current version. Here we use the `dpr_data_history` function to get a
listing of `rda` files, when they were created, and their git SHA-1
hashes. We can then fetch specific versions using
`dpr_recall_data_versions`.

``` r

history <- dpr_data_history(path=path, include_checksums=TRUE)
print(history)
```

    ##                              blob_git_sha1         name   author
    ## 1 6ee0cb23e07ad8fbcbe3b0ac9b5c3c3ffb0aa880 irisArea.rda notauser
    ## 2 c2e89115c592d799c0ebda751dd1cc420a9e65da    mazda.rda notauser
    ## 3 a378d4413db257ba7db7e116f07510db45f9faf5    trees.rda notauser
    ## 4 f7bdd6f078dbd2be0c8dae2a9b6a8aadb94d9c14 irisArea.rda notauser
    ## 5 7fc0b86285717a361c285be86491fcc500cb4ab6    mazda.rda notauser
    ## 6 117c4e907e726a0ee3f3d6ede5963f5370cef9fb    trees.rda notauser
    ##                  when                  object_checksum
    ## 1 2026-06-26 19:17:31 1a8558f6a03c6e7cd0fde036018388b0
    ## 2 2026-06-26 19:17:31 70b01998e5dd4243758e76b5312ee606
    ## 3 2026-06-26 19:17:31 370a7132861fb520bd721d9bcbe008a4
    ## 4 2026-06-26 19:17:34 e1279ff5a8c8ab9b255ce78f2cb08479
    ## 5 2026-06-26 19:17:37 8bf6afe5717bceebb76ed9bf069d4274
    ## 6 2026-06-26 19:17:39 b311633cdf60bc0ab9dd5de7a92bc282

``` r

irisAreas <- history[history$name == "irisArea.rda",]
firstIrisHash <- irisAreas[irisAreas$when == min(irisAreas$when), "blob_git_sha1"]
lastIrisHash <- irisAreas[irisAreas$when == max(irisAreas$when), "blob_git_sha1"]

dataversions <- dpr_recall_data_versions(c(firstIrisHash, lastIrisHash), path)
```

We can use the `dpr_data_digest` function to get a table of the current
hashes recorded in the data package. The last object’s checksum should
match what is in the `dpr_data_digest` output table.

``` r

digest::digest(dataversions[[1]][[1]])
```

    ## [1] "1a8558f6a03c6e7cd0fde036018388b0"

``` r

digest::digest(dataversions[[2]][[1]])
```

    ## [1] "e1279ff5a8c8ab9b255ce78f2cb08479"

``` r

dpr_data_digest(path)
```

    ##           name                  data_digest_md5
    ## 1 irisArea.rda e1279ff5a8c8ab9b255ce78f2cb08479
    ## 2    mazda.rda 8bf6afe5717bceebb76ed9bf069d4274
    ## 3    trees.rda b311633cdf60bc0ab9dd5de7a92bc282

We can inspect the changes between the two versions using
[`diffdf::diffdf`](https://gowerc.github.io/diffdf/latest-tag/reference/diffdf.html).

``` r

diffdf::diffdf(
  dataversions[[1]][[1]],
  dataversions[[2]][[1]]
)
```

    ## Warning in diffdf::diffdf(dataversions[[1]][[1]], dataversions[[2]][[1]]): 
    ## There are columns in BASE that are not in COMPARE !!
    ## There are columns in COMPARE that are not in BASE !!

    ## Differences found between the objects!
    ## 
    ## Summary of BASE and COMPARE
    ##   ============================================================
    ##     PROPERTY            BASE                    COMP          
    ##   ------------------------------------------------------------
    ##       Name     dataversions[[1]][[1]]  dataversions[[2]][[1]] 
    ##      Class           data.frame              data.frame       
    ##     Rows(#)             150                     150           
    ##    Columns(#)            6                       6            
    ##   ------------------------------------------------------------
    ## 
    ## 
    ## There are columns in BASE that are not in COMPARE !!
    ##   ============
    ##     COLUMNS   
    ##   ------------
    ##    Sepal.Area 
    ##   ------------
    ## 
    ## 
    ## There are columns in COMPARE that are not in BASE !!
    ##   ============
    ##     COLUMNS   
    ##   ------------
    ##    Petal.Area 
    ##   ------------

Below, we render a package before building it to compare the object
checksums between the refreshed object in the data directory.

``` r

dpr_render(path)
```

Once we have rendered and changes are detected, we can compare datasets
using `dpr_compare_data_digest`.

``` r

dpr_compare_data_digest(path)
```

    ##           name                         data_md5
    ## 1 irisArea.rda c1bb2fb3a1f22b55ec3a9ce295566370
    ## 2    mazda.rda 8bf6afe5717bceebb76ed9bf069d4274
    ## 3    trees.rda b311633cdf60bc0ab9dd5de7a92bc282
    ##                    data_digest_md5  same
    ## 1 e1279ff5a8c8ab9b255ce78f2cb08479 FALSE
    ## 2 8bf6afe5717bceebb76ed9bf069d4274  TRUE
    ## 3 b311633cdf60bc0ab9dd5de7a92bc282  TRUE
