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
    ## 1 98032b42f26534f0b076f6a34f8159ed78ff900f irisArea.rda notauser
    ## 2 1fc98cb6ec6ff8b71b73b83455f52a4a9042c36e    mazda.rda notauser
    ## 3 1a66500605187834372c89363967d7bdf5d4ff88    trees.rda notauser
    ## 4 93235b1811953c539a7ef5015d0a99fe7a2b24f1 irisArea.rda notauser
    ## 5 e6149047f56bbe9325e284ba602c7d284819b299    mazda.rda notauser
    ## 6 73c95fcdb7f33586e4fddd6573201d572558126f    trees.rda notauser
    ##                  when                  object_checksum
    ## 1 2026-01-13 18:42:28 1a8558f6a03c6e7cd0fde036018388b0
    ## 2 2026-01-13 18:42:28 70b01998e5dd4243758e76b5312ee606
    ## 3 2026-01-13 18:42:28 370a7132861fb520bd721d9bcbe008a4
    ## 4 2026-01-13 18:42:31 e1279ff5a8c8ab9b255ce78f2cb08479
    ## 5 2026-01-13 18:42:34 8bf6afe5717bceebb76ed9bf069d4274
    ## 6 2026-01-13 18:42:36 b311633cdf60bc0ab9dd5de7a92bc282

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
