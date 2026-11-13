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

    ##                                  blob_sha1         name   author
    ## 1 6671c235f6b93ddcbeaa9de483b1decc533c9b97 irisArea.rda notauser
    ## 2 e5cf20020b321ad273a988a94911252d6204582f    mazda.rda notauser
    ## 3 bdcf141805d8ab7303add10533bb33e4cee80499    trees.rda notauser
    ## 4 c5ab415a4c9a7d17d85b684c2d993ecde21556c2 irisArea.rda notauser
    ## 5 b4679a67b25d9ae3fdc492fb64de91800276d990    mazda.rda notauser
    ## 6 6e314bd7f4d5811c9f9ca459eb669034a560121d    trees.rda notauser
    ##                  when                              commit_sha1
    ## 1 2026-11-13 18:44:08 1c87c25f2b79b8ce93065b96b912282e0242e814
    ## 2 2026-11-13 18:44:08 1c87c25f2b79b8ce93065b96b912282e0242e814
    ## 3 2026-11-13 18:44:08 1c87c25f2b79b8ce93065b96b912282e0242e814
    ## 4 2026-11-13 18:44:10 a5368ef5fa4ad3cf064cd6c29dc50404a064edf1
    ## 5 2026-11-13 18:44:13 a35ef25ba8d1bd88d84a003e30d1a8d1505aa377
    ## 6 2026-11-13 18:44:16 543bdb6e6a39088a23b1ddc8b180a9a7788f2d3a
    ##                    object_checksum
    ## 1 1a8558f6a03c6e7cd0fde036018388b0
    ## 2 70b01998e5dd4243758e76b5312ee606
    ## 3 370a7132861fb520bd721d9bcbe008a4
    ## 4 e1279ff5a8c8ab9b255ce78f2cb08479
    ## 5 8bf6afe5717bceebb76ed9bf069d4274
    ## 6 b311633cdf60bc0ab9dd5de7a92bc282

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

    ## [1] "70b01998e5dd4243758e76b5312ee606"

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
    ## There are rows in BASE that are not in COMPARE !!
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
    ##     Rows(#)             150                      2            
    ##    Columns(#)            6                       11           
    ##   ------------------------------------------------------------
    ## 
    ## 
    ## There are rows in BASE that are not in COMPARE !!
    ## First 10 of 148 rows are shown in table below
    ##   ===============
    ##    ..ROWNUMBER.. 
    ##   ---------------
    ##          3       
    ##          4       
    ##          5       
    ##          6       
    ##          7       
    ##          8       
    ##          9       
    ##         10       
    ##         11       
    ##         12       
    ##   ---------------
    ## 
    ## 
    ## There are columns in BASE that are not in COMPARE !!
    ##   ==============
    ##      COLUMNS    
    ##   --------------
    ##    Sepal.Length 
    ##    Sepal.Width  
    ##    Petal.Length 
    ##    Petal.Width  
    ##      Species    
    ##     Sepal.Area  
    ##   --------------
    ## 
    ## 
    ## There are columns in COMPARE that are not in BASE !!
    ## First 10 of 11 rows are shown in table below
    ##   =========
    ##    COLUMNS 
    ##   ---------
    ##      mpg   
    ##      cyl   
    ##     disp   
    ##      hp    
    ##     drat   
    ##      wt    
    ##     qsec   
    ##      vs    
    ##      am    
    ##     gear   
    ##   ---------

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
