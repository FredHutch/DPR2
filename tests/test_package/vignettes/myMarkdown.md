    getwd()

    ## [1] "/home/jmtaylor/Projects/dpr2_spec_docs/test_dp"

    x <- read.csv("inst/data/src.csv") |>
        (\(.){ .$x <- .$x^2; return(.) })()

    barplot(x$x, names.arg=x$y)

![](/home/jmtaylor/Projects/dpr2_spec_docs/test_dp/vignettes/myMarkdown_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    save(x, file="data/mySquareData.rda")
