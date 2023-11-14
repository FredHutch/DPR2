x <- read.csv("inst/data/src.csv") |>
    (\(.){ .$x <- .$x^(1/2); return(.) })()

save(x, file="data/myRootData.rda")
