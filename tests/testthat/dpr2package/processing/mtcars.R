library(DPR2)
data(mtcars, envir=environment())
mazda <- mtcars[grep("Mazda", row.names(mtcars)),]
dpr_save("mazda")
