library(DPR2)

data(iris, envir=environment())
iris$Sepal.Area <- iris$Sepal.Length *  iris$Sepal.Width
irisArea <- iris
dpr_save("irisArea")
