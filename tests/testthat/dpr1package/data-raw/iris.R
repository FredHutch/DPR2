
mtcars_mod <- datapackager_object_read("mtcars_mod")
if(!unique(mtcars_mod$test) == "test")
  stop("wrong mtcars")

data(iris, envir=environment())

