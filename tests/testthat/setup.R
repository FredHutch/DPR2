getPkgDir <- function(){
  temp_dir <- file.path(tempdir(), "packages")
  dir.create(temp_dir)
  return(temp_dir)
}

cleanup <- function(temp_dir){
  if(dir.exists(temp_dir)){
    unlink(
      file.path(temp_dir),
      recursive=T
    )
  }
}

initPkg <- function(temp_dir, package_name, more_args=NULL){
  args <- list(
    path=temp_dir,
    yaml=dpr_yaml_init(process_on_build=c("01.R", "02.R", "01.Rmd")),
    desc=dpr_description_init(Package=package_name)
  )

  ## additional arguments add to the dpr_init call other than the presets listed above at `args`.
  ## this is set by the more_args argument in initPkg
  for(arg in names(args)){
    new <- more_args[[arg]]
    args[[arg]][names(new)] <- new
  }
  newArgs <- more_args[!names(more_args) %in% names(args)]
  args[names(newArgs)] <- newArgs

  do.call(dpr_init, args)

  path <- file.path(temp_dir, package_name)
  
  writeLines(
    c(
      "library(yaml)",
      "mydataframe <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(mydataframe)",
      "objYml1 <- 'test objects values 1'",
      "objYml2 <- 'test objects values 2'",
      "dpr_save('mydataframe')",
      "save(yml, file='data/myyaml.rda')"
    ),
    file.path(path, "processing/01.R")
  )

  writeLines(
    c(
      "---",
      "title: test report",
      "---",
      "test text",
      "```{r}",
      "library(yaml)",
      "df  <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(df)",
      "save(df,  file='data/mydataframe_rmd.rda')",
      "save(yml, file='data/myyaml_rmd.rda')",
      "```"
    ),
    file.path(path, "processing/01.Rmd")
  )
  
  writeLines(
    c(
      "mymatrix <- matrix(1:16, nrow=4)",
      "dpr_save('mymatrix')"
    ),
    file.path(path, "processing/02.R")
  )

  ## a processing script that accesses the datapackager.yml
  writeLines(
    c(
      "dat <- as.list(LETTERS)",
      "ourLetters <- c('d', 'p', 'r')",
      "save(dat, file=file.path(dpr_yaml_get()$data_directory, 'letters.rda'))",
      "dpr_save('ourLetters')"
    ),
    file.path(path, "processing/A1.R")
  )

  ## check if environment is shared
  writeLines(
    c(
      "mymatrix[1] <- 100",
      "newmatrix <- mymatrix",
      "dpr_save('newmatrix')"
    ),
    file.path(path, "processing/S1.R")
  )

}
