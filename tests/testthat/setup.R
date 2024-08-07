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
    yaml=dpr_yaml_init(process_on_build=c("01.R", "02.R")),
    desc=dpr_description_init(Package=package_name)
  )

  args[names(more_args)] <- more_args

  do.call(dpr_init, args)

  path <- file.path(temp_dir, package_name)
  
  writeLines(
    c(
      "library(yaml)",
      "mydataframe <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(mydataframe)",
      "dpr_save(mydataframe)",
      "save(yml, file='data/myyaml.rda')"
    ),
    file.path(path, "processing/01.R")
  )

  writeLines(
    c(
      "mymatrix <- matrix(1:16, nrow=4)",
      "dpr_save(mymatrix)"
    ),
    file.path(path, "processing/02.R")
  )

  ## a processing script that accesses the datapackager.yml
  writeLines(
    c(
      "dat <- as.list(LETTERS)",
      "ourLetters <- c('d', 'p', 'r')",
      "save(dat, file=file.path(dpr_yaml_get()$data_directory, 'letters.rda'))",
      "dpr_save(ourLetters)"
    ),
    file.path(path, "processing/A1.R")
  )

  ## check if environment is shared
  writeLines(
    c(
      "mymatrix[1] <- 100",
      "newmatrix <- mymatrix",
      "dpr_save(newmatrix)"
    ),
    file.path(path, "processing/S1.R")
  )

}
