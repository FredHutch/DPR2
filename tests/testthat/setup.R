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

  writeLines(
    c(
      "chkvar <- 1",
      "dat <- data.frame(x=1:10, y=LETTERS[1:10], z=chkvar)",
      "save(dat, file='data/mydataframe.rda')"
    ),
    file.path(args$path, package_name, "processing/01.R")
  )

  writeLines(
    c(
      "dat <- matrix(1:16, nrow=4)",
      "save(dat, file='data/mymatrix.rda')"
    ),
    file.path(args$path, package_name, "processing/02.R")
  )

  ## a processing script that accesses the datapackager.yml
  writeLines(
    c(
      "dat <- as.list(LETTERS)",
      "save(dat, file=file.path(dpr_yaml_get()$data_directory, 'letters.rda'))"
    ),
    file.path(args$path, package_name, "processing/A1.R")
  )

}
