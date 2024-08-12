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
      "dfm <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(df)",
      "save(dfm, file='data/mydataframe.rda')",
      "save(yml, file='data/myyaml.rda')"
    ),
    file.path(path, "processing/01.R")
  )

  writeLines(
    c(
      "dat <- matrix(1:16, nrow=4)",
      "save(dat, file='data/mymatrix.rda')"
    ),
    file.path(path, "processing/02.R")
  )

  ## a processing script that accesses the datapackager.yml
  writeLines(
    c(
      "dat <- as.list(LETTERS)",
      "save(dat, file=file.path(DPR2::dpr_yaml_get()$data_directory, 'letters.rda'))"
    ),
    file.path(path, "processing/A1.R")
  )

  ## check if environment is shared
  writeLines(
    c(
      "save(dat, file=file.path(DPR2::dpr_yaml_get()$data_directory, 'dat.rda'))"
    ),
    file.path(path, "processing/S1.R")
  )

}
