dpr_yaml_defaults <- function(){
  list(
    "build_output"                 = "../",
    "data_directory"               = "data",
    "source_data_directory"        = "inst/extdata",
    "install_on_build"             = TRUE,
    "process_directory"            = "processing",
    "process_on_build"             = c(),
    "render_on_build"              = TRUE,
    "write_to_vignettes"           = TRUE,
    "auto_increment_data_versions" = TRUE,
    "purge_data_directory"         = TRUE,
    "data_digest_directory"        = "inst"
  )
}

dpr_description_defaults <- function(){
  list(
    "Package"     = "MyPackageName",
    "Title"       = "MyPackageTile",
    "Version"     = "1.0",
    "Authors"     = "FirstName LastName [aut, cre]",
    "Description" = "What the package does (one paragraph).",
    "License"     = "See `https://www.gnu.org/licenses/license-list.html` or `https://choosealicense.com/` for more information",
    "Encoding"    = "UTF-8"
  )
}

dpr_description_init_set <- function(desc, pkgp){
  defa <- dpr_description_defaults()
  Map(desc::desc_set_list, key = names(defa), list_value = defa, file = pkgp) |>
    invisible()
  Map(desc::desc_set_list, key = names(desc), list_value = desc, file = pkgp) |>
    invisible()
}

dpr_yaml_init_set <- function(yml, pkgp){
  def <- dpr_yaml_defaults()
  new <- dpr_set_keys(yml, def)
  yaml::write_yaml(new, file.path(pkgp, "datapackager.yml"))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... Arguments are treated as key value pairs for the datapackager.yml file.
##' @return 
##' @author jmtaylor
##' @export
dpr_yaml_init <- function(...){
  vals <- list(...)
  yaml <- dpr_yaml_defaults()
  ## override defaults and add options with arguments
  for(val in names(vals))
    yaml[[val]] <- vals[[val]]
  return(yaml)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... Arguments are treated as key value pairs for the package DESCRIPTION file.
##' @return 
##' @author jmtaylor
##' @export
dpr_description_init <- function(...){
  vals <- list(...)
  desc <- dpr_description_defaults()
  ## override defaults and add options with arguments
  for(val in names(vals))
    desc[val] <- vals[[val]]
  return(desc)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param path 
##' @param yml 
##' @param desc 
##' @return 
##' @author jmtaylor
##' @export
dpr_init <- function(path = "./", yml = dpr_yaml_init(), desc = dpr_description_init()){

  pkgp <- file.path(path, desc[["Package"]])
  tryCatch(
  {
    ## build package skeleton
    if(desc[["Package"]] == "MyPackageName")
      warning("Default package name used. Update DESCRIPTION file and root directory name to change.")

    ## build package skeleton
    dirnm <- c("data", "inst", yml$process_directory, yml$source_data_directory)
    tpath <- path.package("DPR2") |>
      (\(p)
        ifelse(
          dir.exists(file.path(p, "inst")),
          file.path(p, "inst/templates"),
          file.path(p, "templates")
        )
      )()
        
    dir.create(pkgp)
    for( dir in dirnm )
      dir.create(file.path(pkgp, dir))
    for( fil in c("NAMESPACE", "DESCRIPTION"))
      file.copy(file.path(tpath, fil), file.path(pkgp, fil))
    
    dpr_description_init_set(desc, pkgp)
    dpr_yaml_init_set(yml, pkgp)
      
  },
  error = \(e){
    if(dir.exists(pkgp))
      unlink(pkgp, recursive=TRUE)
    stop(e, traceback())
  },
  finally = {
  })
  
}

