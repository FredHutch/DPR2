##' Default key:value pairs for the data package datapackager.yml.
##'
##' Definitions of yaml key:value pairs
##' @title dpr_yaml_defaults
##' @return a list
##' @author jmtaylor
##' @export
dpr_yaml_defaults <- function(){
  list(
    "build_output"                 = "../",
    "source_data_directory"        = "inst/extdata",
    "install_on_build"             = FALSE,
    "build_tarball"                = FALSE,
    "process_directory"            = "processing",
    "process_on_build"             = "",
    "render_on_build"              = TRUE,
    "write_to_vignettes"           = TRUE,
    "auto_increment_data_versions" = TRUE,
    "purge_data_directory"         = TRUE,
    "data_digest_directory"        = "inst/data_digest",
    "render_env_mode"              = "isolate"
  )
}

##' Default key:value pairs for the data package DESCRIPTION file.
##'
##' @title dpr_description_defaults
##' @return a list
##' @author jmtaylor
##' @export
dpr_description_defaults <- function(){
  list(
    "Package"     = "MyDataPackage",
    "Title"       = "MyDataPackageTitle",
    "Version"     = "1.0",
    "Authors"     = "FirstName LastName [aut, cre]",
    "Description" = "What the package does (one paragraph).",
    "License"     = "See `https://www.gnu.org/licenses/license-list.html` or `https://choosealicense.com/` for more information",
    "Encoding"    = "UTF-8",
    "Depends"     = "R (>= 3.5)"
  )
}

##' Private. A function that generates sets DESCRIPTION file
##' key:values pairs in a new data package.
##'
##' @title dpr_description_init_set
##' @param desc an R desc object.
##' @param pkgp the package path
##' @author jmtaylor
dpr_description_init_set <- function(desc, pkgp){
  defa <- dpr_description_defaults()
  invisible(
    Map(desc::desc_set_list, key = names(defa), list_value = defa, file = pkgp)
  )
  invisible(
    Map(desc::desc_set_list, key = names(desc), list_value = desc, file = pkgp)
  )
}

##' Private. A function that generates sets DESCRIPTION file
##' key:values pairs in a new data package.
##'
##' @title dpr_yaml_init_set
##' @param yml an R yaml object.
##' @param pkgp the package path
##' @author jmtaylor
dpr_yaml_init_set <- function(yml, pkgp){
  def <- dpr_yaml_defaults()
  new <- utils::modifyList(def, yml)
  yaml::write_yaml(new, file.path(pkgp, "datapackager.yml"))
}

##' A function where each argument ammends or adds to the
##' datapackager.yml file's key:value pairs. No arguments will return the
##' datapackager.yml default values. see `dpr_yaml_defaults()` for a list of
##' default values, or call this function with no arguments.
##'
##' @title dpr_yaml_init
##' @param ... Arguments are treated as key value pairs for the
##'     datapackager.yml file.
##' @return A list of key:value pairs for generating a
##'     datapackager.yml file.
##' @author jmtaylor
##' @export
dpr_yaml_init <- function(...){
  vals <- list(...)
  yaml <- dpr_yaml_defaults()
  ## override defaults and add options with arguments
  return(utils::modifyList(yaml, vals))
}

##' A function that where each argument converts or adds to the
##' DESCRIPTION file's key:value pairs. No arguments will return the
##' default values. See `dpr_description_defaults()` for a list of
##' default values, or call this function with no arguments.
##'
##' @title dpr_description_init
##' @param ... Arguments are treated as key value pairs for the
##'     package DESCRIPTION file.
##' @return A list of key:value pairs for generating a DESCRIPTION
##'     file.
##' @author jmtaylor
##' @export
dpr_description_init <- function(...){
  vals <- list(...)
  desc <- dpr_description_defaults()
  if(!"Package" %in% names(vals))
    warning("Default package name used: ", desc$Package)
  ## override defaults and add options with arguments
  for(val in names(vals))
    desc[val] <- vals[[val]]
  return(desc)
}

##' Initialize a data package. Package is initialized with
##' datapackager.yml and DESCRIPTION files as described by
##' dpr_yaml_init() and dpr_description_init() function calls.
##'
##' @title dpr_init
##' @param path A path to the data package.
##' @param yaml A returned list for dpr_yaml_init()
##' @param desc A returned list for dpr_description_init()
##' @param renv_init Logical; whether to initiate renv (default TRUE)
##' @author jmtaylor
##' @export
dpr_init <- function(path = ".", yaml = dpr_yaml_init(), desc = dpr_description_init(), renv_init = TRUE){
  pkgp <- file.path(path, desc$Package)
  
  if(dir.exists(pkgp))
    stop(sprintf("Package '%s' path already exists.", pkgp))
  if(!dir.exists(dirname(pkgp)))
    stop("Package directory does not exist.")

  tryCatch(
  {

    ## create package skeleton
    dirnm <- c("data", "inst", yaml$process_directory, yaml$source_data_directory, yaml$data_digest)

    tpath <- system.file("templates", package="DPR2")

    dir.create(pkgp)
    for( dir in dirnm )
      dir.create(file.path(pkgp, dir))
    for( fil in c("NAMESPACE", "DESCRIPTION"))
      file.copy(file.path(tpath, fil), file.path(pkgp, fil))

    dpr_description_init_set(desc, pkgp)
    dpr_yaml_init_set(yaml, pkgp)

    ## init renv
    if(renv_init == TRUE)
      renv::init(
        pkgp,
        settings = list(snapshot.type = "implicit"),
        load = FALSE,
        restart = FALSE
      )
    
  },
  error = function(e){
    if(dir.exists(pkgp))
      unlink(pkgp, recursive=TRUE)
    stop(e, traceback())
  },
  finally = {
  })

}
