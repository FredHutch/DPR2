##' Private. Return a full path of file from installed templates directory.
##'
##' @title dpr_get_template
##' @return a character vector
##' @author jmtaylor
##' @param regex a regular expression
dpr_get_template <- function(regex){
  list.files(system.file("templates", package="DPR2"), regex, full.names = TRUE)
}

##' Default key:value pairs for the data package datapackager.yml.
##'
##' Definitions of yaml key:value pairs
##' @title dpr_yaml_defaults
##' @return a list
##' @author jmtaylor
##' @export
dpr_yaml_defaults <- function(){
  return(yaml::read_yaml(dpr_get_template("datapackager.yml$")))
}

##' Default key:value pairs for the data package DESCRIPTION file.
##'
##' @title dpr_description_defaults
##' @return a list
##' @author jmtaylor
##' @export
dpr_description_defaults <- function(){
  defd <- dpr_get_template("DESCRIPTION")
  return(
    as.list(
      desc::desc_get(desc::desc_fields(defd), defd)
    )
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
  invisible(
    Map(desc::desc_set_list, key = names(desc), list_value = desc, file = pkgp)
  )
}

##' Private. A function that sets datapackager.yml file
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
  utils::modifyList(dpr_yaml_defaults(), list(...), keep.null = TRUE)
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
  utils::modifyList(desc, vals, keep.null = TRUE)
}

##' Create an empty data package. Package is created with
##' datapackager.yml and DESCRIPTION files as described by
##' dpr_yaml_init() and dpr_description_init() function calls.
##'
##' @title dpr_create
##' @param path A path to the data package.
##' @param yaml A returned list for dpr_yaml_init()
##' @param desc A returned list for dpr_description_init()
##' @param renv_init Logical; whether to initiate renv (default TRUE)
##' @author jmtaylor
##' @export
dpr_create <- function(path = ".", yaml = dpr_yaml_init(), desc = dpr_description_init(), renv_init = TRUE){
  pkgp <- file.path(path, desc$Package)

  if(!dir.exists(path))
    stop("`path` argument does not point to an existing directory.")

  tryCatch(
  {

    ## create package skeleton
    dirs <- c("data", "inst", yaml$process_directory, yaml$source_data_directory, yaml$data_digest)
    dir.create(pkgp, showWarnings = FALSE)

    for( dir in dirs )
      if(!dir.create(file.path(pkgp, dir), showWarnings = FALSE))
        warning(sprintf("`%s` was found, skipping creating that directory.", dir))

    for( fil in c("NAMESPACE", "DESCRIPTION", "datapackager.yml") )
      if(!file.copy(system.file("templates", fil, package="DPR2"), file.path(pkgp, fil)))
        warning(sprintf("`%s` was found, skipping creating that file.", fil))
      else {
        if( fil == "DESCRIPTION" )
          dpr_description_init_set(desc, pkgp)
        if( fil == "datapackager.yml" )
          dpr_yaml_init_set(yaml, pkgp)
      }

    ## init renv
    if(renv_init && !file.exists(file.path(pkgp, "renv.lock")))
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
  })

}

##' A wrapper for dpr_create that uses the parent directory as the
##' location to create the data package, setting the package name as
##' the current directory.
##'
##' @title dpr_init
##' @author jmtaylor
##' @param path a path value to init at a specific path, when using
##'   the default the current working directory is used.
##' @param yaml A returned list for dpr_yaml_init()
##' @param desc A returned list for dpr_description_init()
##' @param renv_init Logical; whether to initiate renv (default TRUE)

##' @export
dpr_init <- function(
    path = ".",
    yaml = dpr_yaml_init(),
    desc = dpr_description_init(),
    renv_init = TRUE)
{
  path <- normalizePath(path)
  dpr_create(
    dirname(path),
    yaml = yaml,
    desc=dpr_description_init(Package = basename(path)),
    renv_init = renv_init
  )
}
