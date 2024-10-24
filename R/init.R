##' Private. Return a full path of file from installed templates directory.
##'
##' @title dpr_get_template
##' @return a character vector
##' @author jmtaylor
##' @param regex a regular expression
dpr_get_template <- function(regex){
  tmpl <- list.files(file.path(system.file("templates", package="DPR2")), full.names = T)
  return(tmpl[grepl(regex, tmpl)])
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
  vals <- list(...)
  yaml <- dpr_yaml_defaults()
  ## override defaults and add options with arguments
  for(val in names(vals))
    yaml[[val]] <- vals[[val]]
  return(yaml)
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
