##' Private. Verifies whether a specified path is a DataPackageR or DPR2
##' package.
##'
##' @title dpr_is
##' @param path path to datapackage
##' @return boolean
##' @author jmtaylor
##' @noRd
dpr_is <- function(path){
  return( dpr_is_dpr1(path) || dpr_is_dpr2(path) )
}

##' Private. Load `datapackager.yml` into memory. Works with both DPR1 and
##' DPR2 package.
##'
##' @title dpr_yaml_load
##' @param path the package path
##' @return a yaml object
##' @author jmtaylor
##' @noRd
dpr_yaml_load <- function(path="."){
  if( dpr_is_dpr1(path) )
    return( dpr_dpr1_yaml_load(path) )
  if( dpr_is_dpr2(path) )
    return( yaml::read_yaml(file.path(path, "datapackager.yml")) )
  stop("`datapackager.yml` is invalid or missing.")
}

##' Private. Check that some yaml keys have specific values.
##'
##' @title dpr_yaml_value_check
##' @param yml a yaml object or list
##' @return elements of the list of key value pairs to check that did not pass
##' @author jmtaylor
##' @noRd
dpr_yaml_value_check <- function(yml){
  key_value = list(
    "render_env_mode" = c("isolate", "share")
  )
  key_check <- vapply(names(key_value), function(key) yml[[key]] %in% key_value[[key]], T)
  return( key_value[!key_check] )
}

##' Private. Check that yaml keys required by DPR2 are found in yaml.
##'
##' @title dpr_yaml_required_check
##' @param yml a yaml object or list
##' @return required yaml keys not found
##' @author jmtaylor
##' @noRd
dpr_yaml_required_check <- function(yml){
  def <- dpr_yaml_defaults()
  # data_digest_directory is not required from the default set
  def["data_digest_directory"] <- NULL
  return( def[!(names(def) %in% names(yml))] )
}

##' Private. Check yaml values that must be specific.
##'
##' @title dpr_yaml_check
##' @param yml a parsed yaml object
##' @author jmtaylor
##' @noRd
dpr_yaml_check <- function(yml){
  ## check that all controlled key values are correct
  kv <- dpr_yaml_value_check(yml)
  if(length(kv) != 0)
    stop(
      sprintf(
        "Invalid yaml values used for the following keys:\n %s",
        paste0(
          Map(function(k, v) sprintf("    %s: %s", k, paste0(v, collapse = ", ")), names(kv), kv),
          collapse = "\n"
        )
      )
    )

  ## check that all default yaml values are present, should catch typos manually
  ## entered in yaml
  nm <- dpr_yaml_required_check(yml)
  if(length(nm) != 0)
    stop(
      "The following required yaml keys are not found: ", paste(names(nm), collapse = ", "), ". ",
      "Please add those using `dpr_yaml_set()`, or directly in the `datapackager.yml` file. ",
      "Use `dpr_yaml_defaults()` to see a list of default values."
    )

}

##' Private. Load the package DESCRIPTION file into memory.
##'
##' @title dpr_description_load
##' @param path the package path
##' @return a desc object
##' @author jmtaylor
##' @noRd
dpr_description_load <- function(path){
  dpr_is(path)
  desc::desc(file = path)
}

##' Get the current datapackager.yml key:value pairs and temporarily
##' add to or modify those with with new values. All additions and
##' modifications happen only in memory, not on disc.
##'
##' @title dpr_yaml_get
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key:value
##'   pairs instead of what is specified by the `datapackager.yml`.
##' @return list
##' @author jmtaylor
##' @export
dpr_yaml_get <- function(path=".", ...){
  if( dpr_is(path) ){
    new <- list(...)
    yml <- utils::modifyList(
      dpr_yaml_load(path),
      new,
      keep.null = TRUE
    )
    dpr_yaml_check(yml)
    return(yml)
  }
  stop("`path` argument is not a DataPackageR or DPR2 package.")
}

##' Write new datapackager.yml with new or modified key:value pairs.
##'
##' @title dpr_yaml_set
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_yaml_set <- function(path=".", ...){
  yml <- dpr_yaml_get(path)
  new <- utils::modifyList(yml, list(...), keep.null = TRUE)
  yaml::write_yaml(new, file.path(path, "datapackager.yml"))
}

##' Write new DESCRIPTION file with new or modified key:value pairs.
##'
##' @title dpr_description_set
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_description_set <- function(path=".", ...){
  dpr_is(path)
  new <- list(...)
  def <- dpr_description_defaults()
  invisible(
      Map(desc::desc_set_list, key = names(def), list_value = def, file = path)
  )
  invisible(
      Map(desc::desc_set_list, key = names(new), list_value = new, file = path)
  )
}

##' A convenience function for writing data objects to the package data
##' directory.
##'
##' @title dpr_save
##' @param objects Character vector of object names to be saved from the
##'   environment specified in \code{envir}.
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @param envir The environment to search for objects to save. Defaults to
##'   calling environment.
##' @returns The original \code{objects} argument, invisibly.
##' @author jmtaylor
##' @export
dpr_save <- function(objects, path = ".", envir = parent.frame()){
  if(!is.character(objects))
    stop("Only character vectors allowed.")
  for(obj in objects){
    save(list=obj, file = file.path(path, "data", paste0(obj, ".rda")), envir=envir)
  }
  invisible(objects)
}
