##' A private function for loading `datapackager.yml` into memory. If missing, throw exception.
##' @title dpr_is
##' @param path path to datapackage
##' @return boolean
##' @author jmtaylor
dpr_is <- function(path){
  return( dpr_is_dpr1(path) || dpr_is_dpr2(path) )
}

##' A private function for loading `datapackager.yml` into memory.
##' @title dpr_yaml_load
##' @param path the package path
##' @return a yaml object
##' @author jmtaylor
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
dpr_yaml_required_check <- function(yml){
  def <- dpr_yaml_defaults()
  def["data_digest_directory"] <- NULL # data_digest_directory is not required from the default set
  return( def[!(names(def) %in% names(yml))] )
}

##' A Private function for checking yaml values that must be specific.
##'
##' @title dpr_yaml_check
##' @param yml a parsed yaml object
##' @author jmtaylor
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

  ## check that all default yaml values are present, should catch typos manually entered in yaml
  nm <- dpr_yaml_required_check(yml)
  if(length(nm) != 0)
    stop(
      "The following required yaml keys are not found: ", paste(names(nm), collapse = ", "), ". ",
      "Please add those using `dpr_yaml_set()`, or directly in the `datapackager.yml` file. ",
      "Use `dpr_yaml_defaults()` to see a list of default values."
    )

}

##' A Private function for loading the package DESCRIPTION file into memory.
##'
##' @title dpr_description_load
##' @param path the package path
##' @return a desc object
##' @author jmtaylor
dpr_description_load <- function(path){
  dpr_is(path)
  desc::desc(file = path)
}


##' A private function for updating or adding adds key:value pairs in old list.
##' @title dpr_set_keys
##' @param old key:values list object
##' @param new key:values list object to add/modify
##' @return the modified list with updated or added key-value pairs
##' @author jmtaylor
dpr_set_keys <- function(old, new){
  ## replace any new keys with new
  for(nm in names(new))
    old[[nm]] <- new[[nm]]
  return(old)
}

##' Get the current datapackager.yml key:value pairs and add to or
##' modify those with with new values. All additions and modifications
##' happen only in memory, not on disc.
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
  new <- list(...)
  yml <- dpr_set_keys(
    dpr_yaml_load(path),
    new
  )
  dpr_yaml_validate(yml)
  return(yml)

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
##' @param path A character string specifying the full path to the data package. The default is the
##'   working directory.
##' @param ... Named key-value pairs that sets or updates datapakager.yml value. When arguments are
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

##' Write a new DESCRIPTION file with new or modified key:value pairs.
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


##' The dpr_data_versions function allows users to easily access and view the data object names and
##' versions rendered to the data directory. It outputs a hash or checksum to uniquely identify the contents of
##' each file, which is useful for tracking data changes over time.
##'
##' @title dpr_data_versions
##' @param path A character string specifying the full path to the data package. The default is the
##'   working directory.
##' @return a data.frame of each data object and the contents of each digest file, which includes a hash
##' representing the data version for the data object
##' @author jmtaylor
##' @export
dpr_data_versions <- function(path="."){
  dat <- list.files(file.path(path, dpr_yaml_get(path)$data_digest_directory), full.names=TRUE)
  data.frame(
    object=basename(dat),
    digest=vapply(dat, function(d) readLines(d), ""),
    row.names=seq(1,length(dat))
  )
}

##' A convenience function for writing and saving data objects to the package data directory.
##' @title dpr_save
##' @param objects a character vector of object names to saved from the calling environment
##' @param path The relative path to the data package. The default is the working directory.
##' @author jmtaylor
##' @export
dpr_save <- function(objects, path = "."){
  if(!is.character(objects))
    stop("Only character vectors allowed.")
  for(obj in objects)
    save(list=obj, file = file.path(path, "data", paste0(obj, ".rda")), envir=parent.frame(1))
}
