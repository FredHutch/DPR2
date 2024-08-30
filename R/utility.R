##' Private. Checks if path is a DPR2 datapackage or not.
##'
##' @title dpr_is  
##' @param path path to datapackage
##' @return boolean
##' @author jmtaylor
dpr_is <- function(path){
  if( !file.exists(file.path(path, "datapackager.yml")) )
    stop("`datapackager.yml` does not exist. Either the working directory of the evaluation environment is not at package root, or 'datapackager.yml' is not found in data package.")
  if( !file.exists(file.path(path, "DESCRIPTION")) )
    stop("`DESCRIPTION` does not exist. The working directory of the evaluation environment is not at package root.")
}

##' Private. Load `datapackager.yml` into memory.
##'
##' @title dpr_yaml_load
##' @param path the package path
##' @return a yaml object
##' @author jmtaylor
dpr_yaml_load <- function(path="."){
  return( yaml::yaml.load_file(file.path(path, "datapackager.yml")) )    
}

##' Private. Check yaml values that must be specific.
##'
##' @title dpr_yaml_validate
##' @param yml a parsed yaml object
##' @author jmtaylor
dpr_yaml_validate <- function(yml){
  ## check that all controlled key values are correct
  key_value = list(
    "render_env_mode" = c("isolate", "share")
  )
  for(key in names(key_value)){
    if(!yml[[key]] %in% key_value[[key]]){
      stop(
        sprintf(
          "Invalid `%s` yaml value used. Please one of these: %s",
          key, paste(key_value[[key]], collapse = ", ")
        )
      )
    }
  }

  ## check that all default yaml values are present, should catch typos manually entered in yaml
  def <- dpr_yaml_defaults()
  nameCheck <- !(names(def) %in% names(yml))
  if(any(nameCheck)){
    nm <- paste(names(def)[nameCheck], collapse = ",")
    stop(
      "The following required yaml values are not found: ", nm, ". ",
      "Please add those using `dpr_yaml_set()`, or directly in the `datapackager.yml` file."
    )
  }

}

##' Private. Load the package DESCRIPTION file into memory.
##'
##' @title dpr_description_load
##' @param path the package path
##' @return a desc object
##' @author jmtaylor
dpr_description_load <- function(path){
  dpr_is(path)
  desc::desc(file = path)
}

##' Private. Replace and add key:value pairs in old list with new.
##'
##' @title dpr_set_keys
##' @param old key:values list object
##' @param new key:values list object to add/modify
##' @return list
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
##'   specified, those arguments are used as the YAML key:value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return list
##' @author jmtaylor
##' @export
dpr_yaml_get <- function(path=".", ...){
  dpr_is(path)
  new <- list(...)
  yml <- dpr_set_keys(
    dpr_yaml_load(path), 
    new
  )
  dpr_yaml_validate(yml)
  return(yml)
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
  new <- dpr_set_keys(yml, list(...))
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

##' A convenience function for writing data objects to the package data directory.
##'
##' @title dpr_save
##' @param objects a character vector of object names to saved from the calling environment.
##' @author jmtaylor
##' @export
dpr_save <- function(objects){
  if(!is.character(objects)){
    stop("Only character vectors allowed.")
  }
  for(obj in objects){
    x <- get(obj, envir=parent.frame())
    save(
      x,
      file = file.path(
        dpr_yaml_get()$data_directory, paste0(obj, ".rda")
      )
    )
  }
}
