##' Private. Load `datapackager.yml` into memory. If missing, throw exception.
##'
##' @title dpr_yaml_load
##' @param pkgp the package path
##' @return a yaml object
##' @author jmtaylor
dpr_yaml_load <- function(pkgp){
  ## looking for exising yaml
  if( !file.exists(file.path(pkgp, "datapackager.yml")) )
    stop("`datapackager.yml` does not exist. Either working directory is not at a package root, or 'datapackager.yml' is not found in data package.")
  return( yaml::yaml.load_file(file.path(pkgp, "datapackager.yml")) )
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
##' @param pkgp the package path
##' @return a desc object
##' @author jmtaylor
dpr_description_load <- function(pkgp){
  desc::desc(file = pkgp)
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
  new <- list(...)
  def <- dpr_description_defaults()
  invisible(
      Map(desc::desc_set_list, key = names(def), list_value = def, file = path)
  )
  invisible(
      Map(desc::desc_set_list, key = names(new), list_value = new, file = path)
  )
}

##' Retrieve data.frame of data object names and versions rendered to the data directory.
##'
##' @title dpr_data_versions
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @return data.frame
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
