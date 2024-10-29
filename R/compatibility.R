##' Converts a DataPackageR yaml file to a DPR2 yaml file. Returns a writeable yaml object.
##'
##' @title dpr_yaml_convert
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @author jmtaylor
##' @return a list of DPR2 yaml key:values pairs
##' @export
dpr_dpr1_yaml_load <- function(path="."){
  yml <- yaml::read_yaml(file.path(path, "datapackager.yml"))
  pro <- yml$configuration$files
  obj <- yml$configuration$objects
  yml <-  dpr_yaml_init(
      process_directory = "data-raw",
      process_on_build = names(pro)[unlist(pro)],
      purge_data_directory = FALSE,
      objects = obj
  )
  yml["data_digest_directory"] <- NULL
  return(yml)
}

##' Replaces a DataPackageR yaml with a DPR2 yaml.
##'
##' @title dpr_dpr1_yaml_convert
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @author jmtaylor
##' @export
dpr_dpr1_yaml_convert <- function(path="."){
  if( dpr_is_dpr1(path) )
    yaml::write_yaml(dpr_dpr1_yaml_load(path), file.path(path, "datapackager.yml"))
  else
    stop("Must be a DataPackageR qualified data package to convert to the datapackager.yml to the DPR2 format.")
}

##' Private. Checks if path is a DataPackageR package.
##'
##' @title dpr_is_dpr1
##' @param path The relative path to the data package. The default is the working directory.
##' @return logical
##' @author jmtaylor
dpr_is_dpr1 <- function(path="."){
  if(file.exists(file.path(path, "datapackager.yml"))){
    yml <- yaml::read_yaml(file.path(path, "datapackager.yml"))
    # this check is based on the DataPackageR check in processData.R
    if(
      "configuration" %in% names(yml) &&
        all(c("files", "objects") %in% names(yml[["configuration"]])) &&
        length(dpr_yaml_required_check(yml)) != 0
    )
      return(TRUE)
  }
  return(FALSE)
}

##' Private. Checks if path is a DPR2 package.
##'
##' @title dpr_is_dpr2
##' @param path path to datapackage
##' @return logical
##' @author jmtaylor
dpr_is_dpr2 <- function(path="."){
  if(
    file.exists(file.path(path, "datapackager.yml")) &&
      !dpr_is_dpr1(path)
  )
    return(TRUE)
  return(FALSE)
}

##' A reimplementation of DataPackageR::datapackager_object_read for
##' processing scripts from DataPackageR packages. This originaly
##' required a previous process to export the object before this could
##' be used. The DPR2 equivalent of that is to simply read the rda
##' file saved by a previous process.
##'
##' @title datapackager_object_read
##' @param object a singleton character vector of an object name saved by `dpr_save` in a previously rendered script. 
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @return an environment containing the object
##' @author jmtaylor
##' @export
datapackager_object_read <- function(object, path = "."){
  warning( "`datapackager_object_read` has been depreciated in DPR2.\nConsider using the datapackager.yml argument `render_env: shared` instead.\n" )

  if( !(is.character(object) && length(object) == 1) )
    stop("`datapackager_object_read` object argument must be a singleton character vector.")

  obj_path <- paste0(path, "/data/", object, ".rda")

  if( !file.exists(obj_path) )
    stop("Object does not exist in the data directory.")

  env <- new.env(parent = emptyenv())
  load(paste0(path, "/data/", object, ".rda"), envir=env)

  if( length(names(env)) > 1 )
    stop("`datapackager_object_read` cannot read multi-object rda files.")
  if( !exists(object, envir = env) )
    stop("`datapackager_object_read` object argument not found at rda file with the object name.")

  else
    return(env[[object]])
}
