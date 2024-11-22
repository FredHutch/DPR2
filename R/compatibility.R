##' Converts a DataPackageR yaml file to a DPR2 yaml file. Returns a writeable yaml object.
##'
##' @title dpr_yaml_convert
##' @param path The reative path to the data package. The default is the
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

##' A private function to verify whether a specified directory contains a DataPackageR package.
##'
##' @title dpr_is_dpr1
##' @param path The relative path to the data package. The default is the working directory.
##' @return a boolean value indicating whether the specified directory contains a DataPackageR package or not
##' @author jmtaylor
dpr_is_dpr1 <- function(path="."){
  if(file.exists(file.path(path, "datapackager.yml"))){
    yml <- yaml::read_yaml(file.path(path, "datapackager.yml"))
    # this check is based on the DataPackageR check in processData.R
    if(
      "configuration" %in% names(yml) &&
        all(c("files", "objects") %in% names(yml[["configuration"]]))
    )
      return(TRUE)
  }
  return(FALSE)
}

##' A private function to verify whether a specified directory contains a DPR2 package or not
##'
##' @title dpr_is_dpr2
##' @param path path to datapackage
##' @return logical
##' @author jmtaylor
##' @noRd
dpr_is_dpr2 <- function(path="."){
  if(
    file.exists(file.path(path, "datapackager.yml")) &&
      !dpr_is_dpr1(path)
  )
    return(TRUE)
  return(FALSE)
}

#' Private. Throw warning when using deprecated [project_path()] wrappers for DPR1
#' compatibility
#' @noRd
legacy_path_helper_warning <- function(fn_nm){
  warning(
    paste0(
      fn_nm,
      '() is a compatibility wrapper for legacy code and ',
      'may be removed in the future. Please use dpr_path() instead.'
    )
  )
}

#' Construct path from data package directory (deprecated)
#'
#' This function is a compatibility wrapper for legacy code and may be removed
#' in the future. Please use [dpr_path()] instead.
#'
#' @param ... Trailing path components passed to [file.path()]. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_path <- function(...){
  legacy_path_helper_warning('project_path')
  dpr_path(...)
}

#' Construct path from data package data directory (deprecated)
#'
#' This function is a compatibility wrapper for legacy code and may be removed
#' in the future. Please use [dpr_path()] instead.
#'
#' @param ... Trailing path components passed to [file.path()]. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_data_path <- function(...){
  legacy_path_helper_warning('project_data_path')
  dpr_path('data', ...)
}

#' Construct path from data package inst/extdata directory (deprecated)
#'
#' This function is a compatibility wrapper for legacy code and may be removed
#' in the future. Please use [dpr_path()] instead.
#'
#' @param ... Trailing path components passed to [file.path()]. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_extdata_path <- function(...){
  legacy_path_helper_warning('project_extdata_path')
  dpr_path('inst', 'extdata', ...)
}
