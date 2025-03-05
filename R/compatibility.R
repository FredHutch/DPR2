#' Private. Converts a DataPackageR yaml file to a DPR2 yaml file. Returns a writeable
#' yaml object.
#'
#' @title dpr1_yaml_load
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @author jmtaylor
#' @return a list of DPR2 yaml key:values pairs
#' @noRd
dpr1_yaml_load <- function(path="."){
  yml <- yaml::read_yaml(file.path(path, "datapackager.yml"))
  pro <- yml$configuration$files
  obj <- yml$configuration$objects
  yml <-  dpr_yaml_init(
      process_directory = "data-raw",
      process_on_build = names(pro)[unlist(pro)],
      purge_data_directory = FALSE,
      objects = obj
  )
  return(yml)
}

#' Private. Converts a DataPackageR DATADIGEST file to a DPR2
#' data_digest directory. Returns a named list of file and thier
#' digest values.
#'
#' @title dpr1_data_digest_load
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @author jmtaylor
#' @return a list of DPR2 yaml key:values pairs
#' @noRd
dpr1_data_digest_load <- function(path="."){
  yml <- yaml::read_yaml(file.path(path, "DATADIGEST"))
  return( yml[ seq(2, length(yml)) ] )
}

#' Private. Replaces a DataPackageR yaml with a DPR2 yaml.
#'
#' @title dpr1_yaml_convert
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @author jmtaylor
#' @noRd
dpr1_yaml_convert <- function(path="."){
  if( dpr_is_dpr1(path) )
    yaml::write_yaml(dpr1_yaml_load(path), file.path(path, "datapackager.yml"))
}

#' Private. Replaces a DataPackageR data digest with a DPR2 data digest.
#'
#' @title dpr1_data_digest_convert
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @author jmtaylor
#' @noRd
dpr1_data_digest_convert <- function(path="."){

  dig <- dpr1_data_digest_load(path)

  # to check if the objects in dpr digest are the same names as the files in data
  found <- names(dig) %in% gsub("\\.rda", "", dpr_list_rda(path), ignore.case=TRUE)
  if(!all(found))
    warning(
      "Items in DataPackageR `DATADIGEST` are not found in the data directory by RDA file name. Those not found will not be included in the DPR2 `data_digest` directory."
    )

  # to write the new data digest files
  dig_dir <- dpr_yaml_load(path)$data_digest_directory
  for(d in names(dig)){
    writeLines(dig[[d]], file.path(path, dig_dir, paste0("_", d, ".rda")))
  }
}

##' Private. A function for cleaning up after a DataPackageR to DPR2 conversion.
##'
##' @title dpr1_clean
##' @author jmtaylor
#' @noRd
dpr1_clean <- function(path){
  unlink(file.path(path, "DATADIGEST"))
  unlink(file.path(path, "NEWS.md"))
  # a place holder to remove older documentation
}

#' Convert a repository from DataPackageR to DPR2.
#'
#' This is a mildly destructive process which convert, adds, and
#' removes files to take information from a DataPackageR repository
#' and put it in the DPR2 format. This function also removes those
#' files that are no longer needed from DataPackageR by DPR2. <specify
#' specifically what is removed, added, and changed>
#' @title dpr_convert
#' @param path a path value to init at a specific path, when using the
#'   default the current working directory is used.
#' @param renv_init to initialize renv at the newly converted data package
#' @author jmtaylor
#' @export
dpr_convert <- function(path = ".", renv_init = TRUE){
  if( !dpr_is_dpr1(path) )
    stop("Data package at path argument is not detected as DataPackageR package.")
  dpr_init(path, renv_init = renv_init)
  dpr1_yaml_convert(path)
  dpr1_data_digest_convert(path)
  # a place holder for converting documentation
  dpr1_clean(path)
}

#' Private. A function to verify whether a specified directory contains a
#' DataPackageR package.
#'
#' @title dpr_is_dpr1
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @return a boolean value indicating whether the specified directory contains
#'   a DataPackageR package or not
#' @author jmtaylor
#' @noRd
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

#' Private. A function to verify whether a specified directory contains a DPR2
#' package or not
#'
#' @title dpr_is_dpr2
#' @param path path to datapackage
#' @return logical
#' @author jmtaylor
#' @noRd
dpr_is_dpr2 <- function(path="."){
  if(
    file.exists(file.path(path, "datapackager.yml")) &&
      !dpr_is_dpr1(path)
  )
    return(TRUE)
  return(FALSE)
}

#' Private. Throw warning when using deprecated [project_path()] wrappers for
#' DPR1 compatibility
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
#' @param ... Trailing path components passed to [file.path()]. All arguments
#'   must be the same length or length one.
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
#' @param ... Trailing path components passed to [file.path()]. All arguments
#'   must be the same length or length one.
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
#' @param ... Trailing path components passed to [file.path()]. All arguments
#'   must be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_extdata_path <- function(...){
  legacy_path_helper_warning('project_extdata_path')
  dpr_path('inst', 'extdata', ...)
}
