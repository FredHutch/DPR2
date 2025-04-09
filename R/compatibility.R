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
      objects = obj[unlist(pro)]
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
#' Must be run after the new `data_digest` directory is established by `dpr_init`.
#'
#' @title dpr1_data_digest_convert
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @author jmtaylor
#' @noRd
dpr1_data_digest_convert <- function(path="."){

  dig <- dpr1_data_digest_load(path)
  rda <- gsub("\\.rda", "", basename(dpr_list_rda(path)), ignore.case=TRUE)

  # to check if the objects in dpr digest are the same names as the files in data
  found <- names(dig) %in% rda
  if(!all(found))
    warning(
      "Items in DataPackageR `DATADIGEST` are not found in the data directory by RDA file name. Those not found will not be included in the DPR2 `data_digest` directory."
    )

  for(dat in rda){
    cks <- dpr_checksum_files(file.path(path, "data", paste0(dat, ".rda")))
    if(dat %in% names(dig) && cks != dig[[dat]]) {
      warning(sprintf("Object `%s` in data directory does not match md5 not found in DATADIGEST.", dat))
    }
    dig[[dat]] <- cks
  }

  # to write the new data digest files
  dig_dir <- dpr_yaml_load(path)$data_digest_directory
  for(d in names(dig)){
    writeLines(dig[[d]], file.path(path, dig_dir, paste0(d, ".rda_")))
  }
}

##' Private. Clean out a directory by processing script name sans ext.
##'
##' @title clean_docs
##' @param process the process name from the processing script directory
##' @param path the package path
##' @param subpath the directory to clean at the process path
##' @author Jason Taylor
#' @noRd
clean_docs <- function(process, path, subpath){
  pro_name <- tools::file_path_sans_ext(basename(process))
  for(sub in list.files(file.path(path, subpath), full.names=TRUE)){
    sub_name <- tools::file_path_sans_ext(basename(sub))
    if(pro_name == sub_name) unlink(sub, recursive = TRUE)
  }
}

##' Private. A function for cleaning up after a DataPackageR to DPR2 conversion.
##'
##' @title dpr1_clean
##' @param path the package path
##' @author jmtaylor
#' @noRd
dpr1_clean <- function(path){

  desc <- desc::desc(file = file.path(path, "DESCRIPTION"))
  desc$del("DataVersion")
  desc$del("Date")
  desc$write()

  for(process in list.files(file.path(path, "data-raw"), full.names=TRUE)){
    clean_docs(process, path, "inst/doc")
  }

  for(process in list.files(file.path(path, "inst/extdata/Logfiles"), full.names=TRUE)){
    clean_docs(process, path, "inst/doc")
  }

  to_delete <- c(
    paste0("R/", desc$get_field("Package"), ".R"),
    "DATADIGEST",
    "NEWS.md",
    "Read-and-delete-me",
    "R/documentation.R",
    "data-raw/documentation.R"
  )

  for(f in to_delete)
    unlink(file.path(path, f))

  unlink(file.path(path, "inst/extdata/Logfiles/"), recursive = TRUE)

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
#' @author jmtaylor
#' @export
dpr_convert <- function(path = "."){
  if( !dpr_is_dpr1(path) )
    stop("Data package at path argument is not detected as DataPackageR package.")
  suppressWarnings(
    dpr_init(path, dpr_yaml_init(process_directory = "data-raw", purge_data_directory = FALSE))
  )
  dpr1_yaml_convert(path)
  dpr1_data_digest_convert(path)
  dpr1_clean(path)
}

#' Private. Produce consistent error when passing invalid package path.
#'
#' @title dpr_is_path
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @noRd
dpr_check_path <- function(path){
    if(!dir.exists(path)) stop("Package path not found.")
}

#' Private. A function to verify whether a specified directory contains a
#' DataPackageR package.
#'
#' @title dpr_is_dpr1
#' @param path The relative path to the data package. The default is the
#'   working directory.
#' @return a boolean value indicating whether the specified directory contains
#'   a DataPackageR package or not
#' @noRd
dpr_is_dpr1 <- function(path="."){
  dpr_check_path(path)
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
