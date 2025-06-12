#' Private. Standard way of listing rda files available in data for use across
#' all functions.
#'
#' @title dpr_list_rda
#' @param path path to data package
#' @return a character vector of rda files in the data directory
#' @author jmtaylor
#' @noRd
dpr_list_rda <- function(path){
  sort(
    list.files(
      file.path(path, "data"),
      "\\.rda$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  )
}

#' Private. Verifies whether a specified path is a DataPackageR or DPR2
#' package.
#'
#' @title dpr_is
#' @param path path to datapackage
#' @return boolean
#' @author jmtaylor
#' @noRd
dpr_is <- function(path){
  return( dpr_is_dpr1(path) || dpr_is_dpr2(path) )
}

#' Private. Load `datapackager.yml` into memory. Works with both DPR1 and
#' DPR2 package.
#'
#' @title dpr_yaml_load
#' @param path the package path
#' @return a yaml object
#' @author jmtaylor
#' @noRd
dpr_yaml_load <- function(path="."){
  if( dpr_is_dpr1(path) )
    stop( "Attemping to load a yaml of a DataPackageR package. Must convert to DPR2 first. See `?dpr_convert." )
  if( dpr_is_dpr2(path) ){
    yml <- yaml::read_yaml(file.path(path, "datapackager.yml"))
    yml$objects <- gsub("\\.rda_$", "",  list.files(file.path(path, yml$to_build_directory, "objects")), ignore.case=TRUE)
    yml$process_on_build <- gsub("_$", "",  list.files(file.path(path, yml$to_build_directory, "scripts")))
    return( yml )
  }
  stop("`datapackager.yml` is invalid or missing.")
}

#' Private. Check that some yaml keys have specific values.
#'
#' @title dpr_yaml_value_check
#' @param yml a yaml object or list
#' @return elements of the list of key value pairs to check that did not pass
#' @author jmtaylor
#' @noRd
dpr_yaml_value_check <- function(yml){
  key_value = list(
    "render_env_mode" = c("isolate", "share")
  )
  key_check <- vapply(names(key_value), function(key) yml[[key]] %in% key_value[[key]], T)
  return( key_value[!key_check] )
}

#' Private. Check that yaml keys required by DPR2 are found in yaml.
#'
#' @title dpr_yaml_required_check
#' @param yml a yaml object or list
#' @return required yaml keys not found
#' @author jmtaylor
#' @noRd
dpr_yaml_required_check <- function(yml){
  def <- dpr_yaml_defaults()
  return( def[!(names(def) %in% names(yml))] )
}

#' Private. Check yaml values that must be specific.
#'
#' @title dpr_yaml_check
#' @param yml a parsed yaml object
#' @author jmtaylor
#' @noRd
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

#' Private. Load the package `DESCRIPTION` file into memory.
#'
#' @title dpr_description_load
#' @param path the package path
#' @return a desc object
#' @author jmtaylor
#' @noRd
dpr_description_load <- function(path){
  dpr_is(path)
  desc::desc(file = path)
}

#' Get the current `datapackager.yml` key:value pairs and temporarily
#' add to or modify those with with new values. All additions and
#' modifications happen only in memory, not on disc. To set on disc,
#' see `?dpr_yaml_set`.
#'
#' @title dpr_yaml_get
#' @param path The full path to the data package. The default is the
#'   working directory.
#' @param ... `datapakager.yml` value overrides. When arguments are
#'   specified, those arguments are used as the YAML key value pairs instead of
#'   what is specified by the `datapackager.yml` file. For a list of those
#'   key value pairs and their purposes, see `?dpr_yaml_defaults`.
#' @return list
#' @author jmtaylor
#' @export
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

#' Write new `datapackager.yml` with new or modified key:value pairs.
#'
#' @title dpr_yaml_set
#' @param path The full path to the data package. The default is the working
#'   directory.
#' @param ... `datapakager.yml` value overrides. When arguments are specified,
#'   those arguments are used as the YAML key value pairs instead of what is
#'   specified by the `datapackager.yml`. Note, any values can be set here, but
#'   only those returned by `dpr_yaml_defaults()` are used by DPR2. See
#'   `?dpr_yaml_defaults` for information on those values.
#' @return nothing
#' @export
dpr_yaml_set <- function(path=".", ...){
  yml <- dpr_yaml_get(path)
  new <- utils::modifyList(yml, list(...), keep.null = TRUE)
  yaml::write_yaml(new, file.path(path, "datapackager.yml"))
}

#' Write new `DESCRIPTION` file with new or modified key:value pairs.
#'
#' @title dpr_description_set
#' @param path The full path to the data package. The default is the working
#'   directory.
#' @param ... datapakager.yml value overrides. When arguments are specified,
#'   those arguments are used as the YAML key value pairs instead of what is
#'   specified by the `datapackager.yml`. See `?dpr_description_defaults` for
#'   the default values that can be set.
#' @return nothing
#' @export
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

#' A convenience function for writing data objects to the package data
#' directory.
#'
#' @description `dpr_save` is vecotorized so users may pass a character vector
#'   of object names found in the calling environment to save to the `data`
#'   directory when the package is built. All objects are saved as single object
#'   `Rda` files by the object names that are passed.
#' @title dpr_save
#' @param objects Character vector of object names to be saved from the
#'   environment specified in `envir`.
#' @param path The path to the data package. Defaults to `dpr_path()`.
#' @param envir The environment to search for objects to save. Defaults to
#'   calling environment.
#' @returns The original `objects` argument, invisibly.
#' @export
dpr_save <- function(objects, path = dpr_path(), envir = parent.frame()){
  if(!is.character(objects))
    stop("Only character vectors allowed.")
  for(obj in objects){
    save(list=obj, file = file.path(path, "data", paste0(obj, ".rda")), envir=envir)
  }
  invisible(objects)
}

#' Construct path from data package directory
#'
#' @param ... Trailing path components passed to `file.path()`. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
dpr_path <- function(...) rprojroot::find_package_root_file(...)

#' Private. Returns a table of added objects or scripts.
#'
#' @title added
#' @param path path to a DPR2 data package
#' @param items_path the path for the items that are being added
#' @param add_type the added type, either 'objects' or 'scripts'
#' @noRd
dpr_added <- function(path, items_path, add_type){
  items <- list.files(file.path(path, items_path))
  added <- gsub("_$", "", list.files(file.path(path, dpr_yaml_load(path)$to_build_directory, add_type)))
  missing <- added[!added %in% items]
  if( length(missing) > 0 ){
    warning("Items being added not found. Consider removing with `dpr_rm_%s`.", add_type)
  }

  out <- data.frame(is_added = items %in% added)
  out[[add_type]] <- items
  return(out)
}

#' Return a data frame of scripts currently set to build.
#'
#' The data frame returned by `dpr_scripts` displays what processing scripts
#' are found in the processing directory (see, ?dpr_yaml_get) and which of those
#' will be rendered by calling `dpr_render` or `dpr_build`.
#' @title dpr_scripts
#' @param path path to a DPR2 data package
#' @return a data frame displaying script names in the processing directory,
#'   and if they are set to build or not
#' @export
dpr_scripts <- function(path = "."){
  dpr_added(path, dpr_yaml_get(path)$process_directory, "scripts")
}

#' Return a data frame of objects currently set to build.
#'
#' The data frame returned by `dpr_objects` displays what objects will be saved
#' from the processing environments to the `data` directory and reports what
#' objects are set to be built by DPR2. Objects are added to this list by using
#' `dpr_add_objects`. `dpr_add_objects` is mostly provided for backwards
#' compatibility with DataPackageR. It is recommended to use `dpr_save` in the
#' processing scripts directly to save files to the data directory rather than
#' using `dpr_add_objects`.
#' @title dpr_objects
#' @param path path to a DPR2 data package
#' @return a data frame displaying object names in the data directory, and if
#'   they are set to build or not
#' @export
dpr_objects <- function(path = "."){
  dpr_added(path, "data", "objects")
}

#' Private. Writes added file.
#'
#' @title write_added_file
#' @param added_file the path of the file to hash and add
#' @param added_type the added type, either 'objects' or 'scripts'
#' @param path path to a DPR2 data package
#' @noRd
write_added_file <- function(added_file, added_type, path = "."){
  file.create(
    file.path(path, dpr_yaml_get(path)$to_build_directory, added_type, paste0(basename(added_file), "_"))
  )
}

#' Add processing scripts to list of scripts rendered when a package is
#' rendered or built.
#'
#' This function will add new processing scripts to be rendered. To remove
#' processing scripts, see `?dpr_rm_scripts`. To generate a table of scripts
#' set to build, see `?dpr_processing`.
#' @title dpr_add_scripts
#' @param scripts the names of the scripts in the processing directory
#'   described in the datapackager.yml to be built when running `dpr_build`.
#'   scripts currently set to build.
#' @param path path to a data package
#' @export
dpr_add_scripts <- function(scripts, path = "."){
  yml <- dpr_yaml_load(path)
  process_files <- list.files(file.path(path, yml$process_directory))
  for( pro in scripts ) {
    if( !pro %in% process_files )
      stop(sprintf("`%s` not found in the process directory described in `datapackage.yml`. ", pro))
    write_added_file(file.path(path, yml$process_directory, pro), "scripts", path)
  }
}

#' Add objects to be built by DPR2.
#'
#' `dpr_add_objects` is mostly provided for backwards compatibility with
#' DataPackageR. It is recommended to use `dpr_save` in the processing scripts
#' directly to save files to the data directory rather than using
#' `dpr_add_objects`.
#' @title dpr_add_objects
#' @param objects the names of objects defined in processing scripts set to
#'   build.
#' @param path path to a DPR2 data package
#' @export
dpr_add_objects <- function(objects, path = "."){
  object_files <- list.files( file.path(path, "data") )
  added_files <- grepl( paste(objects, collapse = "|"), object_files )
  for( obj in object_files[added_files] )
    write_added_file(file.path(path, "data", obj), "objects", path)
}

#' Private. A function for removing added items, scripts or objects, from
#' the DPR2 package.
#'
#' @title dpr_rm_added
#' @param items the items to find in the to_build directory
#' @param added_type the added type, either 'objects' or 'scripts'
#' @param path path to a DPR2 data package
#' @noRd
dpr_rm_added <- function(items, added_type, path){
  adding <- file.path(path, dpr_yaml_get(path)$to_build_directory, added_type)
  adds <- list.files(adding)
  for( item in items ) {
    idx <- which(grepl(item, adds))
    if( length(idx) == 0  )
      stop(sprintf("`%s` not found in `%s/%s`.", item, dpr_yaml_get(path)$to_build_directory, added_type))
    unlink(file.path(adding, adds[idx]))
  }
}

#' Remove processing scripts from list of scripts rendered when a package is
#' rendered or built.
#'
#' This function will remove processing scripts from those that will be
#' rendered. To add processing scripts, see `?dpr_add_scripts`. To generate
#' a table of scripts currently being added, see `?dpr_scripts`.
#' @title dpr_rm_scripts
#' @param scripts names of scripts, found in the processing_directory set in
#'   the `datapackager.yml` file, to be removed from to_build and no long run
#'   when a script is rendered.
#' @param path path to a DPR2 data package
#' @export
dpr_rm_scripts <- function(scripts, path = "."){
  dpr_rm_added(scripts, "scripts", path)
}

#' Remove objects set to be built by DPR2.
#'
#' `dpr_add_objects` is mostly provided for backwards compatibility with
#' DataPackageR. It is recommended to use `dpr_save` in the processing scripts
#' directly to save files to the data directory rather than using
#' `dpr_add_objects`.
#' @title dpr_rm_objects
#' @param objects a character vector of object names to be built, adding them
#'   to the data directory after a processing script is rendered.
#' @param path path to a DPR2 data package
#' @export
dpr_rm_objects <- function(objects, path = "."){
  dpr_rm_added(objects, "objects", path)
}

#' Construct path from data package directory
#'
#' @param ... Trailing path components passed to `file.path()`. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
dpr_path <- function(...) rprojroot::find_package_root_file(...)
