#' Private. Throws error if the package path is already a dpr package, a check
#' for dpr_create or dpr_init.
#'
#' @title dpr_check_dpr
#' @param path A path to check if the source is a DataPackageR or DPR2 package.
#' @noRd
dpr_check_dpr <- function(path){
  if(dir.exists(path)){
    if(dpr_is(path)){
      if(dpr_is_dpr2(path)){
        stop("This source is already a DPR2 package.")
      }
      if(dpr_is_dpr1(path)){
        stop("This is a DataPackageR package. Please convert the package to DPR2 using `dpr_convert` instead of using `dpr_create` or `dpr_init`.")
      }
    }
  }
}

#' Private. Updates `.Rbuildignore` and `.gitignore` to use DPR2 practices.
#'
#' @title dpr_update_ignores
#' @param path the package path
#' @noRd
dpr_update_ignores <- function(path){
  rig <- file.path(path, ".Rbuildignore")
  gig <- file.path(path, ".gitignore")
  for(ig in c(rig, gig))
    if(!file.exists(ig))
      file.create(ig)
  writeLines(
    c(
      readLines(rig),
      "^data/.+\\.(r|tab|txt|csv|tsv|rds)$",
      "^inst/extdata/"
    ),
    rig
  )
  writeLines(
    c(
      readLines(gig),
      "inst/doc/"
    ),
    gig
  )
}

#' Private. A function that returns file paths from the installed `templates`
#' directory.
#'
#' @title dpr_get_template
#' @return a character vector
#' @author jmtaylor
#' @param regex a regular expression
#' @noRd
dpr_get_template <- function(regex){
  list.files(system.file("templates", package="DPR2"), regex, full.names = TRUE)
}

#' This function returns default key value pairs for a DPR2 `datapackager.yml`
#' file. These defaults are the same for each package and are read from the
#' DPR2 installation directory.
#'
#' The keys returned here are required for all DPR2 data packages but the users
#' may set valid values to them by using `dpr_yaml_set(...)` or simply editing
#' the `datapackager.yml` file manually. Below is a list of the keys, their
#' default values, and any other valid values when applicable.
#'
#' * source_data_directory : inst/extdata, the location where source data for processing scripts
#' * purge_data_directory  : TRUE, whether to delete all contents of the `data` directory when the package is rendered
#' * process_directory     : processing, where to find the processing scripts to render
#' * process_on_build      : NULL, which processing scripts to run from the `process_directory` location when the package is rendered
#' * render_on_build       : TRUE, whether to render the processing scripts when the package is built or not
#' * render_env_mode       : isolate, valid values are "isolate" or "share" which determine if each processing script is run in its own R session (isolate) or the same R session (share). No option allows processing scripts to be run in the current session.
#' * write_to_vignettes    : TRUE, if vignettes should be generated when rendering
#' * write_docs            : TRUE, if data documentation should be generated when whendering
#' * data_digest_directory : inst/data_digest, the location of the `data_digest` folder.
#' * build_tarball         : FALSE, if a package tarball should be built when the package is built
#' * install_on_build      : FALSE, if the package should be installed when the package is built
#' * build_output          : ../, where the package should be save if the tarball is built
#'
#' @title dpr_yaml_defaults
#' @return a list containing default yaml key-value pairs from `datapackager.yml`
#' @export
dpr_yaml_defaults <- function(){
  return(yaml::read_yaml(dpr_get_template("datapackager.yml$")))
}

#' This function returns default key:value pairs for the data package
#' `DESCRIPTION` file.
#'
#' @title dpr_description_defaults
#' @return a list containing default metadata from the `DESCRIPTION` file
#' @export
dpr_description_defaults <- function(){
  defd <- dpr_get_template("DESCRIPTION")
  return(
    as.list(
      desc::desc_get(desc::desc_fields(defd), defd)
    )
  )
}

#' Private. A function that sets and generates a `DESCRIPTION` file key:values
#' pairs in a new data package.
#'
#' @title dpr_description_init_set
#' @param desc an R desc object containing a list of metadata fields and
#'   corresponding values to set in the `DESCRIPTION` file
#' @param pkgp the package path
#' @noRd
dpr_description_init_set <- function(desc, pkgp){
  do.call(desc::desc_set, c(desc, file = pkgp))
}

#' Private. A function that sets `datapackager.yml` file
#' key:values pairs in a new data package.
#'
#' @title dpr_yaml_init_set
#' @param yml an R yaml object containing user-defined YAML configurations
#' @param pkgp the package path
#' @noRd
dpr_yaml_init_set <- function(yml, pkgp){
  def <- dpr_yaml_defaults()
  new <- utils::modifyList(def, yml, keep.null = TRUE)
  yaml::write_yaml(new, file.path(pkgp, "datapackager.yml"))
}

#' A function that provides a convenient way to initialize a YAML configuration
#' by merging default settings with user specific values to the
#' `datapackager.yml` file's key:value pairs. If no  arguments are provided, it
#' will return the `datapackager.yml` file  default values. See `?dpr_yaml_defaults`
#' for a list of default values, or call this function with no arguments.
#'
#' @title dpr_yaml_init
#' @param ... Arguments are treated as key value pairs for the `datapackager.yml`
#'   file.
#' @return A list of key:value pairs for generating a `datapackager.yml` file.
#' @export
dpr_yaml_init <- function(...){
  utils::modifyList(dpr_yaml_defaults(), list(...), keep.null = TRUE)
}

#' A function that provides a convenient way to initialize a package's
#' `DESCRIPTION` file by merging default settings with user specific values to the
#' `DESCRIPTION` file's key:value pairs. If no arguments are provided, it will
#' return the default values. See `dpr_description_defaults()` for a list of
#' default values, or call this function with no arguments.
#'
#' @title dpr_description_init
#' @param ... Arguments are treated as key value pairs for the package
#'   `DESCRIPTION` file.
#' @return A list of key:value pairs for generating a `DESCRIPTION` file.
#' @author jmtaylor
#' @export
dpr_description_init <- function(...){
  vals <- list(...)
  desc <- dpr_description_defaults()
  ## override defaults and add options with arguments
  utils::modifyList(desc, vals, keep.null = TRUE)
}

#' Initialize a data package by creating a structured directory skeleton for a
#' new R package. Sets up essential directories, initializes package metadata in
#' the `datapackager.yml` and `DESCRIPTION` files as described by [dpr_yaml_init()] and
#' [dpr_description_init()] function calls.
#'
#' @title dpr_create
#' @param path A path to the data package.
#' @param yaml A returned list for [dpr_yaml_init()]
#' @param desc A returned list for [dpr_description_init()]
#' @export
dpr_create <- function(path = ".", yaml = dpr_yaml_init(), desc = dpr_description_init()){
  pkgp <- file.path(path, desc$Package)

  if(!dir.exists(path))
    stop("`path` argument does not point to an existing directory.")

  if(is.null(getOption('dpr2_is_converting'))){
    # need to check both paths in case the user calls init or create from inside
    # an existing data package, where each call has different default paths
    dpr_check_dpr(pkgp) # dpr_create()
    dpr_check_dpr(path) # dpr_init()
  }

  tryCatch(
  {

    ## create package skeleton
    dirs <- c(
      "data", "inst", yaml$process_directory, yaml$source_data_directory, yaml$to_build_directory, "inst/data_digest",
      vapply(c("scripts", "objects"), function(f) file.path(yaml$to_build_directory, f), "")
    )
    dir.create(pkgp, showWarnings = FALSE, recursive = TRUE)

    for( dir in dirs )
      if(!dir.create(file.path(pkgp, dir), showWarnings = FALSE))
        warning(sprintf("`%s` was found, skipping creating that directory.", dir))

    for( fil in c("NAMESPACE", "DESCRIPTION", "datapackager.yml") )
      if(!file.copy(system.file("templates", fil, package="DPR2"), file.path(pkgp, fil)))
        warning(sprintf("`%s` was found, skipping creating that file.", fil))
      else {
        if( fil == "DESCRIPTION" )
          dpr_description_init_set(desc, pkgp)
        if( fil == "datapackager.yml" )
          dpr_yaml_init_set(yaml, pkgp)
      }

    dpr_update_ignores(pkgp)

  },
  error = function(e){
    if(dir.exists(pkgp))
      unlink(pkgp, recursive=TRUE)
    stop(e, traceback())
  })

}

#' A wrapper for `dpr_create` that uses the parent directory as the location to
#' create the data package, setting the package name as the current directory.
#'
#' @title dpr_init
#' @author jmtaylor
#' @param path a path value to init at a specific path. The default
#'   path is the current working directory.
#' @param yaml A returned list from [dpr_yaml_init()]
#' @param desc A returned list from [dpr_description_init()]. The default
#'   argument sets the package name to the name of the directory containing the
#'   data package.
#' @export
dpr_init <- function(
    path = ".",
    yaml = dpr_yaml_init(),
    desc = dpr_description_init(Package = basename(path)))
{
  path <- normalizePath(path)
  dpr_create(
    dirname(path),
    yaml = yaml,
    desc = desc
  )
}
