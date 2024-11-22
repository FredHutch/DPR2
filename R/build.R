##' Private. A function for purging the data directory.
##'
##' @title dpr_purge_data_directory
##' @param path the package path
##' @param yml an R yaml list object
##' @return nothing
##' @author jmtaylor
dpr_purge_data_directory <- function(path=".", yml){
  datadir <- file.path(path, "data")
  for(d in list.files(datadir)){
    unlink(file.path(datadir, d), recursive=TRUE)
  }
}

##' Private. A function for fetch all global objects from a callr session.
##'
##' @param session a callr session
##' @return a list of objects
callr_get_objects <- function(session){
  return(session$run(function() as.list(globalenv())))
}

#' Private. Shared rendering mode in a separate R process with error handling
#'
#' @param files_to_process Character vector of file paths to be rendered
#' @param process_args Named list of arguments to be passed to [rmarkdown::render()]
#' @param mode The render environment mode from datapackager yaml render_env_mode
#' @return A list of objects created by all of the  processing files
#' @noRd
callr_render <- function(files_to_process, render_args, mode){
  rs <- callr::r_session$new()
  on.exit(rs$close())

  if(mode == "isolate")
    objs <- list()

  for(src in files_to_process) {

    res <- rs$run_with_output(
      function(...) rmarkdown::render(envir = globalenv(), ...),
      c(render_args, input = src)
    )
    if (!is.null(res$error)) {
      res$error$message <- paste0(res$error$message, res$error$stderr)
      stop(res$error)
    }

    if(mode == "isolate"){
      # Earlier object(s) with same name will be overwritten
      objs <- modifyList(objs, callr_get_objects(rs))
      rs$close()
      rs <- callr::r_session$new()
    }

  }

  if(mode == "share")
    objs <- callr_get_objects(rs)

  return(objs)
}

##' Render all processing scripts in the for the data package. Does
##' not build. Using the `dpr_render()` calling environment. For
##' evaluation.
##'
##' @title dpr_render
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_render <- function(path=".", ...){
  if( dpr_is_dpr1(path) )
    warning("Rendering a data package using DPR2 when the yaml is from DataPackageR. See docs for list of side effects.")

  yml <- dpr_yaml_get(path, ...)

  if(yml$purge_data_directory)
    dpr_purge_data_directory(path, yml)

  # Prepare to render
  if(is.null(yml$process_on_build)){
    stop("No files specified to process_on_build. See datapackager.yml file.")
  }
  files_to_process = file.path(path, yml$process_directory, yml$process_on_build)
  render_args <- list(
    knit_root_dir = normalizePath(path),
    output_dir = ifelse(yml$write_to_vignettes, file.path(path, "vignettes"), tempdir()),
    output_format = "md_document"
  )

  # render and convert to environment
  # parent.env(env) will be emptyenv(). See ?as.environment
  objects <- callr_render(files_to_process, render_args, yml$render_env_mode)
  env <- as.environment(objects)

  saved_objects <- dpr_save(
    intersect(ls(env), as.character(yml$objects)), path, env
  )

  missed_objects <- setdiff(yml$objects, saved_objects)
  if(length(missed_objects) != 0)
    warning(
      sprintf(
        "Objects listed in yaml not found in processing scripts to save to data directory: %s",
        paste(missed_objects, collapse = ", ")
      )
    )
}

##' Render and build data package. Uses a special environment,
##' `dpr_build_env`, for the evaluation environment. `dpr_build_env`
##' is removed from the .GlobalEnv once complete.
##'
##' @title dpr_build
##' @param path The relative path to the data package. The default is the
##'   working directory.
##' @param ... datapackager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_build <- function(path=".", ...){
  quiet <- identical(Sys.getenv("TESTTHAT"), "true")
  yml <- dpr_yaml_get(path, ...)

  if(yml$render_on_build)
    dpr_render(path, ...)

  if("data_digest_directory" %in% names(yml))
    dpr_update_data_digest(path, yml)

  if(yml$build_tarball)
    pkgp <- pkgbuild::build(
      path = path,
      dest_path = file.path(path, yml$build_output),
      quiet = quiet
    )

  if(yml$install_on_build){
    if(yml$build_tarball){
      utils::install.packages(pkgp, repo=NULL, quiet = quiet)
    } else {
      pkgp <- pkgbuild::build(
        path = path,
        dest_path = tempdir(),
        quiet = quiet
      )
      utils::install.packages(pkgp, repo=NULL, quiet = quiet)
    }
  }
}
