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
  if (yml$render_env_mode == 'isolate'){
    render_lst <- render_isolate(files_to_process, render_args)
  } else if (yml$render_env_mode == 'share'){
    render_lst <- render_share(files_to_process, render_args)
  }

  # parent.env(env) will be emptyenv(). See ?as.environment
  env <- as.environment(render_lst)

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

##' Private. A render function for callr
##'
##' @param ... arguments passed to the callr function
##' @param mode the return mode of the function, "isolate" returns
##'   objects of the callr global env to be parsed for any save
##'   objects listed in datapackager.yml
##' @return return a list of objects when mode isolate, otherwise return nothing
##' @author jmtaylor
callr_render <- function(..., mode){
  rmarkdown::render(envir = globalenv(), ...)
  if(mode == "isolate")
    return(as.list(globalenv()))
}

#' Private. Isolated rendering in separate R processes with error handling
#'
#' @param files_to_process Character vector of file paths to be rendered
#' @param render_args Named list of arguments to be passed to [rmarkdown::render()]
#'
#' @return list of objects created by render of all processing files
#' @noRd
render_isolate <- function(files_to_process, render_args){
  process_envs <- lapply(files_to_process, function(src){
    callr_args <- c(render_args, input = src, mode = "isolate")
    rs <- callr::r_session$new()
    on.exit(rs$close())
    res <- rs$run_with_output(callr_render, callr_args)
    if (!is.null(res$error)){
      res$error$message <- paste0(res$error$message, res$error$stderr)
      stop(res$error)
    }
    return(res$result)
  })

  # Recombine. Earlier object(s) with same name will be overwritten
  objs <- Reduce(
    function(...) utils::modifyList(..., keep.null = TRUE),
    process_envs
  )

  return(objs)
}

#' Private. Shared rendering mode in a separate R process with error handling
#'
#' @param files_to_process Character vector of file paths to be rendered
#' @param render_args Named list of arguments to be passed to [rmarkdown::render()]
#' @return a list of objects created by render of all processing files
#' @noRd
render_share <- function(files_to_process, render_args){
  rs <- callr::r_session$new()
  on.exit(rs$close())

  for(src in files_to_process) {
    # Earlier object(s) with same name will be overwritten
    callr_args <- c(render_args, input = src, mode = "share")
    res <- rs$run_with_output(callr_render, callr_args)
    if (!is.null(res$error)) {
      res$error$message <- paste0(res$error$message, res$error$stderr)
      stop(res$error)
    }
  }

  objs <- rs$run(function() as.list(globalenv()))
  return(objs)
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
