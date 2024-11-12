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

  if(is.null(yml$process_on_build)){
    stop("No files specified to process_on_build. See datapackager.yml file.")
  }

  # Prepare to render
  src_vec = file.path(path, yml$process_directory, yml$process_on_build)
  render_args <- list(
    knit_root_dir = normalizePath(path),
    output_dir = ifelse(yml$write_to_vignettes, file.path(path, "vignettes"), tempdir()),
    output_format = "md_document"
  )
  render_fn <- switch(yml$render_env_mode,
                      share = render_shared,
                      isolate = render_isolate)

  # render and convert to environment
  env <- as.environment(render_fn(src_vec, render_args))

  saved_objects <- character()
  for( obj in intersect(ls(env), yml$objects) ){
    dpr_save(obj, path, envir = env)
    saved_objects <- c(saved_objects, obj)
  }

  missed_objects <- setdiff(yml$objects, saved_objects)
  if(length(missed_objects) != 0)
    warning(
      sprintf(
        "Objects listed in yaml not found in processing scripts to save to data directory: %s",
        paste(missed_objects, collapse = ", ")
      )
    )
}

#' Private. Isolated rendering in separate R processes with error handling
#'
#' @param src_vec Character vector of file paths to be rendered
#' @param render_args Named list of arguments to be passed to [rmarkdown::render()]
#'
#' @return list of objects created by render of all processing files
#' @noRd
render_isolate <- function(src_vec, render_args){
  lst_each_process <- lapply(src_vec, function(src){
    callr_args <- c(render_args, input = src)
    callr_fn <- function(...){
      rmarkdown::render(envir = globalenv(), ...)
      as.list(globalenv())
    }
    rs <- callr::r_session$new()
    on.exit(rs$close())
    res <- rs$run_with_output(callr_fn, callr_args)
    if (! is.null(res$error)){
      on.exit(print(res$error), add = TRUE)
      stop(res$error)
    }
    res$result
  })
  # Recombine. Earlier object(s) with same name will be overwritten
  lst_all_process <- Reduce(
    function(...) utils::modifyList(..., keep.null = TRUE),
    lst_each_process, simplify = FALSE
  )
}

#' Private. Shared rendering mode in a separate R process with error handling
#'
#' @param src_vec Character vector of file paths to be rendered
#' @param render_args Named list of arguments to be passed to [rmarkdown::render()]
#' @returns list of objects created by render of all processing files
#' @noRd
render_shared <- function(src_vec, render_args){
  rs <- callr::r_session$new()
  on.exit(rs$close())
  lapply(src_vec, function(src){
    callr_args <- c(render_args, input = src)
    callr_fn <- function(...) rmarkdown::render(envir = globalenv(), ...)
    res <- rs$run_with_output(callr_fn, callr_args)
    if (! is.null(res$error)){
      on.exit(print(res$error))
      stop(res$error)
    }
  })
  rs$run(function() as.list(globalenv()))
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
