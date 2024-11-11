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

  if(identical(yml$process_on_build, '')){
    stop("Are any processes set to build? See datapackager.yml file.")
  }

  # Prepare to render
  mode <- yml$render_env_mode
  src_vec = file.path(path, yml$process_directory, yml$process_on_build)
  render_args <- list(
    knit_root_dir = normalizePath(path),
    output_dir = { if(yml$write_to_vignettes) file.path(path, "vignettes") else tempdir() },
    output_format = "md_document"
  )

  # Shared rendering environment in a clean R process
  if (mode == 'share'){
    lst_all_process <- render_shared(src_vec, render_args)
  }

  # Isolated rendering in separate clean R processes
  if (mode == 'isolate'){
    lst_each_process <- lapply(src_vec, function(src){
      callr_args <- c(render_args, input = src)
      callr_fn <- function(...){
        rmarkdown::render(envir = globalenv(), ...)
        as.list(globalenv())
      }
      stderr_file <- tempfile()
      on.exit(extract_rmarkdown_render_stderr(stderr_file))
      callr::r(callr_fn, callr_args, stderr = stderr_file)
    })
    # Recombine. Earlier object(s) with same name will be overwritten
    lst_all_process <- Reduce(
      function(...) utils::modifyList(..., keep.null = TRUE),
      lst_each_process, simplify = FALSE
    )
  }

  env <- as.environment(lst_all_process)
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

#' Private. Extract rmarkdown::render() error message from stderr file, then
#' delete the file.
#'
#' @param stderr_file File to which rmarkdown::render() stderr has been written
#'   by callr. File will be deleted after scraping for error message.
#' @noRd
extract_rmarkdown_render_stderr <- function(stderr_file){
  # Fight rmarkdown::render() to get its error message...
  on.exit(if (file.exists(stderr_file)) file.remove(stderr_file))
  erln <- readLines(stderr_file)
  qln  <- grepl('^Quitting', erln)
  if (any(qln)){
    err <- paste('in dpr_render() from rmarkdown::render():', erln[qln])
    stop(err, call. = FALSE)
  }
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
