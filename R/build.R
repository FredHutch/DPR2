##' Private. A function for purging the data directory.
##'
##' @title dpr_purge_data_directory
##' @param path A character string specifying the directory path of the data package. The default is the
##'   working directory
##' @param yml an R yaml list object
##' @return nothing
##' @author jmtaylor
dpr_purge_data_directory <- function(path=".", yml){
  for(d in list.files(file.path(path, yml$data_directory))){
    unlink(file.path(path, yml$data_directory, d), recursive=TRUE)
  }
}

##' Private. A function for updating the data digest components.
##'
##' @title dpr_update_data_digest
##' @param path A character string specifying the directory path of the data package. The default is the
##'   working directory
##' @param yml an R yaml list object
##' @return nothing
##' @author jmtaylor
dpr_update_data_digest <- function(path=".", yml){
  dat <- list.files(file.path(path, "data"), full.names = T)
  dig <- file.path(path, yml$data_digest_directory)
  if(!dir.exists(dig))
    stop(sprintf("Data digest directory does not exist: %s", dig))
  for(d in dat)
    write(
      digest::digest(d, file=T, algo="sha1"),
      file.path(
        dig,
        tools::file_path_sans_ext(basename(d))
      )
    )
}

##' The dpr_render function process and render all processing scripts defined in the datapackager.yml configuration file. Does
##' not build or install the data package. For full package build and installation, use the dpr_build function.
##'
##'
##' @title dpr_render
##' @param path A character string specifying the directory path of the data package. The default is the
##'   working directory.
##' @param ... Additional arguments that overrides datapackager.yml default configuration values.
##'
##' @return does not return anything but performs rendering, processing and data-saving operation defined in configuration file
##' @author jmtaylor
##' @export
dpr_render <- function(path=".", ...){
  yml <- DPR2::dpr_yaml_get(path, ...)

  if(yml$purge_data_directory)
    dpr_purge_data_directory(path, yml)

  mode <- yml$render_env_mode

  if(mode == "share")
    env <- new.env()

  for(src in yml$process_on_build){
    if(dir.exists(file.path(path, yml$process_directory, src)))
      stop("Are any processes set to build? See datapackager.yml file.")

    if(mode == "isolate")
      env <- new.env()

    tryCatch({
      rmarkdown::render(
        input = file.path(path, yml$process_directory, src),
        knit_root_dir = normalizePath(path),
        output_dir = { if(yml$write_to_vignettes) file.path(path, "vignettes") else tempdir() },
        output_format = "md_document",
        envir = env,
        quiet = TRUE
      )

      if( !is.null(yml$objects) ){
        for( obj in yml$objects ){
          if( exists(obj, envir=env) ){
            assign(obj, get(obj, envir=env))
            save(obj, file=file.path(path, yml$data_directory, paste0(substitute(obj), ".rda")))
          } else {
            warning("Objects listed in yaml not found in processing scripts to save to data directory.")
          }
        }
      }

    },
    error = function(e) stop(sprintf("dpr_render() failed: %s \n", e$message))
    )
  }
}

##' The dpr_build function process, render and build data package. Uses a special environment,
##' `dpr_build_env`, for the evaluation environment. `dpr_build_env`
##' is removed from the .GlobalEnv once complete.
##'
##' @title dpr_build
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return does not return any value. It performs rendering, building and installing the package based on the configuration
##' @author jmtaylor
##' @export
dpr_build <- function(path=".", ...){
  tryCatch(
    expr = {
      yml <- DPR2::dpr_yaml_get(path, ...)

      if(yml$render_on_build)
        dpr_render(path, ...)

      if("data_digest_directory" %in% names(yml))
        dpr_update_data_digest(path, yml)

      if(yml$build_tarball)
        pkgp <- pkgbuild::build(
          path = path,
          dest_path = file.path(path, yml$build_output)
        )

      if(yml$install_on_build)
        if(yml$build_tarball){
          utils::install.packages(pkgp, repo=NULL)
        } else {
          pkgp <- pkgbuild::build(
            path = path,
            dest_path = tempdir()
          )
          utils::install.packages(pkgp, repo=NULL)
        }

    },
    error = function(e) stop(sprintf("dpr_build() failed: %s \n", e$message))

  )
}
