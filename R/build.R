##' Private. A function for purging the data directory.
##'
##' @title dpr_purge_data_directory
##' @param path the package path
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
##' @param path the package path 
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

##' Render all processing scripts in the for the data package. Does
##' not build. Using the `dpr_render()` calling environment. For
##' evaluation.
##'
##' @title dpr_render 
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_render <- function(path=".", ...){
  yml <- DPR2::dpr_yaml_get(path, ...) 
  if(yml$purge_data_directory) dpr_purge_data_directory(path, yml)
  for(src in yml$process_on_build){
    ## knitr::knit or rmarkdown::render?
    tryCatch({
      rmarkdown::render(
        input = file.path(path, yml$process_directory, src),
        knit_root_dir = normalizePath(path),
        output_dir = { if(yml$write_to_vignettes) file.path(path, "vignettes") else tempdir() },
        output_format = "md_document",
        envir = { if(exists("dpr_build_env", .GlobalEnv)) .GlobalEnv$dpr_build_env else parent.frame() },
        quiet = TRUE
      )
    },
    error = \(e) stop(sprintf("dpr_render() failed: %s \n", e$message))
    )    
  }
}

##' Render and build data package. Uses a special environment,
##' `dpr_build_env`, for the evaluation environment. `dpr_build_env`
##' is removed from the .GlobalEnv once complete. 
##'
##' @title dpr_build 
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_build <- function(path=".", ...){
  tryCatch(
    expr = {
      assign("dpr_build_env", new.env(), envir = .GlobalEnv)
      assign("yml", DPR2::dpr_yaml_get(path, ...), envir = .GlobalEnv$dpr_build_env)
      yml <- .GlobalEnv$dpr_build_env$yml

      if(yml$render_on_build)
        dpr_render(path, ...)

      if("data_digest_directory" %in% names(yml))
        dpr_update_data_digest(path, yml)

      if(yml$build_tarball)
        pkgp <- pkgbuild::build(path=path, dest_path=file.path(path, yml$build_output))

      if(yml$install_on_build)
        install.packages(pkgp, repo=NULL)

    },
    error = \(e) stop(sprintf("dpr_build() failed: %s \n", e$message)),
    finally = rm(list="dpr_build_env", envir=.GlobalEnv) 
  )
}
