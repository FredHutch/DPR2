#' Private. A function for purging the data directory.
#'
#' @title dpr_purge_data_directory
#' @param path A character string specifying the directory path of the data
#'   package. The default is the working directory
#' @return Invisibly, a vector of paths of backup copies of purged files
#' @author jmtaylor
#' @noRd
dpr_purge_data_directory <- function(path="."){
  datadir <- file.path(path, "data")
  files_to_purge <- file.path(datadir, list.files(datadir))

  backup_dir <- tempfile()
  dir.create(backup_dir)
  file.copy(files_to_purge, backup_dir)

  unlink(files_to_purge, recursive=TRUE)

  not_deleted <- file.exists(files_to_purge)
  if (any(not_deleted)){
    stop(
      sprintf(
        'Error purging %s', paste(basename(files_to_purge[not_deleted]), collapse = ', ')
      )
    )
  }

  invisible(list.files(backup_dir, full.names = TRUE))
}

#' Private. A function for making vignette files from rendered processing scripts.
#'
#' @param path the data package path to save the processed vignettes to.
#' @param processing_dir the location processing scripts are in.
#' @param vignette_tempdir the temp location vignettes were saved to.
#' @return No return value. Modifies vignette files as a side effect.
#' @noRd
process_vignettes <- function(path, processing_dir, vignette_tempdir){
  vignettes_dir <- file.path(path, 'vignettes')
  if (!dir.exists(vignettes_dir)) dir.create(vignettes_dir)

  srcs <- list.files(file.path(path, processing_dir), full.names = TRUE)
  file.copy(
    list.files(vignette_tempdir, full.names = TRUE),
    vignettes_dir,
    recursive = TRUE
  )

  md_files <- list.files(vignettes_dir, pattern = '\\.md$', full.names = TRUE)
  file.rename(md_files, sub('\\.md$', '.Rmd', md_files))

  rmds <- list.files(vignettes_dir, pattern = '\\.Rmd$', full.names = TRUE)
  for(vig in rmds){
    lins <- readLines(vig)
    vignette_yml <- "%%\\VignetteIndexEntry{%s}\n%%\\VignetteEngine{knitr::rmarkdown}\n%%\\VignetteEncoding{UTF-8}\n"

    if (tolower(basename(vig)) %in% tolower(basename(srcs))) { # to check if the vignette was originally an rmd file and that it has a yaml header
      src <- readLines(srcs[ tolower(basename(srcs)) %in% tolower(basename(vig)) ])

      yml_idx <- which(grepl("^---$", src))
      if(length(yml_idx)== 0)  yml_idx <- c(0, 0)
      rmd_yml <- yaml::read_yaml(text = src[seq_len(yml_idx[2])])

      rmd_yml$vignette <-
        sprintf(
          vignette_yml,
          ifelse(!is.null(rmd_yml$title), rmd_yml$title, gsub("\\.rmd$", "", basename(src), ignore.case = T))
        )

    } else {
      rmd_yml <- list(title = basename(vig), vignette = sprintf(vignette_yml, basename(vig)))
    }
    new_yml <- c("---", unlist(strsplit(yaml::as.yaml(rmd_yml), "\\n")), "---")
    writeLines(c(new_yml, lins), vig)
  }
}

#' Private. A function to fetch all global objects from a callr session.
#'
#' @param session a callr session object
#' @return a list of objects in the global environment
#' @noRd
get_callr_globals <- function(session){
  return(session$run(function() as.list(globalenv())))
}

#' Private. Render in separate callr R process(es) with error handling.
#'
#' @param files_to_process Character vector of file paths to be rendered
#' @param render_args Named list of arguments to be passed to
#'   [rmarkdown::render()]
#' @param render_mode The render environment mode from datapackager.yml
#'   `render_env_mode`
#' @param r_session_wait_timeout Number of milliseconds to wait for r session
#' @return A list of objects created by all of the processing files
#' @noRd
callr_render <- function(files_to_process, render_args, render_mode, r_session_wait_timeout){
  rs <- callr::r_session$new(wait=TRUE, wait_timeout=r_session_wait_timeout)
  on.exit(rs$close())

  if (render_mode == "isolate") objs <- list()

  for (file_to_process in files_to_process) {

    # If in share mode, earlier object(s) with same name are overwritten here
    res <- rs$run_with_output(
      function(...) rmarkdown::render(envir = globalenv(), ...),
      c(render_args, input = file_to_process)
    )

    if (!is.null(res$error)) {
      # Include useful debugging info from stderr in the actual error msg
      res$error$message <- paste0(res$error$message, res$error$stderr)
      stop(res$error)
    }

    if (render_mode == "isolate"){
      # Earlier object(s) with same name are overwritten here
      objs[names(get_callr_globals(rs))] <- get_callr_globals(rs)
      if ( file_to_process != files_to_process[ length(files_to_process) ] ){
        rs$close()
        rs <- callr::r_session$new(wait=TRUE, wait_timeout=r_session_wait_timeout)
      }
    }

  }

  if (render_mode == "share")
    objs <- get_callr_globals(rs)

  return(objs)
}

#' Process and render all processing scripts defined in the `datapackager.yml`
#' configuration file. Does not build or install the data package. For full
#' package build and installation, use the `dpr_build` function.
#'
#' `dpr_render` is the primary process that renders processing
#' scripts to vignettes and data. This function can be run in two modes:
#' isolate or share. This mode is set in the `datapackager.yml` file's
#' `render_env_mode`'s value.  When the `isolate` mode is set, each
#' processing script is run in a separate R session. When the `share`
#' mode is set, each process is run in the same session, which enables each
#' process to use variables defined by previous processing scripts for the
#' current `dpr_render` call.
#' @title dpr_render
#' @param path The relative path to the data package. The default is the working
#'   directory.
#' @param ... `datapackager.yml` value overrides. When arguments are
#'   specified, those arguments are used as the YAML key value pairs instead of
#'   what is specified by the `datapackager.yml` file. For a list of those
#'   key value pairs and their purposes, see `?dpr_yaml_defaults`.
#' @return does not return anything but performs rendering, processing and
#'   data-saving operations as defined in `datapackager.yml`
#' @author jmtaylor
#' @export
dpr_render <- function(path=".", ...){
  if( dpr_is_dpr1(path) )
    stop("Rendering a data package using DPR2 when the yaml is from DataPackageR. Must use `dpr_convert()` first. See `?dpr_convert`.")

  yml <- dpr_yaml_get(path, ...)

  if(yml$purge_data_directory){
    purge_backup_files <- dpr_purge_data_directory(path)
    purge_restore <- TRUE
    on.exit({
      # conditionally restore from backup
      if (purge_restore) file.copy(purge_backup_files, file.path(path, 'data'))
      # always unlink backup files
      unlink(unique(dirname(purge_backup_files)), recursive = TRUE)
    })
  }

  # Prepare to render
  if(is.null(yml$process_on_build) | length(yml$process_on_build) == 0){
    stop("No files specified to process in `to_build/scripts`. See `?dpr_add_scripts`.")
  }

  vignette_tempdir <- tempfile()
  dir.create(vignette_tempdir)

  render_args <- list(
    knit_root_dir = normalizePath(path),
    output_dir = vignette_tempdir,
    output_format = "md_document"
  )

  # render and convert to environment
  objects <- callr_render(
    file.path(path, yml$process_directory, yml$process_on_build),
    render_args,
    yml$render_env_mode,
    yml$r_session_wait_timeout
  )

  # parent.env(env) will be emptyenv(). See ?as.environment
  env <- as.environment(objects)

  # transfer vignettes to vignettes/ if desired, now that we're done rendering
  if (yml$write_to_vignettes){
    process_vignettes(path, yml$process_directory, vignette_tempdir)
  }

  # now safe to cancel purge restore on exit
  if(yml$purge_data_directory){
    purge_restore <- FALSE
  }

  saved_objects <- dpr_save(
    intersect(ls(env), as.character(yml$objects)), path, env
  )

  missed_objects <- setdiff(yml$objects, saved_objects)
  if(length(missed_objects) != 0)
    warning(
      sprintf(
        "Objects to build not found in processing evaluation environment to save to data directory: %s",
        paste(missed_objects, collapse = ", ")
      )
    )

  if(yml$write_data_docs){
    if(!data_is_empty(path=path)){
      generate_all_docs(path=path)
      local({
        # clean up search path mess left behind by roxygen2::roxygenize()
        on.exit({
          if (basename(path) %in% names(utils::sessionInfo()$otherPkgs)){
            pkgload::unload(basename(path))
          }
        })
        suppressPackageStartupMessages(
          suppressMessages(
            roxygen2::roxygenize(path)
          )
        )
      })
    }
  }

  if(!yml$write_data_docs & length(saved_objects) > 0){
    warning("`write_data_docs` is set to FALSE. Objects are being saved, but data documentation is not being updated. Existing documentation may be outdated.")
  }
}

#' Build the data package. This includes processing and rendering all processing
#' scripts defined in the `datapackager.yml` configuration file and
#' additionally will build and install the data package if configured to do so
#' in the `datapackager.yml`.
#'
#' `dpr_build` wraps many DPR2 processes in a single call: renders
#' processing scripts, updates data digest, builds package to an installable
#' tarball, and installs the tarball. Each of these processes can be
#' controlled from the `datapackager.yml` file. Only the processing script
#' rendering function is exported to users. See `dpr_render` for more
#' information regarding rendering. For more information regarding
#' configuration options, see `?dpr_yaml_defaults`.
#' @title dpr_build
#' @param path The relative path to the data package. The default is the working
#'   directory.
#' @param ... `datapakager.yml` value overrides. When arguments are
#'   specified, those arguments are used as the YAML key value pairs instead of
#'   what is specified by the `datapackager.yml` file. For a list of those
#'   key value pairs and their purposes, see `?dpr_yaml_defaults`.
#' @return does not return anything but performs rendering, processing and
#'   data-saving operations as defined in datapackager.yml`
#' @author jmtaylor
#' @export
dpr_build <- function(path=".", ...){
  quiet <- identical(Sys.getenv("TESTTHAT"), "true")
  if( dpr_is_dpr1(path) )
    stop("Rendering a data package using DPR2 when the yaml is from DataPackageR. Must use `dpr_convert()` first. See `?dpr_convert`.")

  yml <- dpr_yaml_get(path, ...)

  if(yml$render_on_build)
    dpr_render(path, ...)

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
