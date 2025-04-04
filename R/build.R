#' Private. A function for purging the data directory.
#'
#' @title dpr_purge_data_directory
#' @param path A character string specifying the directory path of the data
#'   package. The default is the working directory
#' @param yml an R yaml list object
#' @return Invisibly, a vector of paths of backup copies of purged files
#' @author jmtaylor
#' @noRd
dpr_purge_data_directory <- function(path=".", yml){
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

#' Private. A function for making vingette files from rendered processing scripts.
#'
#' @param path the data package path to save the processed vignettes to.
#' @param temp the temp location vignettes were saved to.
#' @noRd
process_vignettes <- function(path, processing_dir, vignette_tempdir){
  vignettes_dir <- file.path(path, 'vignettes')
  if (!dir.exists(vignettes_dir)) dir.create(vignettes_dir)

  srcs <- list.files( file.path(path, processing_dir), full.names = TRUE )
  mds  <- list.files( vignette_tempdir, full.names = TRUE )
  vins <- file.path( path, "vignettes", gsub("\\.md$", ".rmd", basename(mds)) )

  file.copy(mds, vins)

  for(vin in vins){
    lins <- readLines(vin)
    vignette_yml <- "%%\\VignetteIndexEntry{%s}\n%%\\VignetteEngine{knitr::rmarkdown}\n%%\\VignetteEncoding{UTF-8}\n"

    if (tolower(basename(vin)) %in% tolower(basename(srcs))) { # to check if the vinette was originally an rmd file and that it has a yaml header
      src <- readLines(srcs[ tolower(basename(srcs)) %in% tolower(basename(vin)) ])

      yml_idx <- which(grepl("^---$", src))
      if(length(yml_idx)== 0)  yml_idx <- c(0, 0)
      rmd_yml <- yaml::read_yaml(text = src[seq_len(yml_idx[2])])

      rmd_yml$vignette <-
        sprintf(
          vignette_yml,
          ifelse(!is.null(rmd_yml$title), rmd_yml$title, gsub("\\.rmd$", "", basename(src), ignore.case = T))
        )

    } else {
      rmd_yml <- list(title = basename(vin), vignette = sprintf(vignette_yml, basename(vin)))
    }
    new_yml <- c("---", unlist(strsplit(yaml::as.yaml(rmd_yml), "\\n")), "---")
    writeLines(c(new_yml, lins), vin)
  }
}

#' Private. A function for fetch all global objects from a callr session.
#'
#' @param session a callr session
#' @return a list of objects
#' @noRd
get_callr_globals <- function(session){
  return(session$run(function() as.list(globalenv())))
}

#' Private. Render in separate callr R process(es) with error handling
#'
#' @param files_to_process Character vector of file paths to be rendered
#' @param process_args Named list of arguments to be passed to
#'   [rmarkdown::render()]
#' @param render_mode The render environment mode from datapackager.yml
#'   `render_env_mode`
#' @return A list of objects created by all of the processing files
#' @noRd
callr_render <- function(files_to_process, render_args, render_mode){
  rs <- callr::r_session$new()
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
      objs <- utils::modifyList(objs, get_callr_globals(rs), keep.null = TRUE)
      rs$close()
      rs <- callr::r_session$new()
    }

  }

  if (render_mode == "share")
    objs <- get_callr_globals(rs)

  return(objs)
}

#' Process and render all processing scripts defined in the `datapackager.yml`
#' configuration file. Does not build or install the data package. For full
#' package build and installation, use the dpr_build function.
#'
#' `dpr_render` is the primary process that renders processing
#' scripts to vignettes and data. This function can be run in two modes:
#' isolate or share. This mode is set in the `datapackager.yml` file's
#' `render_env_mode`'s value.  When the `isolate` mode is set, each
#' processing script is run in a seperate R session. When the `share`
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
    purge_backup_files <- dpr_purge_data_directory(path, yml)
    purge_restore <- TRUE
    on.exit({
      # conditionally restore from backup
      if (purge_restore) file.copy(purge_backup_files, file.path(path, 'data'))
      # always unlink backup files
      unlink(unique(dirname(purge_backup_files)), recursive = TRUE)
    })
  }

  # Prepare to render
  if(is.null(yml$process_on_build)){
    stop("No files specified to process_on_build. See datapackager.yml file.")
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
    yml$render_env_mode
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
        "Objects listed in yaml not found in processing scripts to save to data directory: %s",
        paste(missed_objects, collapse = ", ")
      )
    )

  if(yml$write_docs){
    if(!data_is_empty(path=path)){
      generate_all_docs(path=path)
    }
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
#' controlled from the `datapackager.yml` file. Only th processing script
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
