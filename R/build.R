dpr_purge_data_directory <- function(yml){
  for(d in list.files(file.path(yml$package_root, yml$data_directory))){
    unlink(file.path(yml$package_root, yml$data_directory, d), recursive=TRUE)
  }
}

dpr_update_data_digest <- function(yml){
  dat <- list.files(file.path(yml$package_root, "data"))
  for(d in dat){
    write(
      digest::sha1(d),
      file.path(
        yml[["data_digest_directory"]],
        tools::file_path_sans_ext(basename(d))
      )
    )
  }
}

##' Render all processing scripts in the for the data package. Does not build. 
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_render <- function(...){
  yml <- DPR2::dpr_yml_get(...) 
  if(yml$purge_data_directory) dpr_purge_data_directory(yml)
  for(src in yml$process_on_build){
    ## knitr::knit or rmarkdown::render?
    rmarkdown::render(
      input = file.path(yml$package_root, yml$process_directory, src),
      knit_root_dir = normalizePath(yml$package_root),
      output_dir = { if(yml$write_to_vignettes) file.path(yml$package_root, "vignettes") else tempdir() },
      output_format = "md_document",
      envir = { if(exists("dpr_build_env", .GlobalEnv)) .GlobalEnv$dpr_build_env else parent.frame() }
    )
  }
}

dpr_install <- function(...){
  
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_build <- function(...){
  tryCatch(
  expr = {
    assign("dpr_build_env", new.env(), envir = .GlobalEnv)
    assign("yml", DPR2::dpr_yml_get(...), envir = dpr_build_env)
    if(dpr_build_env$yml$render_on_build)
      dpr_render(...)
    dpr_update_data_digest(dpr_build_env$yml)
    pkgbuild::build(dpr_build_env$yml$package_root, dpr_build_env$yml$build_output)
    if(dpr_build_env$yml$install_on_build){
      dpr_install(dpr_build_env$yml)
    }
  },
  error = \(e) message(sprintf("DPR2 build failed. %s", e)),
  finally = rm("dpr_build_env") 
  )
}

