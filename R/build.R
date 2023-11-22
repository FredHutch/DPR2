dpr_purge_data_directory <- function(yml){
  for(d in list.files(file.path(yml$package_root, "data"))){
    unlink(d)
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

dpr_render <- function(yml){
  if(yml$purge_data_directory) dpr_purge_data_directory(yml)
  for(src in yml$process_on_build){
    ## knitr::knit or rmarkdown::render?
    rmarkdown::render(
      src,
      knit_root_dir = yml$package_root,
      output_dir = file.path(yml$package_root, "vignettes"),
      output_format = "md_document"
    )
  }
}

dpr_install <- function(yml){
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
  {
    assign("dpr_build_env", new.env(parent=.GlobalEnv))
    assign("yml", DPR2::dpr_yml_get(...), envir = dpr_build_env)
    dpr_render(dpr_build_env$yml)
    dpr_update_data_digest(dpr_build_env$yml)
    ## could use functions in callr, or processx, or sys. 
    system(
      paste("cd ", dpr_build_env$yml$build_ouput, " && R CMD build ", dpr_build_env$yml$package_root)
    )
    if(dpr_build_env$yml$install){
      dpr_install(dpr_build_env$yml)
    }
  },
  error = \(e) message(sprintf("DPR2 build failed. %s", e)),
  finally = rm(dpr_build_env)
  )
}

