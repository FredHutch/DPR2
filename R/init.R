dpr_yml_init <- function(...){
  yml_set <- list(...)
  yml <- list(
    "project_root"                 = "./",
    "build_output"                 = "../",
    "data_directory"               = "data",
    "source_data_directory"        = "inst/extdata",
    "install_on_build"             = TRUE,
    "process_directory"            = "processing",
    "process_on_build"             = c(),
    "render_on_build"              = TRUE,
    "write_to_vignettes"           = TRUE,
    "auto_increment_data_versions" = TRUE,
    "purge_data_directory"         = TRUE,
    "data_digest_directory"        = "inst"
  )

  ## override defaults and add options with arguments
  for( a in names(yml_set) )
    yml[[a]] <- yml_set[[a]]
  
  return(yml)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param path 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_init <- function(path = "./", ...){

  tryCatch(
    {
      yml <- dpr_yml_init(...)

    ## build package skeleton
    devtools::create(path)

    dirs <- c("data", "inst", yml$process_directory, yml$source_data_directory)
    for( d in dirs )
      dir.create(file.path(path, d))
    
    ## write yml
      yaml::write_yaml(yml, file.path(path, "datapackager.yml"))
    },
    error = \(e) message(paste("There was an error.", e, collapse = "\n"))
  )
    
}

