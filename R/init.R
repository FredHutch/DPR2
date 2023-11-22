dpr_yml_init <- function(...){
  yml_set <- list(...)
  yml <- list(
    "build_output"                 = "../",
    "source_data_directory"        = "inst/extdata",
    "process_directory"            = "processing",
    "process_on_build"             = c(),
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

dpr_package_init <- function(...){
  ## build package skeleton
  yml <- dpr_yml_get_default(...)

  ## write yml
  yaml::write_yaml(yml, file.path(here::here(), "datapackager.yml"))
}

