datapackager.yml <- function(...){
  add_args <- list(...)
  args <- list(
    "package_root"                 = "./",
    "build_output"                 = "../",
    "source_data_directory"        = "inst/extdata",
    "process_directory"            = "processing",
    "process_on_build"             = c(),
    "write_to_vignettes"           = TRUE,
    "auto_increment_data_versions" = TRUE
  ) |>
    populate_process_on_build()

  for(a in names(add_args))
    args[[a]] <- add_args[[a]]
  
  return(args)
}

populate_process_on_build <- function(args){
    args[["process_on_build"]] <- list.files(args[["process_directory"]])
    return(args)
}

dpr_init_package <- function(dpr_yml){
  ## build package skeleton
  ## write datapackager.yml from datapackager.yml()
  args <- datapackager.yml()
  browser()
  yaml::write_yaml(args, file.path(args[["package_root"]], "datapackager.yml"))
}

