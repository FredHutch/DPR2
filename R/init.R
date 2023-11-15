datapackage.yml <- function(){
  args <- list(
    "package_root"                 = "./",
    "build_output"                 = "../",
    "source_data_directory"        = "inst/extdata",
    "process_directory"            = "processing",
    "process_on_build"             = c(),
    "write_to_vignettes"           = TRUE,
    "auto_increment_data_versions" = TRUE
  )

  ## replace values for "process_on_build" with all values in "pricess_directory"
  args[["process_on_build"]] <- list.files(args[["process_directory"]])

  return(args)
}

dpr_init_package <- function(){
  ## build package skeleton
  ## write datapackager.yml from datapackagr.yml()
}

