copyPkg <- function(pkg){
  path <- file.path(tempdir(), pkg)
  file.copy(test_path(pkg), tempdir(), recursive = TRUE)
  suppressWarnings(
    dpr_init(path, dpr_yaml_init(purge_data_directory = TRUE))
  )
  return(path)
}
