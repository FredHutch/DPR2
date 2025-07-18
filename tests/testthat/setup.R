copyPkg <- function(pkg){
  path <- file.path(tempdir(), pkg)
  file.copy(test_path(pkg), tempdir(), recursive = TRUE)
  suppressWarnings(
    dpr_init(path)
  )
  return(path)
}
