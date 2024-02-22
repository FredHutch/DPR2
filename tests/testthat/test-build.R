tdir <- getPkgDir()
pkgn <- "testPkg"
path <- file.path(tdir, pkgn)

dpr_init(
  tdir,
  yaml=dpr_yaml_init(process_on_build=c("01.R", "02.R")),
  desc=dpr_description_init(Package=pkgn)
)

writeLines(
  c(
    "dat <- data.frame(x=1:10, y=LETTERS[1:10])",
    "save(dat, file='data/mydataframe.rda')"
  ),
  file.path(path, "processing/01.R")
)

writeLines(
  c(
    "dat <- matrix(1:16, nrow=4)",
    "save(dat, file='data/mymatrix.rda')"
  ),
  file.path(path, "processing/02.R")
)

## a processing script that accesses the datapackager.yml
writeLines(
  c(
    "dat <- as.list(LETTERS)",
    "save(dat, file=file.path(dpr_yaml_get()$data_directory, 'letters.rda'))"
  ),
  file.path(path, "processing/A1.R")
)

testthat::test_that("checking package build", {

  dpr_build(path, process_on_build = "01.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 1)
  
  dpr_build(path, process_on_build = "02.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 2)

  dpr_build(path, process_on_build = "A1.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 3)
  
  expect_equal(
      file.path(path,"..") |> list.files("testPkg.*\\.tar\\.gz") |> length(),
      1
  )
  expect_error(
      dpr_build(path, data_digest_directory="/notapath"),
      "Data digest directory does not exist"
  )
  expect_error(
      dpr_build(tempdir()),
      "`datapackager.yml` does not exist"
  )
  expect_false(exists("dpr_build_env", .GlobalEnv))
  
  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
#### check that dpr_render does not build package
#### check that working env contains render generated var names

})

cleanup(tdir)
