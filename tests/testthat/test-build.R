tdir <- getPkgDir()

testthat::test_that("checking package build", {

  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)
  initPkg(tdir, pkgn, list(renv_init = FALSE))

  dpr_build(path, process_on_build = "01.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 1)

  dpr_build(path, process_on_build = "02.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 2)

  dpr_build(path, process_on_build = "A1.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 3)

  expect_length(
    list.files(
      file.path(path,".."),
      "testPkg.*\\.tar\\.gz"
    ),
    1L
  )
  expect_error(
      dpr_build(path, data_digest_directory="/notapath"),
      "Data digest directory does not exist"
  )
  expect_error(
      dpr_build(tempdir()),
      "`datapackager.yml` does not exist"
  )

  ## build env should be cleared
  expect_false(exists("dpr_build_env", .GlobalEnv))

  
  ## no variables should be in calling environment
  expect_false(exists("chkvar", environment()))

  unlink(file.path(tdir, pkgn), recursive = TRUE)

})

testthat::test_that("checking package render",{

  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

cleanup(tdir)
