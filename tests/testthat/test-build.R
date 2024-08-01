tdir <- getPkgDir()
pkgn <- "testPkg"

testthat::test_that("checking package build", {

  path <- file.path(tdir, pkgn)
  initPkg(tdir, pkgn, list(renv_init = FALSE))

  dpr_build(path, process_on_build = "01.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 1)

  dpr_build(path, process_on_build = "02.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 2)

  dpr_build(path, process_on_build = "A1.R", build_tarball = TRUE)
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

  dpr_build(path)
  expect_false(exists("dfm"))

  dpr_build(path, render_env_mode = "isolate", process_on_build = "01.R")
  expect_false(exists("dfm"))

  expect_error(
    dpr_build(path, render_env_mode = "isolate", process_on_build = c("01.R", "S1.R"))
  )

  dpr_build(path, render_env_mode = "share", process_on_build = c("02.R", "S1.R"))
  
  ## no variables should be in calling environment
  expect_false(exists("chkvar", environment()))

  ## check yaml validation
  expect_error(
    dpr_build(path, render_env_mode = "not valid"),
    "Invalid `render_env_mode` yaml value used. Please one of these:"
  )
  
  unlink(path, recursive = TRUE)

})

testthat::test_that("checking package render",{

  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

cleanup(tdir)
