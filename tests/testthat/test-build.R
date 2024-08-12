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

  ## check that when nothing is set to process_on_build, error is as expected
  path <- file.path(tdir, "NoProcess")
  dpr_init(tdir, desc=dpr_description_init(Package=basename(path)))
  expect_error(
    dpr_build(path),
    "Are any processes set to build?"
  )

  unlink(path, recursive = TRUE)

  path <- file.path(tdir, "WrongYaml")
  dpr_init(tdir, desc=dpr_description_init(Package=basename(path)))
  ypth <- file.path(path, "datapackager.yml")
  yfil <- readLines(ypth)
  writeLines(gsub("render_on_build", "rnder_n_bild", yfil), ypth)
  expect_error(
    dpr_build(path),
    "The following required yaml values are not found: render_on_build."
  )
})

testthat::test_that("checking package render",{

  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

cleanup(tdir)
