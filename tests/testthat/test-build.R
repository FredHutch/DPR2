tdir <- getPkgDir()
pkgn <- "testPkg"

testthat::test_that("checking package build", {

  path <- file.path(tdir, pkgn)
  initPkg(tdir, pkgn, list(renv_init = FALSE))

  dpr_build(path, process_on_build = "01.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 1)
  datn <- list.files(file.path(path, "data"))
  expect_true(all(datn == c("mydataframe.rda", "myyaml.rda")))

  dpr_build(path, process_on_build = "02.R")
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 2)
  datn <- list.files(file.path(path, "data"))
  expect_true(datn == "mymatrix.rda")
  
  dpr_build(path, process_on_build = "A1.R", build_tarball = TRUE)
  vign <- list.files(file.path(path, "vignettes"))
  expect_true(length(vign) == 3)

  ## looking for one object 
  dpr_build(path, process_on_build = "01.R", objects = "objYml1")
  datn <- list.files(file.path(path, "data"))
  expect_true(all("objYml1.rda" %in% datn))
  expect_false(all(c("objYml1.rda", "objYml2.rda") %in% datn))

  ## looking for both objects and manually saved data
  dpr_build(path, process_on_build = "01.R", objects = c("objYml1", "objYml2"))
  datn <- list.files(file.path(path, "data"))
  expect_true(all(c("objYml1.rda", "objYml2.rda") %in% datn))
  expect_true(
    all(
      datn %in% c("mydataframe.rda", "myyaml.rda", "objYml1.rda", "objYml2.rda")
    )
  )

  ## looking for both objects when set in the yaml and not passed as build arguments 
  dpr_yaml_set(path, process_on_build = "01.R", objects = c("objYml1", "objYml2"))
  dpr_build(path)
  datn <- list.files(file.path(path, "data"))
  expect_true(
    all(
      datn %in% c("mydataframe.rda", "myyaml.rda", "objYml1.rda", "objYml2.rda")
    )
  )
  dpr_yaml_set(path, process_on_build = "01.R", objects = c())

  ## warn when typo in object name
  expect_warning(
    dpr_build(path, objects = "objYmlX.rda")
  )
  datn <- list.files(file.path(path, "data"))
  expect_true(
    all(datn %in% c("mydataframe.rda", "myyaml.rda"))
  )
  
  ## check that tarball is built
  dpr_build(path, build_tarball = TRUE)
  expect_length(
    list.files(
      file.path(path,".."),
      "testPkg.*\\.tar\\.gz"
    ),
    1L
  )

  ## check for valid package
  expect_error(
      dpr_build(path, data_digest_directory="/notapath"),
      "Data digest directory does not exist"
  )
  
  expect_error(
      dpr_build(tempdir()),
      "`datapackager.yml` does not exist"
  )

  ## check evaluation share/isolate
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

  unlink(path, recursive = TRUE)
  
  ## check dpr_save 
  initPkg(tdir, "Saving")
  path <- file.path(tdir, "Saving")
  dpr_build(path, process_on_build = "SV1.R")
  script <- file.path(path, "processing", "SV1.R")
  newScript <- gsub("'", "", readLines(script))
  writeLines(newScript, script)
  expect_error(
    dpr_build(path, process_on_build = "SV1.R")
  )
    
  unlink(path, recursive = TRUE)
  
})

testthat::test_that("checking package render",{

  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

cleanup(tdir)
