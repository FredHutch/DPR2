# keep this line first in test-build.R file
attached_before_test_builds <- names(sessionInfo()$otherPkgs)

tdir <- getPkgDir()
pkgn <- "testPkg"

testthat::test_that("checking package build", {

  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn)

  rda_to_restore <- file.path(path, 'data', 'restore_me.rda')
  file.create(rda_to_restore)
  expect_error(
    dpr_build(path, process_on_build = 'nolib.R'),
    'could not find function'
  )
  expect_true(file.exists(rda_to_restore))
  unlink(rda_to_restore)

  dpr_build(path, process_on_build = "01.Rmd")
  vign <- list.files(file.path(path, "vignettes"), full.names = TRUE)
  expect_true(length(vign) == 1)
  vignette <- readLines(vign[1])
  expect_true("vignette: |" %in% vignette & any(grepl("VignetteIndexEntry", vignette)))
  datn <- list.files(file.path(path, "data"))
  expect_true(all(datn == c("mydataframe_rmd.rda", "myyaml_rmd.rda")))

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
    "`path` argument is not a DataPackageR or DPR2 package"
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

  ## check NULL process_on_build on existing packages
  expect_error(
    dpr_build(path, process_on_build = NULL),
    "No files specified to process"
  )

  ## no variables should be in calling environment
  expect_false(exists("chkvar", environment()))

  ## check yaml validation
  expect_error(
    dpr_build(path, render_env_mode = "not valid"),
    "Invalid yaml values.+render_env_mode: isolate"
  )

  unlink(path, recursive = TRUE)

  ## check that when nothing is set to process_on_build, error is as expected
  path <- file.path(tdir, "NoProcess")
  dpr_create(tdir, desc=dpr_description_init(Package=basename(path)))
  expect_error(
    dpr_build(path),
    "No files specified to process"
  )

  unlink(path, recursive = TRUE)

  path <- file.path(tdir, "WrongYaml")
  dpr_create(tdir, desc=dpr_description_init(Package=basename(path)))
  ypth <- file.path(path, "datapackager.yml")
  yfil <- readLines(ypth)
  writeLines(gsub("render_on_build", "rnder_n_bild", yfil), ypth)
  expect_error(
    dpr_build(path),
    "yaml.*not found.*render_on_build"
  )

  path <- file.path(tdir, "multi")
  dpr_create(tdir, desc=dpr_description_init(Package=basename(path)))
  writeLines("x <- 1; y <- 1; save(x, y, file= 'data/multi.rda')", file.path(path, "processing/multi.R"))
  expect_warning(
    dpr_build(path, process_on_build = "multi.R"),
    "No checksum computed for RDA files containing more than"
  )

  unlink(path, recursive = TRUE)

})

testthat::test_that("checking package render",{
  expect_true(TRUE)
  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

cleanup(tdir)

# Keep this test last in test-build.R file
testthat::test_that("R/Rmd library() calls not attached to main R process",{
  attached_in_scripts <- c('yaml', 'lubridate')
  pkgs_to_check <- setdiff(attached_in_scripts, attached_before_test_builds)
  skip_if(length(pkgs_to_check) == 0L, 'pkgs already attached before testing')
  expect_false(
    any(pkgs_to_check %in% names(sessionInfo()$otherPkgs))
  )
})
