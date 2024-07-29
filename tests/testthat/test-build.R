tdir <- getPkgDir()
pkgn <- "testPkg"
path <- file.path(tdir, pkgn)

DPR2::dpr_init(
  tdir,
  yaml=DPR2::dpr_yaml_init(process_on_build=c("01.R", "02.R")),
  desc=DPR2::dpr_description_init(Package=pkgn)
)

## test that libraries can be loaded and accessed
writeLines(
  c(
    "library(yaml)",
    "dfm <- data.frame(x=1:10, y=LETTERS[1:10])",
    "yml <- as.yaml(df)",
    "save(dfm, file='data/mydataframe.rda')",
    "save(yml, file='data/myyaml.rda')"
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
    "save(dat, file=file.path(DPR2::dpr_yaml_get()$data_directory, 'letters.rda'))"
  ),
  file.path(path, "processing/A1.R")
)

## check if environment is shared
writeLines(
  c(
    "save(dat, file=file.path(DPR2::dpr_yaml_get()$data_directory, 'dat.rda'))"
  ),
  file.path(path, "processing/S1.R")
)

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

  unlink(file.path(tdir, pkgn), recursive = TRUE)

})

testthat::test_that("checking package render",{

  ## render DPR2 package - renders all processing scripts, but does not build, renders in working env
  ## check that dpr_render does not build package
  ## check that working env contains render generated var names

})

testthat::test_that("checking yaml validation", {
  expect_error(
    dpr_build(path, render_env_mode = "not valid"),
    "Invalid `render_env_mode` yaml value used. Please one of these:"
  )
})

cleanup(tdir)
