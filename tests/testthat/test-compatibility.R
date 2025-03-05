testthat::test_that("checking DataPackageR compatibility functions", {

  tdir <- getPkgDir()

  file.copy(test_path("dpr1package"), tdir, recursive = TRUE)
  path1 <- file.path(tdir, "dpr1package/")

  expect_error(
    dpr_build(path1),
    "Rendering a data package using DPR2 when the yaml is from DataPackageR"
  )

  expect_error(
    dpr_render(path1),
    "Rendering a data package using DPR2 when the yaml is from DataPackageR"
  )

  expect_error(
    dpr_yaml_get(path1),
    "Attemping to load a yaml of a DataPackageR package"
  )

  pkgn <- "testPkg"
  path2 <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn, list(renv_init = FALSE))

  expect_error(
    dpr_convert(path2),
    "not detected as DataPackageR package"
  )

  expect_warning(
    dpr_convert(path1, renv_init = FALSE),
    "`datapackager.yml` was found, skipping creating that file."
  )

  yml <- yaml::read_yaml(file.path(path1, "datapackager.yml"))
  expect_true(
    !is.null(yml$process_directory) && yml$process_directory == "data-raw"
  )

  expect_true(
    all(
      yml$process_on_build == c("mtcars.R", "iris.R", "trees.Rmd")
    )
  )

  expect_true(
    length(list.files(path1, "DATADIGEST")) == 0
  )

  expect_true(
    length(list.files(path1, "NEWS.md")) == 0
  )

  expect_true(
    length(list.files(file.path(path1, yml$data_digest_directory))) == 3
  )

  dpr_build(path1)

  cleanup(tdir)

})
