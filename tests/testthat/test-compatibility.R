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

  dpr_convert(path1)

  yml <- yaml::read_yaml(file.path(path1, "datapackager.yml"))
  expect_true(
    !is.null(yml$process_directory) && yml$process_directory == "data-raw"
  )

  expect_false(
    all(
      c(
        file.exists(
          file.path(path1, "R/dpr1package.R"),
          file.path(path1, "DATADIGEST"),
          file.path(path1, "NEWS.md"),
          file.path(path1, "R/documentation.R"),
          file.path(path1, "inst/extdata/Logfiles"),
          file.path(path1, "Read-and-delete-me")
        ),
        dir.exists(
          file.path(path1, "inst/extdata/Logfiles")
        )
      )
    )
  )

  expect_false("Date" %in% desc::desc(file.path(path1, "DESCRIPTION"))$fields())

  expect_true(
    length(list.files(file.path(path1, "vignettes"))) == 1
  )

  expect_true(
    basename(list.files(file.path(path1, "vignettes"))) == "welcome.Rmd"
  )

  expect_true(
    all(
      yml$process_on_build == c("mtcars.R", "iris.R")
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
