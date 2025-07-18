testthat::test_that("checking DataPackageR compatibility functions", {

  path1 <- file.path(tempdir(), "dpr1package")
  file.copy(test_path("dpr1package"), tempdir(), recursive = TRUE)

  expect_error(
    dpr_build(path1),
    "Rendering a data package using DPR2 when the yaml is from DataPackageR."
  )

  expect_error(
    dpr_render(path1),
    "Rendering a data package using DPR2 when the yaml is from DataPackageR."
  )

  expect_error(
    dpr_yaml_get(path1),
    "Attemping to load a yaml of a DataPackageR package"
  )

  path2 <- copyPkg("dpr2test")
  on.exit(unlink(path2, recursive = TRUE))

  expect_error(
    dpr_convert(path2),
    "not detected as DataPackageR package"
  )

  expect_error(
    dpr_convert("notAPath"),
    "Package path not found."
  )

  expect_length(
    readLines(file.path(path1, "DATADIGEST")), 3
  )

  expect_error(
    dpr_create(path1),
    "This is a DataPackageR package. Please convert the package"
  )

  expect_error(
    dpr_init(path1),
    "This is a DataPackageR package. Please convert the package"
  )

  warns <- capture_warnings(
    dpr_convert(path1)
  )

  expect_length(warns, 1)
  expect_length(list.files(file.path(path1, "inst/to_build/objects")), 2)

  expect_true(
    "Object `mtcars_mod` in data directory does not match md5 not found in DATADIGEST." %in% warns
  )

  expect_length(
    list.files(file.path(path1, "inst/data_digest")), 3
  )

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

  expect_false(
    all(
      c("Date", "DataVersion") %in% desc::desc(file.path(path1, "DESCRIPTION"))$fields()
    )
  )

  expect_true(
    all(
      "welcome.Rmd" == basename(list.files(file.path(path1, "inst/doc")))
    )
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

  expect_error(
    dpr_create(path1),
    "This source is already a DPR2 package."
  )

  expect_error(
    dpr_init(path1),
    "This source is already a DPR2 package."
  )

  dpr_build(path1)

})
