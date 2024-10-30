testthat::test_that("checking DataPackageR compatibility functions", {

  file.copy(test_path("dpr1package"), tempdir(), recursive = TRUE)
  path <- file.path(tempdir(), "dpr1package/")

  expect_warning(
    dpr_build(path),
    "Rendering a data package using DPR2 when the yaml is from DataPackageR"
  )

  ## convert DataPackageR yaml to DPR2 yaml
  dpr_dpr1_yaml_convert(path)

  expect_false(
    dpr_is_dpr1(path)
  )
  expect_true(
    dpr_is_dpr2(path)
  )

  dpr_build(path)
  unlink(path)

})

testthat::test_that("checking datapackager_object_read messaging", {
  path <- file.path(tempdir(), "data")
  dir.create(path)
  iris_t <- iris
  mtcars_t <- mtcars
  dpr_save("iris_t", tempdir())
  save(iris_t, mtcars_t, file=file.path(path, "tabs.rda"))
  save(iris_t, file=file.path(path, "iris_d.rda"))

  expect_warning(
    datapackager_object_read("iris_t", tempdir()),
    "`datapackager_object_read` has been depreciated"
  )
  
  expect_error(
    expect_warning(
      datapackager_object_read(c("tabs", "iris_t"), tempdir())
    ),
    "`datapackager_object_read` object argument must be a singleton character vector."
  )

  expect_error(
    expect_warning(
      datapackager_object_read("tables", tempdir())
    ),
    "Object does not exist in the data directory."
  )

  expect_error(
    expect_warning(
      datapackager_object_read("tabs", tempdir())
    ),
    "`datapackager_object_read` cannot read multi-object"
  )

  expect_error(
    expect_warning(
      datapackager_object_read("iris_d", tempdir())
    ),
    "`datapackager_object_read` object argument not found at rda file with the object name."
  )
  
  unlink(path, recursive=TRUE)
})
