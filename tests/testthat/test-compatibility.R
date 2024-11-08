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
