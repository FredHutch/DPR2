testthat::test_that("checking package utilities", {
  path <- "setup.R"
  expect_true(
    dpr_hash_file(path) == 
      git2r::hashfile(path)
  )
})
