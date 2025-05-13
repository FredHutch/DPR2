testthat::test_that("checking package workflow", {
  new <- file.path(tempdir(), "dpr2package")
  on.exit({unlink(new, recursive = TRUE)})
  dir.create(new)
  file.copy("dpr2package/processing", new, recursive=TRUE)
  expect_warning(
    dpr_init(new),
    "`processing` was found, "
  )
  dpr_add_scripts(list.files(file.path(new, "processing")), path = new)
  dpr_build(new)
})
