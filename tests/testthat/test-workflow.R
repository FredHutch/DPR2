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
  expect_error(
    dpr_add_scripts("not_real.R", path=new),
    "^`not_real.R` not found.+"
  )
  write_added_file("missing.R", "scripts", path = new)
  write_added_file("not_real.R", "scripts", path = new)
  expect_warning(
    dpr_scripts(path = new),
    "Items being added not found.+removing with.+: missing.R, not_real.R"
  )

})
