test_that("project_path functions work", {
  pkg_dir <- tempfile()
  dir.create(pkg_dir)
  on.exit(unlink(pkg_dir, recursive = TRUE))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(pkg_dir)
  # error when no DESCRIPTION file yet in file tree
  expect_error(project_path())
  expect_error(project_data_path())
  expect_error(project_extdata_path())
  writeLines('Package: ', 'DESCRIPTION')
  # correct directory with no arguments, called from wd
  expect_equal(basename(project_path()), basename(pkg_dir))
  expect_equal(basename(project_data_path()), 'data')
  expect_equal(basename(project_extdata_path()), 'extdata')
  # now call from inside subfolder
  dir.create('subfolder')
  setwd('subfolder')
  # correct directory with no arguments
  expect_equal(basename(project_path()), basename(pkg_dir))
  expect_equal(basename(project_data_path()), 'data')
  # path with subfolder and file arguments
  pp <- project_path('hello', 'world.txt')
  expect_equal(basename(pp), 'world.txt')
  expect_equal(basename(dirname(pp)), 'hello')
  expect_equal(basename(dirname(dirname(pp))), basename(pkg_dir))
  pdp <- project_data_path('hello', 'world.txt')
  expect_equal(basename(dirname(pdp)), 'hello')
})
