test_that("project_path functions work", {
  # normalize for equality tests later
  pkg_dir <- normalizePath(tempfile(), '/', FALSE)
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
  expect_equal(project_path(), pkg_dir)
  expect_equal(project_data_path(), file.path(pkg_dir, 'data'))
  expect_equal(project_extdata_path(), file.path(pkg_dir, 'inst', 'extdata'))
  # now call from inside subfolder
  dir.create('subfolder')
  setwd('subfolder')
  # correct directory with no arguments
  expect_equal(project_path(), pkg_dir)
  expect_equal(project_data_path(), file.path(pkg_dir, 'data'))
  expect_equal(project_extdata_path(), file.path(pkg_dir, 'inst', 'extdata'))
  # path with subfolder and file arguments
  expect_equal(
    project_path('hello', 'world.txt'),
    file.path(pkg_dir, 'hello', 'world.txt')
  )
  expect_equal(
    project_data_path('hello', 'world.txt'),
    file.path(pkg_dir, 'data', 'hello', 'world.txt')
  )
  expect_equal(
    project_extdata_path('hello', 'world.txt'),
    file.path(pkg_dir, 'inst', 'extdata', 'hello', 'world.txt')
  )
})
