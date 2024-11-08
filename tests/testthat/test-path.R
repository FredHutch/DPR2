test_that("project_path functions work", {
  pkg_dir <- tempfile()
  pkg_name <- basename(pkg_dir)
  dir.create(pkg_dir)
  on.exit(unlink(pkg_dir, recursive = TRUE))
  cwd <- getwd()
  on.exit(setwd(cwd), add = TRUE)
  setwd(pkg_dir)
  # error when no DESCRIPTION file yet in file tree
  # and also expect deprecation warning
  expect_error(expect_warning(project_path(), 'legacy'))
  expect_error(expect_warning(project_data_path(), 'legacy'))
  expect_error(expect_warning(project_extdata_path(), 'legacy'))
  writeLines('Package: ', 'DESCRIPTION')
  # suppress deprecation warnings for rest of tests
  suppressWarnings({
    # correct directory with no arguments, called from wd
    expect_equal(basename(project_path()), pkg_name)
    expect_equal(basename(project_data_path()), 'data')
    expect_equal(basename(dirname(project_data_path())), pkg_name)
    expect_equal(basename(project_extdata_path()), 'extdata')
    expect_equal(basename(dirname(project_extdata_path())), 'inst')
    expect_equal(basename(dirname(dirname(project_extdata_path()))), pkg_name)
    # test that we still find package root when getwd() is within a subfolder
    dir.create('subfolder777')
    setwd('subfolder777')
    # correct directory with no arguments
    expect_equal(basename(project_path()), pkg_name)
    expect_false(grepl('subfolder777', project_path()))
    # path with subfolder and file arguments
    pp <- project_path('hello', 'world.txt')
    expect_false(grepl('subfolder777', pp))
    expect_equal(basename(pp), 'world.txt')
    expect_equal(basename(dirname(pp)), 'hello')
    expect_equal(basename(dirname(dirname(pp))), pkg_name)
  })
})
