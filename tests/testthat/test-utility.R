testthat::test_that("dpr_save() always saves binary/gzip RDAs", {
  old_option <- getOption("save.defaults")
  on.exit(options(save.defaults = old_option))

  check_fn <- function(...){
    df <- data.frame(1:10)
    tf <- tempfile()
    on.exit(unlink(tf, recursive = TRUE))
    td <- file.path(tf, 'data')
    dir.create(td, recursive = TRUE)
    dpr_save('df', tf, ...)
    digest::digest(file = file.path(td, 'df.rda'))
  }

  # mimic serialization overrides forced by Rstudio server
  # https://github.com/rstudio/rstudio/issues/16419
  options(save.defaults = list(ascii = FALSE, compress = FALSE))
  hash_rstudio <- check_fn()
  # mimic default serialization options in base R save()
  options(save.defaults = NULL)
  hash_r <- check_fn()
  expect_equal(hash_r, hash_rstudio)
  # can still customize serialization via dpr_save arguments
  hash_custom <- check_fn(ascii = FALSE, compress = FALSE)
  expect_false(hash_r == hash_custom)
  # warn when overwriting uncompressed RDA with compressed RDA
  local({
    df <- data.frame(1:10)
    tf <- tempfile()
    on.exit(unlink(tf, recursive = TRUE))
    td <- file.path(tf, 'data')
    dir.create(td, recursive = TRUE)
    # this mimics having a legacy uncompressed RDA
    dpr_save('df', tf, ascii = FALSE, compress = FALSE)
    # this mimics overwriting that with DPR2 gzip-compressed RDA
    expect_warning(
      dpr_save('df', tf),
      'Overwriting uncompressed'
    )
  })
})

testthat::test_that("is_rda_compressed works", {
  tf <- tempfile()
  on.exit(unlink(tf))
  save(letters, file = tf, ascii = FALSE, compress = TRUE)
  expect_true(is_rda_compressed(tf))
  save(letters, file = tf, ascii = FALSE, compress = FALSE)
  expect_false(is_rda_compressed(tf))
})

testthat::test_that("dpr_yaml_load errors on invalid datapackager.yml file", {
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td))
  expect_error(dpr_yaml_load(td), 'invalid or missing')
})
