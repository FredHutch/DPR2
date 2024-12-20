tdir <- getPkgDir()
pkgn <- "testPkg"

test_that("check that R object documentation is written as expected", {
  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn, list(renv_init = FALSE))
  dpr_build(path, process_on_build = "01.R", objects = "objYml1")
  # check that .R file exists
  expect_true(file.exists(file.path(path, "R", "objYml1.R")))
  # check that .R file exists for objects saved in script
  expect_true(file.exists(file.path(path, "R", "mydataframe.R")))

  # check that there is no warning since a new object is being created
  expect_no_warning(dpr_build(path, process_on_build = "01.R", objects = c("objYml1", "objYml2")))
  expect_true(file.exists(file.path(path, "R", "objYml2.R")))

  #expect a warning when there are no changes to object or no new object is created
  expect_warning(dpr_build(path, process_on_build = "01.R", objects = c("objYml1", "objYml2")),
                 "No new data objects have been created, and no existing objects have been modified.")

  # expect a warning when rda has multiple objects
  # create test script
  writeLines(
    c(
      "df1 <- data.frame(x = 1:11, y = LETTERS[1:11])",
      "df2 <- data.frame(y = 1:5, y = letters[1:5])",
      "save(df1, df2, file=DPR2::dpr_path('data', 'df.rda'))"
    ),
    file.path(path, "processing/mult_obj.R")
  )

  dpr_yaml_set(path, process_on_build = "mult_obj.R")
  expect_warning(
    dpr_build(path),
    "'df.rda' contains multiple or no objects. Will skip writing documentation for it."
  )

  unlink(path, recursive = TRUE)
})

test_that("check that write_doc_file writes documentation files correctly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  doc_block <- c("#' Example Documentation", "#' For a test object")
  object_name <- "test_object"

  write_doc_file(temp_dir, doc_block, object_name)

  # check that file is created
  file_path <- file.path(temp_dir, paste0(object_name, ".R"))
  expect_true(file.exists(file_path))

  # check that content is correct
  content <- readLines(file_path)
  expect_equal(content, doc_block)

  unlink(temp_dir, recursive = TRUE)
})

test_that("check that template_doc_block generates documentation correctly", {
  df <- data.frame(x = 1:11, y = LETTERS[1:11])
  object_name <- "df"

  doc_block <- template_doc_block(df, object_name)

  # check that content is correct
  expect_true(grepl(object_name, doc_block[1]))
  expect_true(any(grepl("\\\\describe\\{", doc_block)))
  expect_true(any(grepl("\\\\item\\{x\\}", doc_block)))
  expect_true(any(grepl("\\\\item\\{y\\}", doc_block)))
  expect_true(any(grepl("11 rows and 2 columns", doc_block)))
})
