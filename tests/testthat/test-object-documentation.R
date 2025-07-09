test_that("path and out_dir must be character strings in generate_all_docs", {
  # expect error when `path` is not a character string
  expect_error(
    generate_all_docs(path = 123),
    "`path` must be a character string."
  )
})

test_that("check that R object documentation is written as expected", {
  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn)
  dpr_build(path, process_on_build = "01.R", objects = "objYml1")
  # check that .R file exists
  expect_true(file.exists(file.path(path, "R", "objYml1.R")))
  # check that .R file exists for objects saved in script
  expect_true(file.exists(file.path(path, "R", "mydataframe.R")))

  # check that there is no warning since a new object is being created
  expect_no_message(dpr_build(path, process_on_build = "01.R", objects = c("objYml1", "objYml2")))
  expect_true(file.exists(file.path(path, "R", "objYml2.R")))

  #expect a message when there are no changes to object or no new object is created
  expect_message(dpr_build(path, process_on_build = "01.R", objects = c("objYml1", "objYml2")),
                 "No new data object documentation created, as no objects")

  unlink(path, recursive = TRUE, force = TRUE)
  cleanup(tdir)
})

test_that("check that roxygenize on build generates .Rd files", {
  tdir <- getPkgDir()
  pkgn <- "testPkgRD"
  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn)
  writeLines(
    c(
      "library(yaml)",
      "library(DPR2)",
      "df1 <- data.frame(x = 1:11, y = LETTERS[1:11])",
      "df2 <- data.frame(y = 1:5, y = letters[1:5])",
      "df_list <- list(a = 'x', b = 'y', c = 'z')",
      "yml <- as.yaml(df2)",
      "descript <- desc::desc()",
      "dpr_save('df1')",
      "dpr_save('df2')",
      "dpr_save('df_list')",
      "dpr_save('yml')",
      "dpr_save('descript')" # check that other class type can generate documentation
    ),
    file.path(path, "processing/df_gen.R")
  )

  dpr_add_scripts("df_gen.R", path)
  dpr_build(path)

  expect_true(file.exists(file.path(path, "man", "df1.Rd")))
  expect_true(file.exists(file.path(path, "man", "df_list.Rd")))
  expect_true(file.exists(file.path(path, "man", "yml.Rd")))
  expect_true(file.exists(file.path(path, "man", "descript.Rd")))

  unlink(path, recursive = TRUE, force = TRUE)
  cleanup(tdir)
})

test_that("check that generate_docs throws a warning when rda object does not match filename", {

  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn)
  writeLines(
    c(
      "library(DPR2)",
      "df1 <- data.frame(x = 1:11, y = LETTERS[1:11])",
      "df2 <- data.frame(y = 1:5, y = letters[1:5])",
      "save(df1, file=dpr_path('data', 'wrong_name.rda'))",
      "dpr_save('df2')"
    ),
    file.path(path, "processing/df_gen.R")
  )

  dpr_add_scripts("df_gen.R", path)

  expect_warning(
    dpr_build(path),
    "'wrong_name.rda' does not match data object name 'df1'. Will skip writing documentation for it."
  )

  unlink(path, recursive = TRUE, force = TRUE)
  cleanup(tdir)
})

test_that("check that no_change is filtered based on R doc files in generate_all_docs()", {
  out_dir <- tempdir()
  no_change <- c("dat1", "dat2")

  #create temp doc file
  doc_file <- file.create(file.path(out_dir, "dat1.R"))

  # if object does not have a R doc file for some reason, remove from list
  if (length(no_change) > 0) {
    doc_files <- list.files(out_dir, all.files = TRUE, no.. = TRUE)
    doc_basenames <- tools::file_path_sans_ext(basename(doc_files))
    no_change <- intersect(no_change, doc_basenames)
  }

  expect_equal(no_change, c("dat1"))
  unlink(file.path(out_dir), recursive=TRUE, force=TRUE)
})

test_that("check that delete_unused_doc_files accurately deletes unused R doc files", {

  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)
  createPkg(tdir, pkgn)
  writeLines(
    c(
      "library(DPR2)",
      "df1 <- data.frame(x = 1:11, y = LETTERS[1:11])",
      "df2 <- data.frame(y = 1:5, y = letters[1:5])",
      "dpr_save('df1')",
      "dpr_save('df2')"
    ),
    file.path(path, "processing/df_gen.R")
  )

  dpr_add_scripts("df_gen.R", path)

  dpr_build(path, write_data_docs = FALSE)
  expect_true(
    length(list.files(file.path(path, "R"))) == 0
  )

  dpr_build(path)
  expect_true(
    length(list.files(file.path(path, "R"))) != 0
  )

  # remove one of the Rda files to test delete_unused_doc_files
  file.remove(file.path(path, "data", "df1.rda"))

  delete_unused_doc_files(path)

  # should have removed df1.R
  expect_true(!"df1.R" %in% list.files(file.path(path, "R")))

  # test that non-data doc files do not get removed
  writeLines(
    c(
      "#' Example function",
      "#' @param x input",
      "#' @return output",
      "ex_func <- function(x) {x + 1}"
    ),
    file.path(path, "R/test_func.R")
  )

  delete_unused_doc_files(path)

  expect_true("test_func.R" %in% list.files(file.path(path, "R")))

  unlink(path, recursive = TRUE, force = TRUE)
  cleanup(tdir)

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

  unlink(temp_dir, recursive = TRUE, force = TRUE)
})

test_that("check that template_doc_block generates documentation correctly", {
  df <- data.frame(x = 1:11, y = LETTERS[1:11])
  object_name <- "df"

  doc_block <- template_doc_block(df, object_name)

  # check that content is correct
  expect_true(grepl(object_name, doc_block[3]))
  expect_true(any(grepl("\\\\describe\\{", doc_block)))
  expect_true(any(grepl("\\\\item\\{x\\}", doc_block)))
  expect_true(any(grepl("\\\\item\\{y\\}", doc_block)))
  expect_true(any(grepl("11 rows and 2 columns", doc_block)))
})

test_that("check that tryCatch for write_doc_file and template_doc_block in generate_all_docs() outputs an error", {
  object_name <- "no_exist"
  fake_env <- new.env()

  expect_warning(
    result <- tryCatch({
    # return object value
    object <-  get(object_name, envir = fake_env)
    # generate roxygen documentation block
    doc_block <- template_doc_block(object, object_name)

    # write the documentation block to an .R file
    write_doc_file(out_dir, doc_block, object_name)
  }, error = function(e) {
    warning(sprintf("Error processing '%s': %s", object_name, e$message))
  }),
  sprintf("Error processing '%s': object '%s' not found", object_name, object_name))
})
