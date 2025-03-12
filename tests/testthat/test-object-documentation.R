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
  createPkg(tdir, pkgn, list(renv_init = FALSE))
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
                 "No new data objects have been created, and no existing objects have been modified. There are no objects to document.")

  unlink(path, recursive = TRUE)
  cleanup(tdir)
})

test_that("check that error occurs in stop(e) of no_change portion in generate_all_docs when run outside of a DPR2 pkg", {

  expect_error({
    no_change <- tryCatch({
      digest_data <- dpr_compare_data_digest(".")
      gsub(".rda", "", digest_data$name[digest_data$same == TRUE])
    },
    error = function(e) {
      if (grepl("No digest files found. Has any data been added to the data package yet?", e$message)) {
        return(NA)
      } else {
        stop(e)  # Let unexpected errors propagate
      }
    })
  }, "`path` argument is not a DataPackageR or DPR2 package.")

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
  createPkg(tdir, pkgn, list(renv_init = FALSE))
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

  dpr_yaml_set(path, process_on_build = "df_gen.R")
  dpr_build(path)

  # remove one of the Rda files to test delete_unused_doc_files
  file.remove(file.path(path, "data", "df1.rda"))

  delete_unused_doc_files(path)

  # should have removed df1.R
  expect_equal(list.files(file.path(path, "R")), "df2.R")

  unlink(path, recursive = TRUE)
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
