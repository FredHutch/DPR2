## dpr_data_checksums and dpr_data_digest
testthat::test_that("checking package data hashes report", {

  path <- copyPkg("dpr2test")
  on.exit(unlink(path, recursive = TRUE))

  dpr_add_scripts("01.R", path)
  dpr_build(path)

  script <- file.path(path, "processing/01.R")
  writeLines(gsub("y=", "z=", readLines(script)), script)

  dpr_render(path)

  expect_equal(nrow(dpr_data_checksums(path)), 2)
  expect_equal(nrow(dpr_data_digest(path)), 2)

  comp <- dpr_compare_data_digest(path)
  expect_true(!comp[comp$name == "mydataframe.rda", "same"])
  expect_true(comp[comp$name == "myyaml.rda", "same"])

  expect_warning({
    dhst <- dpr_data_history(path)
    expect_true(
      identical(dhst, comp)
    )
  })

  expect_true(
    all(
      names(comp) == c("name", "data_md5", "data_digest_md5", "same")
    )
  )

})

## dpr_data_digest
testthat::test_that("checking package data history with git", {

  path <- copyPkg("dpr2test")
  on.exit(unlink(path, recursive = TRUE))

  expect_error(
    expect_warning(
      dpr_data_history(path)
    ), "No digest files found"
  )

  git2r::init(path = path, branch="main")
  git2r::add(repo = path, path = ".")
  Sys.sleep(1) # odb_blobs appears to be ordered by when, and the smallest unit is 1 sec, adding pauses to get correct sorting
  git2r::commit(repo = path, message="commit 0")

  expect_error(dpr_data_history(path), "No files found at the `data` path")

  dpr_add_scripts("01.R", path)
  script <- file.path(path, "processing/01.R")

  dpr_build(path)
  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 1")

  ## change dataframe
  writeLines(gsub("y=", "z=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 2")

  ## revert dataframe
  writeLines(gsub("z=", "y=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 3")

  ## create rda with 2 objects
  writeLines(gsub("save\\(myyaml", "save(myyaml, objYml1", readLines(script)), script)
  expect_warning(
    dpr_build(path)
  )

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 4")

  expect_warning(
    dataHistory <- dpr_data_history(path=path, include_checksums=TRUE)
  )

  expect_equal( ncol(dataHistory), 5 )
  expect_equal( nrow(dataHistory), 4 ) # 2 versions of mydatapackage, 2 versions of myyaml
  expect_true( all(row.names(dataHistory) == 1:nrow(dataHistory)) )
  expect_true( any(grepl("No checksum computed", dataHistory$object_checksum)) )

  ## test recall objects are correctly named
  fullHash <- dataHistory$blob_git_sha1[c(1,2)]
  subHash  <- substr(fullHash, 1, 5)
  missHash <- "0000000"
  notHash  <- "qwerty"

  expect_true(
    all(
      c("mydataframe", "myyaml") == sapply(dpr_recall_data_versions(fullHash, path), names)
    )
  )

  expect_equal(
    length(dpr_recall_data_versions(fullHash, path)), 2
  )

  expect_equal(
    length(dpr_recall_data_versions(subHash, path)), 2
  )

  expect_error(
    dpr_recall_data_versions(missHash, path),
    "Data version not found. Either "
  )

  expect_error(
    dpr_recall_data_versions(notHash, path),
    "Data version not found. Either "
  )

  ## check that behavior multiple objects are saved
  lastHash <- tail(dataHistory,1)$blob_git_sha1
  expect_equal(
    length(dpr_recall_data_versions(lastHash, path)[[1]]), 2
  )

  expect_true(
    grepl(
      "No checksum",
      tail(dataHistory, 1)$object_checksum
    )
  )

  unlink(path, recursive = TRUE)

})
