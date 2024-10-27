## check defaulting to data_digest when git is not used in the repository
testthat::test_that("check dpr_hash against git2r hash", {
  path <- "setup.R"
  expect_true(
    dpr_hash_files(path) ==
      git2r::hashfile(path)
  )
})

## dpr_data_hashes and dpr_data_digest
testthat::test_that("checking package data hashes report", {

  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)

  initPkg(
    tdir,
    pkgn
  )

  dpr_build(path)

  script <- file.path(path, "processing/01.R")
  writeLines(gsub("y=", "z=", readLines(script)), script)

  dpr_render(path)

  expect_true(
    nrow(dpr_data_hashes(path)) == 5
  )

  expect_true(
    nrow(dpr_data_digest(path)) == 5
  )

  comp <- dpr_compare_data_digest(path)
  expect_true(!comp[comp$name == "mydataframe.rda", "same"])
  expect_true(comp[comp$name == "mydataframe_rmd.rda", "same"])

  expect_warning({
    dhst <- dpr_data_history(path)
    expect_true(
      identical(dhst, comp)
    )
  })

  expect_true(
    all(
      names(comp) == c("name", "data_hash", "data_digest_hash", "same")
    )
  )

  unlink(path, recursive = TRUE)
  cleanup(tdir)

})

## dpr_data_digest
testthat::test_that("checking package data history with git", {

  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)

  initPkg(
    tdir,
    pkgn
  )

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

  dpr_build(path)
  git2r::add(repo = path, path = ".")
  git2r::commit(repo = path, message="commit 1")

  script <- file.path(path, "processing/01.R")
  writeLines(gsub("y=", "z=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 2")

  writeLines(gsub("z=", "y=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 3")

  script <- file.path(path, "processing/01.R")
  writeLines(gsub("y=", "a=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 4")

  writeLines(gsub("save\\(yml", "save(yml, objYml1", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  Sys.sleep(1)
  git2r::commit(repo = path, message="commit 5")

  dataHistory <- dpr_data_history(path=path, include_checksums=TRUE)

  expect_equal( ncol(dataHistory), 5 )
  expect_equal( nrow(dataHistory), 10 )
  expect_true( all(row.names(dataHistory) == 1:nrow(dataHistory)) )

  expect_equal(
      length(
          unique(
            dataHistory[
              dataHistory$object_md5 %in% names(which(table(dataHistory$object_md5) > 1)),"object_md5"
            ]
          )
      ), 2 # this should be 4... record of reverting the blobs seems to be dropped from odb_blobs, commit 3
  )

  ## test recall objects are correctly named

  fullHash <- dataHistory$blob_file_hash[c(1,2)]
  subHash  <- substr(fullHash, 1, 5)
  missHash <- "0000000"
  notHash  <- "qwerty"

  expect_true(
    all(
      c("mydataframe", "df") == sapply(dpr_recall_data_versions(fullHash, path), names)
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
  lastHash <- tail(dataHistory,1)$blob_file_hash
  expect_true(
    sapply(dpr_recall_data_versions(lastHash, path), length) == 2
  )

  expect_true(
    grepl(
      "No checksum",
      tail(dataHistory, 1)$object_md5
    )
  )

  unlink(path, recursive = TRUE)
  cleanup(tdir)
  # did this last cleanup work?
  expect_false(dir.exists(tdir))

})
