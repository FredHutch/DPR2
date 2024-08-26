testthat::test_that("checking package utilities", {
  path <- "setup.R"
  expect_true(
    dpr_hash_file(path) == 
      git2r::hashfile(path)
  )
})

testthat::test_that("checking package data history", {

  tdir <- getPkgDir()
  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)

  initPkg(
    tdir,
    pkgn
  )

  dpr_build(path)
  
  git2r::init(path = path, branch="main")
  git2r::add(repo = path, path = ".")
  git2r::commit(repo = path, message="commit 1")

  script <- file.path(path, "processing/01.R")
  writeLines(gsub("y=", "z=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  git2r::commit(repo = path, message="commit 2")
  
  script <- file.path(path, "processing/01.R")
  writeLines(gsub("z=", "y=", readLines(script)), script)
  dpr_build(path)

  git2r::add(repo = path, path = ".")
  git2r::commit(repo = path, message="commit 3")
  
  dataHistory <- dpr_data_history(include_checksums = TRUE, path=path)

  expect_equal( ncol(dataHistory), 6 )
  expect_equal( nrow(dataHistory), 7 )
  expect_true( all(row.names(dataHistory) == 1:nrow(dataHistory)) )
  expect_equal(
      length(
          unique(
            dataHistory[
              dataHistory$object_md5 == names(which(table(dataHistory$object_md5) > 1)),"object_md5"
            ]
          )
        ), 1
      )

  unlink(path, recursive = TRUE)
  cleanup(tdir)
  
})
