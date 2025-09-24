test_that("check DESCRIPTION file has populated fields", {
  pkgn <- "DescripPackage"
  on.exit(unlink(file.path(tempdir(), pkgn), recursive = TRUE))
  pkgt <- "A package for testing description writing"
  dpr_create(
    tempdir(),
    desc = dpr_description_init(
      Package = pkgn,
      Title   = pkgt,
      Authors = "Foo Bar [aut, cre]"
    )
  )

  defi <- DPR2:::dpr_description_defaults()
  desc <- desc:::desc(file.path(tempdir(), pkgn))

  expect_true(desc$get("Package")     == pkgn)
  expect_true(desc$get("Title")       == pkgt)
  expect_true(desc$get("Description") == defi$Description)

})

test_that("check datapackager.yml and ignores", {

  pkgn = "testing"
  dpr_create(
    tempdir(),
    yaml=dpr_yaml_init(process_directory = "data-raw"),
    desc=dpr_description_init(Package = pkgn)
  )

  rIgnore <- c(
    "^data/.+\\.(r|tab|txt|csv|tsv|rds)$",
    "^inst/extdata/"
  )

  gIgnore <- c(
    "inst/doc/"
  )

  expect_true(
    all(
      rIgnore %in% readLines(file.path(tempdir(), pkgn, ".Rbuildignore"))
    )
  )

  expect_true(
    all(
      gIgnore %in% readLines(file.path(tempdir(), pkgn, ".gitignore"))
    )
  )

  ## yml names manually set but have the manual values, other values unchanged
  yml <- dpr_yaml_get(file.path(tempdir(), "testing"))
  ymlInit <- dpr_yaml_init()

  expect_true(yml$process_directory == "data-raw")
  expect_true(yml$to_build_directory == ymlInit$to_build_directory)

  ## all names in init must be in package yaml
  expect_true(
    all(
      names(ymlInit) %in% names(yml)
    )
  )

  unlink(file.path(tempdir(), pkgn), recursive = TRUE)
})

test_that("init populates exisiting directory", {

  pkgn <- "initExist"
  path <- file.path(tempdir(), pkgn)
  dir.create(path)
  wd <- getwd()

  local({

    on.exit({
      setwd(wd)
      unlink(path, recursive = TRUE)
    })

    setwd(path)
    dir.create("processing")
    writeLines("read.csv('extdata/src.csv')", "processing/01.R")
    dir.create("extdata")
    write.csv(data.frame(1:10), "extdata/src.csv")
    expect_warning(
      dpr_init(),
      "(`processing`|`extdata`) was found"
    )
    expect_true('DESCRIPTION' %in% list.files())

  })

  unlink(path, recursive = TRUE)

})

test_that("no dpr_(description_)_init warning about default package name", {
  owd <- getwd()
  on.exit({
    setwd(owd)
    unlink(twd, recursive = TRUE)
  })
  twd <- tempfile()
  dir.create(twd)
  setwd(twd)
  expect_no_warning(dpr_init())
})

test_that("dpr_create throws error on non-existent directory", {
  expect_error(
    dpr_create(tempfile(), 'does not point to an existing directory')
  )
})
