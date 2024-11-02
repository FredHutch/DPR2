tdir <- getPkgDir()

test_that("check default package name warning", {
  pkgn <- "testPackage"
  expect_silent(
    dpr_create(tdir, desc=dpr_description_init(Package = pkgn))
  )
  expect_warning(
    dpr_create(tdir, desc=dpr_description_init()),
    "Default package name used"
  )
  unlink(file.path(tdir, pkgn), recursive = TRUE)
  unlink(
    file.path(
      tdir,
      suppressWarnings({DPR2::dpr_description_init()$Package}),
      recursive = TRUE
    )
  )
})

test_that("check DESCRIPTION file has populated fields", {
  pkgn <- "DescripPackage"
  pkgt <- "A package for testing description writing"
  dpr_create(
    tdir,
    desc = dpr_description_init(
      Package = pkgn,
      Title   = pkgt,
      Authors = "Foo Bar [aut, cre]"
    )
  )

  defi <- DPR2:::dpr_description_defaults()
  desc <- desc:::desc(file.path(tdir, pkgn))

  expect_true(desc$get("Package")     == pkgn)
  expect_true(desc$get("Title")       == pkgt)
  expect_true(desc$get("Description") == defi$Description)
  unlink(file.path(tdir, pkgn), recursive = TRUE)

})

test_that("check datapackager.yml", {

  pkgn = "testing"
  dpr_create(
    tdir,
    yaml=dpr_yaml_init(process_directory = "data-raw"),
    desc=dpr_description_init(Package = pkgn)
  )

  ## yml names manually set but have the manual values, other values unchanged
  yml <- dpr_yaml_get(file.path(tdir, "testing"))
  ymlInit <- dpr_yaml_init()

  expect_true(yml$process_directory == "data-raw")
  expect_true(yml$data_digest_directory == ymlInit$data_digest_directory)

  ## all names in init must be in package yaml
  expect_true(
    all(
      names(ymlInit) %in% names(yml)
    )
  )

  unlink(file.path(tdir, pkgn), recursive = TRUE)
})

test_that("check renv", {

  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)

  createPkg(tdir, pkgn, list(renv_init=TRUE))
  expect_true(dir.exists(file.path(path, "renv")))
  unlink(path, recursive = TRUE)

  createPkg(tdir, pkgn, list(renv_init=FALSE))
  expect_true(!dir.exists(file.path(path, "renv")))
  unlink(path, recursive = TRUE)

})

test_that("init populates exisiting directory", {

  pkgn <- "initExist"
  path <- file.path(tdir, pkgn)
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
      "[`processing`|`extdata`] was found"
    )
    expect_true('DESCRIPTION' %in% list.files())

  })

  unlink(path, recursive = TRUE)

})

cleanup(tdir)
