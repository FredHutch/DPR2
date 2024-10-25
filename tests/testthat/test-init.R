tdir <- getPkgDir()

test_that("check default package name warning", {
  pkgn <- "testPackage"
  expect_silent(
    dpr_init(tdir, desc=dpr_description_init(Package = pkgn))
  )
  expect_warning(
    dpr_init(tdir, desc=dpr_description_init()),
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
  dpr_init(
    tdir,
    desc = dpr_description_init(
      Package = pkgn,
      Title   = "A package for testing description writing",
      Authors = "Foo Bar [aut, cre]"
    )
  )

  defi <- DPR2:::dpr_description_defaults()
  desc <- desc:::desc(file.path(tdir, "DescripPackage"))

  expect_true(desc$get("Package")     == "DescripPackage")
  expect_true(desc$get("Title")       == "A package for testing description writing")
  expect_true(desc$get("Description") == defi$Description)
  unlink(file.path(tdir, pkgn), recursive = TRUE)

})

test_that("check datapackager.yml", {

  pkgn = "testing"
  dpr_init(
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

  initPkg(tdir, pkgn, list(renv_init=TRUE))
  expect_true(dir.exists(file.path(path, "renv")))
  unlink(path, recursive = TRUE)

  initPkg(tdir, pkgn, list(renv_init=FALSE))
  expect_true(!dir.exists(file.path(path, "renv")))
  unlink(path, recursive = TRUE)

})

test_that("check stop on existing init directory", {

  pkgn <- "testPkg"
  path <- file.path(tdir, pkgn)
  initPkg(tdir, pkgn)

  expect_error(initPkg(tdir, pkgn), "path already exists")
  unlink(path, recursive = TRUE)

})
