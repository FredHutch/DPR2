library(DPR2)
tdir <- file.path(tempdir(), "packages")
dir.create(tdir)

test_that("check default package name warning", {
  dpr_init(tdir, desc=dpr_description_init(Package = "testPackage"))
  expect_warning(dpr_init(tdir), "Default package name used")
})

test_that("check DESCRIPTION file has populated fields", {
  dpr_init(
    tdir,
    desc = dpr_description_init(
      Package = "DescripPackage",
      Title   = "A package for testing description writing",
      Authors = "Foo Bar [aut, cre]"
    )
  )
    
  defi <- DPR2:::dpr_description_defaults()
  desc <- desc:::desc(file.path(tdir, "DescripPackage"))

  expect_true(desc$get("Package")     == "DescripPackage")
  expect_true(desc$get("Title")       == "A package for testing description writing")
  expect_true(desc$get("Description") == defi$Description)
    
})

test_that("checking package init", {

  ## init DPR2 package in temp

  #### check for datapackager.yml elements
  #### check for clean working environment names

  ## init DPR2 package in local dir
  #### check for datapackager.yml elements
  #### check for clean working directory names

})

## cleanup
unlink(
  file.path(tdir, "packages"),
  recursive=T
)
