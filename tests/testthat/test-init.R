tdir <- getPkgDir()

test_that("check default package name warning", {
  dpr_init(tdir, desc=dpr_description_init(Package = "testPackage")) |>
    expect_silent()
  dpr_init(tdir, desc=dpr_description_init()) |>
    expect_warning("Default package name used")
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

test_that("check datapackager.yml", {

  dpr_init(
    tdir,
    yaml=dpr_yaml_init(process_directory = "data-raw"),
    desc=dpr_description_init(Package = "testing")
  )

  ## yml names manually set but have the manual values, other values unchanged
  yml <- dpr_yaml_get(file.path(tdir, "testing"))
  ymlInit <- dpr_yaml_init()

  expect_true(yml$process_directory == "data-raw")
  expect_true(yml$data_digest_directory == ymlInit$data_digest_directory)

  ## all names in init must be in package yaml
  expect_true(names(ymlInit) %in% names(yml) |> all())

})

cleanup(tdir)
