# dpr_yaml_defaults

This function returns default key value pairs for a DPR2
`datapackager.yml` file. These defaults are the same for each package
and are read from the DPR2 installation directory.

## Usage

``` r
dpr_yaml_defaults()
```

## Value

a list containing default yaml key-value pairs from `datapackager.yml`

## Details

The keys returned here are required for all DPR2 data packages but the
users may set valid values to them by using `dpr_yaml_set(...)` or
simply editing the `datapackager.yml` file manually. Below is a list of
the keys, their default values, and any other valid values when
applicable.

- `source_data_directory` : `inst/extdata`, the location where source
  data for processing scripts

- `purge_data_directory` : `FALSE`, whether to delete all contents of
  the `data` directory when the package is rendered

- `process_directory` : `processing`, where to find the processing
  scripts to render

- `render_on_build` : `TRUE`, whether to render the processing scripts
  when the package is built or not

- `render_env_mode` : `isolate`, valid values are "isolate" or "share"
  which determine if each processing script is run in its own R session
  (isolate) or the same R session (share). No option allows processing
  scripts to be run in the current session.

- `write_to_vignettes` : `TRUE`, if vignettes should be generated when
  rendering

- `write_data_docs` : `TRUE`, if data documentation should be generated
  when rendering

- `to_build_directory` : `inst/to_build`, the location of the tracking
  directory for what script are built and objects are saved.

- `build_tarball` : `FALSE`, if a package tarball should be built when
  the package is built

- `install_on_build` : `FALSE`, if the package should be installed when
  the package is built

- `build_output` : `../`, where the package should be save if the
  tarball is built relative to the package root

There are also optional values can will be used but are not required.

- `process_on_build` : a character vector of which processing scripts to
  run from the `process_directory` location when the package is
  rendered. These overwrite what scripts have been assigned in
  `inst/to_build/scripts` with `dpr_add_scripts`.

- `objects` : which objects to save from the processing scripts
  environments. These values overwrite what objects have been assigned
  in `inst/to_build/objects` with `dpr_add_objects`.

- `r_session_wait_timeout` : How long should a callr session wait before
  timing out. Add this to `datapackager.yml` if `dpr_build` or
  `dpr_render` return timeout errors. Units are in milliseconds with the
  default being 3000. A value of 5000 or higher will most likely prevent
  timeout errors.
