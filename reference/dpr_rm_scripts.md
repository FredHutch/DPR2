# dpr_rm_scripts

Remove processing scripts from list of scripts rendered when a package
is rendered or built.

## Usage

``` r
dpr_rm_scripts(scripts, path = ".")
```

## Arguments

- scripts:

  names of scripts, found in the processing_directory set in the
  `datapackager.yml` file, to be removed from to_build and no long run
  when a script is rendered.

- path:

  path to a DPR2 data package

## Details

This function will remove processing scripts from those that will be
rendered. To add processing scripts, see
[`?dpr_add_scripts`](https://fredhutch.github.io/DPR2/reference/dpr_add_scripts.md).
To generate a table of scripts currently being added, see
[`?dpr_scripts`](https://fredhutch.github.io/DPR2/reference/dpr_scripts.md).
