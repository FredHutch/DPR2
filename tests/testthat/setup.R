getPkgDir <- function(){
  temp_dir <- file.path(tempdir(), "packages")
  dir.create(temp_dir)
  return(temp_dir)
}

cleanup <- function(temp_dir){
  if(dir.exists(temp_dir)){
    unlink(
      file.path(temp_dir),
      recursive=TRUE,
      force=TRUE
    )
  }
}

#' Private, for tests only. Create test data package.
#'
#' @param temp_dir Character, directory to contain the created data package
#' @param package_name Character, package name
#' @param more_args List of arguments additionally passed to [dpr_create()].
#'   Will overwrite presets defined in 'args' at beginning of function.
#'
#' @return Called for side effect of creating a test data package.
#' @noRd
createPkg <- function(temp_dir, package_name, more_args = list()){
  args <- list(
    path=temp_dir,
    desc=dpr_description_init(Package=package_name)
  )

  do.call(
    dpr_create,
    utils::modifyList(args, more_args, keep.null = TRUE)
  )

  path <- file.path(temp_dir, package_name)

  writeLines(
    c(
      "library(yaml)",
      "library(lubridate)",
      "library(DPR2)",
      "date('2024-01-01')", # test function masking of `date()`
      "mydataframe <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(mydataframe)",
      "objYml1 <- 'test objects values 1'",
      "objYml2 <- 'test objects values 2'",
      "dpr_save('mydataframe')",
      "save(yml, file=dpr_path('data', 'myyaml.rda'))"
    ),
    file.path(path, "processing/01.R")
  )

  writeLines(
    c(
      "---",
      "title: test report",
      "---",
      "test text",
      "```{r}",
      "library(DPR2)",
      "library(yaml)",
      "df  <- data.frame(x=1:10, y=LETTERS[1:10])",
      "yml <- as.yaml(df)",
      "save(df,  file=dpr_path('data', 'mydataframe_rmd.rda'))",
      "save(yml, file=dpr_path('data', 'myyaml_rmd.rda'))",
      "```"
    ),
    file.path(path, "processing/01.Rmd")
  )

  writeLines(
    c(
      "library(DPR2)",
      "mymatrix <- matrix(1:16, nrow=4)",
      "dpr_save('mymatrix')"
    ),
    file.path(path, "processing/02.R")
  )

  ## a processing script that accesses the datapackager.yml
  writeLines(
    c(
      "library(DPR2)",
      "dat <- as.list(LETTERS)",
      "ourLetters <- c('d', 'p', 'r')",
      "save(dat, file=dpr_path('data', 'letters.rda'))",
      "dpr_save('ourLetters')"
    ),
    file.path(path, "processing/A1.R")
  )

  ## check if environment is shared
  writeLines(
    c(
      "library(DPR2)",
      "mymatrix[1] <- 100",
      "newmatrix <- mymatrix",
      "dpr_save('newmatrix')"
    ),
    file.path(path, "processing/S1.R")
  )

  ## reproducibility: check no access to library only attached in main R process
  writeLines(
    c(
      "dpr_path()"
    ),
    file.path(path, "processing/nolib.R")
  )

  dpr_track_processes(c("01.R", "02.R", "01.Rmd"), file.path(args$path, package_name))

}
