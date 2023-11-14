dpr_yaml_exists <- function(yml_path = "datapackager.yml"){
  if( !file.exists(yml_path) )
    return(FALSE)
  return(TRUE)
}

dpr_read_yml <- function(){
  if( !dpr_yaml_exists() )
    stop("Either R session is not in DPR2 package root or 'datapackager.yml' is not available in data package.")
  return(yaml::yaml.load_file("datapackager.yml"))
}

dpr_args <- function(...){
  args <- list(...)
  yaml <- dpr_read_yml()
  tmpl <- default_yaml
  for(arg in names(args)){
    yaml[[arg]] <- args[[arg]]
  }
  return(yaml)
}

dpr_purge_data <- function(...){
  args <- dpr_args(...)
  if(dpr_args[["data_purge"]]){
    for(d in list.files("data")){
      unlink(d)
    }
  }
}

dpr_update_data_digest <- function(...){
  args <- dpr_args(...)
  ddir <- "data"
  ddat <- list.files(ddir)
  for(d in file.path(ddir, ddat)){
    write(
      digest::sha1(d),
      file.path(
        args[["data_digest_directory"]],
        tools::file_path_sans_ext(basename(d))
      )
    )
  }
}

dpr_build <- function(..., dpr_path = getwd()){
  args <- dpr_args(...)
  if(args[["purge_data"]]) dpr_purge_data(...)
  for(src in args[["process_on_build"]]){
    ## knitr::knit or rmarkdown::render?
    rmarkdown::render(
      src,
      knit_root_dir = getwd(),
      output_dir = file.path(getwd(), "vignettes"),
      output_format = "md_document"
    )
  }
  dpr_update_data_digest(...)
  ## could use functions in callr, or processx, or sys. 
  system(paste("cd", args[["build_ouput"]], "&& R CMD build", dpr_path))
  if(args[["install"]]){
    
  }
}
