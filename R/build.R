dpr_yaml_exists <- function(args){
  if( file.exists(file.path(args[["package_root"]], "datapackager.yml")) )
    return(TRUE)
  return(FALSE)
}

dpr_read_yml <- function(){
  if( !dpr_yaml_exists() )
    stop("Either R `package_root` argument (see ?datapackager.yml) is not a data package, or 'datapackager.yml' is not found in data package.")
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

dpr_purge_data <- function(args){
  for(d in list.files(file.path(args[["package_root"]], "data"))){
    unlink(d)
  }
}

dpr_update_data_digest <- function(args){
  dat <- list.files(file.path(args[["package_root"]], "data"))
  for(d in dat){
    write(
      digest::sha1(d),
      file.path(
        args[["data_digest_directory"]],
        tools::file_path_sans_ext(basename(d))
      )
    )
  }
}

dpr_render <- function(args){
  if(args[["purge_data"]]) dpr_purge_data(args)
  for(src in args[["process_on_build"]]){
    ## knitr::knit or rmarkdown::render?
    rmarkdown::render(
      src,
      knit_root_dir = args[["package_root"]],
      output_dir = file.path(args[["package_root"]], "vignettes"),
      output_format = "md_document"
    )
  }
}

dpr_install <- function(args){
}

dpr_build <- function(...){
  args <- dpr_args(...)
  dpr_render(args)
  dpr_update_data_digest(args)
  ## could use functions in callr, or processx, or sys. 
  system(paste("cd", args[["build_ouput"]], "&& R CMD build", args[["package_root"]]))
  if(args[["install"]]){
    dpr_install(args)
  }
}
