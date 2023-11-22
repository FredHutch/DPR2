dpr_yml_load <- function(project_root = "./"){
  ## if being called during dpr_build
  if( exists("dpr_build_env") && exists("dpr_build_env$yml") )
    return( dpr_build_env$yml )
  ## looking for exising yml
  if( !file.exists(file.path(project_root, "datapackager.yml")) )
    stop("`here::here('datapackager.yml')` returns FALSE. Either R `package_root` argument (see ?datapackager.yml) is not a data package, or 'datapackager.yml' is not found in data package.")
  return( yaml::yaml.load_file(file.path(project_root, "datapackager.yml")) )
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_yml_get <- function(...){
  yml_dft <- dpr_yml_init()
  yml_set <- list(...)
  yml <- dpr_yml_load()
  for(a in names(yml_set))
    yml[[a]] <- yml_set[[a]]
  for(a in names(yml_dft)[!names(yml_dft) %in% names(yml)])
    yml[[a]] <- yml_dft[[a]]
  return(yml)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_yml_set <- function(...){
  yml <- dpr_yml_get(...)
  yaml::write_yaml(yml, here::here("datapackager.yml"))
}
