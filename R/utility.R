dpr_yaml_load <- function(pkgp){
  ## if being called during dpr_build
  if( exists("dpr_build_env") && exists("dpr_build_env$yaml") )
    return( dpr_build_env$yaml )
  ## looking for exising yaml
  if( !file.exists("datapackager.yml") )
    stop("`datapackager.yml` does not exist. Either working directory is not at a package root, or 'datapackager.yml' is not found in data package.")
  return( yaml::yaml.load_file(file.path(pkgp, "datapackager.yml")) )
}

dpr_description_load <- function(pkgp){
  desc::desc(file = pkgp)
}

dpr_set_keys <- function(old, new, missing=NULL){
  ## replace any new keys with new
  for(a in names(new))
    old[[a]] <- new[[a]]
  ## replace any missing keys with missing
  for(a in names(missing)[!names(missing) %in% names(old)])
    old[[a]] <- missing[[a]]
  return(old)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param ... 
##' @return 
##' @author jmtaylor
##' @export
dpr_yaml_get <- function(..., pkgp="."){
  new <- list(...)
  yml <- dpr_yaml_load(pkgp) |>
    dpr_set_keys( new)
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
dpr_yaml_set <- function(..., pkgp="."){
  yml <- dpr_yaml_get(...)
  def <- dpr_yaml_defaults()
  new <- dpr_set_keys(yml, def)
  yaml::write_yaml(new, file.path(pkgp, "datapackager.yml"))
}

dpr_description_set <- function(..., pkgp="."){
    new <- list(...)
    def <- dpr_description_defaults()
    Map(desc::desc_set_list, key = names(defa), list_value = defa, file = pkgp) |>
        invisible()
    Map(desc::desc_set_list, key = names(desc), list_value = desc, file = pkgp) |>
        invisible()
}

