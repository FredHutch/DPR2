##' Private. Load `datapackager.yml` into memory. If missing, throw exception.
##'
##' @title dpr_yaml_load
##' @param pkgp the package path
##' @return a yaml object
##' @author jmtaylor
dpr_yaml_load <- function(pkgp){
  ## looking for exising yaml
  if( !file.exists(file.path(pkgp, "datapackager.yml")) )
    stop("`datapackager.yml` does not exist. Either working directory is not at a package root, or 'datapackager.yml' is not found in data package.")
  return( yaml::yaml.load_file(file.path(pkgp, "datapackager.yml")) )
}

##' Private. Check yaml values that must be specific.
##'
##' @title dpr_yaml_validate
##' @param yml a parsed yaml object
##' @author jmtaylor
dpr_yaml_validate <- function(yml){

  ## check that all controlled key values are correct
  key_value = list(
    "render_env_mode" = c("isolate", "share")
  )
  for(key in names(key_value)){
    if(!yml[[key]] %in% key_value[[key]]){
      stop(
        sprintf(
          "Invalid `%s` yaml value used. Please one of these: %s",
          key, paste(key_value[[key]], collapse = ", ")
        )
      )
    }
  }

  ## check that all default yaml values are present, should catch typos manually entered in yaml
  def <- dpr_yaml_defaults()
  nameCheck <- !(names(def) %in% names(yml))
  if(any(nameCheck)){
    nm <- paste(names(def)[nameCheck], collapse = ",")
    stop(
      "The following required yaml values are not found: ", nm, ". ",
      "Please add those using `dpr_yaml_set()`, or directly in the `datapackager.yml` file."
    )
  }

}

##' Private. Load the package DESCRIPTION file into memory.
##'
##' @title dpr_description_load
##' @param pkgp the package path
##' @return a desc object
##' @author jmtaylor
dpr_description_load <- function(pkgp){
  desc::desc(file = pkgp)
}

##' Private. Replace and add key:value pairs in old list with new.
##'
##' @title dpr_set_keys
##' @param old key:values list object
##' @param new key:values list object to add/modify
##' @return list
##' @author jmtaylor
dpr_set_keys <- function(old, new){
  ## replace any new keys with new
  for(nm in names(new))
    old[[nm]] <- new[[nm]]
  return(old)
}

##' Get the current datapackager.yml key:value pairs and add to or
##' modify those with with new values. All additions and modifications
##' happen only in memory, not on disc.
##'
##' @title dpr_yaml_get
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key:value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return list
##' @author jmtaylor
##' @export
dpr_yaml_get <- function(path=".", ...){
  new <- list(...)
  yml <- dpr_set_keys(
    dpr_yaml_load(path), 
    new
  )
  dpr_yaml_validate(yml)
  return(yml)
}

##' Write new datapackager.yml with new or modified key:value pairs.
##'
##' @title dpr_yaml_set
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_yaml_set <- function(path=".", ...){
  yml <- dpr_yaml_get(path)
  new <- dpr_set_keys(yml, list(...))
  yaml::write_yaml(new, file.path(path, "datapackager.yml"))
}

##' Write new DESCRIPTION file with new or modified key:value pairs.
##'
##' @title dpr_description_set
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @param ... datapakager.yml value overrides. When arguments are
##'   specified, those arguments are used as the YAML key value pairs
##'   instead of what is specified by the `datapackager.yml`.
##' @return nothing
##' @author jmtaylor
##' @export
dpr_description_set <- function(path=".", ...){
  new <- list(...)
  def <- dpr_description_defaults()
  invisible(
      Map(desc::desc_set_list, key = names(def), list_value = def, file = path)
  )
  invisible(
      Map(desc::desc_set_list, key = names(new), list_value = new, file = path)
  )
}

##' Retrieve data.frame of data object names and versions rendered to the data directory.
##'
##' @title dpr_data_versions
##' @param path The full path to the data package. The default is the
##'   working directory.
##' @return data.frame
##' @author jmtaylor
##' @export
dpr_data_versions <- function(path="."){
  dat <- list.files(file.path(path, dpr_yaml_get(path)$data_digest_directory), full.names=TRUE)
  data.frame(
    object=basename(dat),
    digest=vapply(dat, function(d) readLines(d), ""),
    row.names=seq(1,length(dat))
  )
}

##' Return hash of file at the path provided. Hash is equivalent to the git blob hash of the file.
##'
##' @title dpr_hash_file
##' @param path path of file to hash
##' @return a character string of the SHA1 hash of the file using git style hash padding.
##' @author jmtaylor
##' @export
dpr_hash_file <- function(path){
  digest::digest(
    c(
      charToRaw(paste0("blob ", file.info(path)$size)),
      as.raw(0),
      readBin(path, "raw", file.info(path)$size)
    ), algo = "sha1", serialize = F
  )
}

##' Private. Get md5 checksum of object once loaded from a path.
##'
##' @title dpr_checksum_data_objects
##' @param path a path to an rda file
##' @return a string of an md5 checksum of an object loaded from a path
##' @author jmtaylor
dpr_checksum_data_objects <- function(path){
  env <- new.env(parent=emptyenv())
  load(path, env)
  return(
    digest::digest(
      get(ls(envir=env), envir=env),
      algo="md5"
    )
  )
}

##' A convenience function for writing data objects to the package data directory.
##'
##' @title dpr_save
##' @param object an object to save to the path set for the yaml data_directory value 
##' @author jmtaylor
##' @export
dpr_save <- function(object){
  save(
    object,
    file = file.path(dpr_yaml_get()$data_directory, paste0(deparse(substitute(object)), ".rda"))
  )
}

##' Load a data version into memory by its hash. Only rda files in the data directory will be recalled.
##'
##' @title dpr_recall_data_version
##' @param version the hash of the data to recall; partial hashes allowed from 1 to n digits
##' @param path the path to the data package
##' @return a list of objects loaded from an rda file pulled from the git history
##' @author jmtaylor
##' @export 
dpr_recall_data_version <- function(version, path = "."){
  odb <- git2r::odb_blobs(path)
  rda <- unique(
    odb[ grepl(paste0("^", version), odb$sha), c("name", "path", "sha") ]
  ) 
  rda <- rda[ rda$path == "data", ]
  rda <- rda[ grepl("\\.[Rr][Dd][Aa]", rda$name) ]
  if( nrow(rda) == 0 )
    stop("Data version not found. Either version hash is not found, or the hash does not point to an `rda` file in the `data` directory.")
  if( length(unique(rda$sha)) != 1 )
    stop("Multiple hashes found from partial hash. Please use more digits.")
  tmps <- file.path(tempdir(), rda$name)

  ## there can be multiple rdas with the same hash
  ## there can be multiple objects in each rda
  ## this is a feature of rda files, but will it be a feature of rda files created by DPR2?

  recall <- list()
  tryCatch({
    for(tmp in tmps){
      writeBin(
        git2r::content(git2r::lookup(repo=path, sha=version), raw=T),
        tmp
      )
      env <- new.env(parent = emptyenv())
      load(tmp, envir = env)
      objs <- ls(envir = env)
      out <- list()
      ## using for loop to assign names
      for(obj in objs)
        out[[obj]] <- get(obj, envir = env)
      recall[[tmp]] <- out
    }
  }, error = function(e) { rm(env); stop(e) }
  )
  return(recall)
}

##' Get a checksum of an object from the git history.
##'
##' @title dpr_checksum_from_version
##' @param version a string hash of a data version
##' @param path a path to a data package
##' @return a string of a checksum from an object recalled from the git history
##' @author jmtaylor
##' @export 
dpr_checksum_from_version <- function(version, path = "."){
  recall <- dpr_recall_data_version(version, path)
  checksums <- list()
  for(ck in names(recall))
    checksums[[ck]] <- digest::digest(recall[[ck]], algo="md5")
  return(checksums)
}

##' Return the history of all the former and current files in the `data` directory.
##'
##' @title dpr_data_history
##' @param include_checksums a boolean value indicating if checksums should be included in the returned data.frame object; computing checksums is less performant
##' @param path path to data package
##' @return a data.frame object
##' @author jmtaylor
##' @export
dpr_data_history <- function(include_checksums=FALSE, path="."){
  if( !dir.exists(file.path(path, "data")) )
    stop("`data` directory not found. Is the path pointing to a data package?")
  if( !"git2r" %in% row.names(utils::installed.packages()) )
    stop("Please install `git2r` to use `dpr_data_history`.")
  odb <- git2r::odb_blobs(path)
  dat <- list.files(file.path(path, "data"))
  odb <- odb[odb$name %in% dat, !names(odb) %in% c("commit", "len")]
  names(odb)[names(odb) == "sha"] <- "blob_file_hash"
  odb <- odb[,names(odb)[c(1, 2, 3, 4, 5)]]
  row.names(odb) <- 1:nrow(odb)
  if(include_checksums){
    cksum <- list()
    for(hash in odb$blob_file_hash){
      record <- dpr_checksum_from_version(hash, path)
      cksum[[hash]] <- data.frame(
        blob_file_hash = hash,
        name = basename(names(record)),
        object_md5 = unlist(record)
      )
    }
    cksum <- do.call(rbind, cksum)
    odb <- merge(odb, cksum) 
  }
  return(odb)
}
