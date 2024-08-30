##' Private. A function for updating the data digest components.
##'
##' @title dpr_update_data_digest
##' @param path path to data package
##' @param yml the yml object used for the build
##' @return nothing
##' @author jmtaylor
dpr_update_data_digest <- function(path=".", yml){
  rda <- dpr_list_rda(path)
  dig <- file.path(path, yml$data_digest_directory)

  if(!dir.exists(dig))
    stop(sprintf("Data digest directory does not exist: %s", dig))

  unlink(list.files(dig, full.names = T))
  
  for(d in rda)
    write(dpr_hash_file(d), file.path(dig, paste0(basename(d), "_")))

}

##' Private. Standard way of listing rda files available in data for use across all functions.
##'
##' @title dpr_list_rda
##' @param path path to data package
##' @return a character vector of rda files in the data directory
##' @author jmtaylor
dpr_list_rda <- function(path){
  sort(
    list.files(
      file.path(path, dpr_yaml_get(path)$data_directory),
      "\\.[Rr][Dd][Aa]$",
      full.names = T
    )
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

##' Private. Checks for nessessary git features for using dpr_data_history.
##'
##' @title dpr_check_git
##' @param path path to data package
##' @return boolean
##' @author jmtaylor
dpr_check_git <- function(path){
  git <- c(TRUE, TRUE)
  if( !"git2r" %in% row.names(utils::installed.packages()) ){
    warning("Please install `git2r` to use `dpr_data_history` with version control.")
    git[1] <- FALSE
  }
  if( !dir.exists(file.path(path, ".git")) ){
    warning("Package is not under git version control (no `.git` directory found at package path).")
    git[2] <- FALSE
  }
  return(all(git))  
}

##' Return hash of file at the path provided. Hash is equivalent to the git blob hash of the file.
##'
##' This function will return a git style sha1 hash of a file found at the path argument. Git style
##' hashing pads the front of the file with "blob" the length of the file in bytes, a null
##' character, and then the binary content of the file itself. This style of hashing is used here to
##' ensure that data objects created across objects that have the same blob can be compared whether
##' the package is under version control or not, or packages that are put under version control at
##' future dates can compare objects with versions of the package from before version control was
##' applied.
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

##' Return the current hashes recorded to the data digest directory during the last build. 
##'
##' @title dpr_data_digest
##' @param path path to data package 
##' @return a data.frame showing the hashes found in the data digest
##' @author jmtaylor
##' @export 
dpr_data_digest <- function(path="."){
  ddig <- list.files(
    file.path(path, dpr_yaml_get(path)$data_digest_directory),
    full.names = TRUE
  )
  return(
    data.frame(
      name = gsub("\\.rda_$", ".rda", basename(ddig)),
      data_digest_hash = vapply(ddig, readLines, ""),
      row.names = seq_along(ddig)
    )
  )
}

##' Return table of current hashes for rda files in the data directory.
##'
##' @title dpr_data_hashes
##' @param path path to data package
##' @return a data.frame
##' @author jmtaylor
##' @export
dpr_data_hashes <- function(path="."){
  rda <- dpr_list_rda(path)
  return(
    data.frame(
      name = basename(rda),
      data_hash = vapply(rda, dpr_hash_file, ""),
      row.names = seq_along(rda)
    )
  )
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

##' A data digest comparison table, comparing the current data digest with the current data hashes.
##' 
##' Compare the current hashes for the data listed in the data directory with the hashes listed in
##' the data digest. The data digest is only updated when the package is built, not when it is
##' simply rendered using `dpr_render()`.
##' @title dpr_compare_data_digest
##' @param path path to data package
##' @param display_digits how many digits in the compare to display
##' @return a data.frame
##' @author jmtaylor
##' @export
dpr_compare_data_digest <- function(path=".", display_digits = 7){
  comp <- merge(
    dpr_data_hashes(path),
    dpr_data_digest(path),
    by = "name",
    all.x = T,
    all.y = T
  )

  comp$same <-
    comp$data_hash == comp$data_digest_hash
  comp$data_hash <-
    substring(comp$data_hash, 1, display_digits)
  comp$data_digest_hash <-
    substring(comp$data_digest_hash, 1, display_digits)

  return(comp)
}

##' Return the history of all the former and current files in the `data` directory.
##'
##' @title dpr_data_history
##' @param path path to data package
##' @param include_checksums a boolean value indicating if checksums should be included in the returned data.frame object; computing checksums is less performant
##' @return a data.frame object
##' @author jmtaylor
##' @export
dpr_data_history <- function(path=".", include_checksums=FALSE){
  dpr_is(path)

  if( !dpr_check_git(path) ){
    warning("Returning data digest comparision table instead of data history.")
    return(dpr_compare_data_digest(path))
  }
  
  odb <- git2r::odb_blobs(path)
  odb <- odb[
    odb$path == dpr_yaml_get(path)$data_directory,
    c("sha", "path", "name", "author", "when")
  ]
  names(odb)[names(odb) == "sha"] <- "blob_file_hash"
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

##' Load a data version into memory by its hash. Only rda files in the data directory will be recalled.
##'
##' @title dpr_recall_data_version
##' @param version the hash of the data to recall; partial hashes allowed from 1 to n digits
##' @param path the path to the data package
##' @return a list of objects loaded from an rda file pulled from the git history
##' @author jmtaylor
##' @export 
dpr_recall_data_version <- function(version, path = "."){
  dpr_is(path)
  if(!dpr_check_git(path))
    stop("Datapackage is not under git version control. Data can not be recalled.")
  odb <- git2r::odb_blobs(path)
  rda <- unique(
    odb[ grepl(paste0("^", version), odb$sha), c("name", "path", "sha") ]
  ) 
  rda <- rda[ rda$path == "data", ]
  rda <- rda[ grepl("\\.[Rr][Dd][Aa]$", rda$name) ]
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
