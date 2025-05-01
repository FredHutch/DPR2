#' Private. A function for updating the data digest components by generating
#' checksums for all .rda files in the specified directory
#'
#' @title dpr_update_data_digest
#' @param path path to data package
#' @param yml the yml list object used for the build
#' @return nothing
#' @noRd
dpr_update_data_digest <- function(path=".", yml){
  rda <- dpr_list_rda(path)
  dig <- file.path(path, "inst", "data_digest")

  if (! dir.exists(dig)) dir.create(dig, recursive = TRUE)

  unlink(list.files(dig, full.names = T))

  # throw error if dpr_checksum_files throws a warning
  warn <- options()$warn
  on.exit(options("warn" = warn))
  options("warn" = 2)
  for(d in rda)
    write(dpr_checksum_files(d), file.path(dig, paste0(basename(d), "_")))

}

#' Private. Verifies if necessary conditions are met for accessing Git features
#'
#' @title dpr_check_git
#' @param path path to data package
#' @return a logical value indicating if the conditions are satisfied
#' @noRd
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

#' Private. Reads a data digest source, validates it's contents, and returns a
#' list.
#'
#' @title dpr_validate_data_digest_source
#' @param path path to data package
#' @return a list of a data digest source
#' @author jmtaylor
#' @noRd
dpr_validate_data_digest_source <- function(path){
  data_digest <- list.files(
    file.path(path, file.path("inst", "data_digest")),
    full.names = TRUE
  )
  if(length(data_digest) == 0)
    stop("No digest files found. Has any data been added to the data package yet?")
  if(!all(grepl("\\.rda_$", data_digest, ignore.case = TRUE)))
    stop("Invalid file in `data_digest` directory. Should only contain files with an `.rda_` extension.")
  return(data_digest)
}

#' Return the current hashes recorded to the data digest directory generated
#' during the last build.
#'
#' @title dpr_data_digest
#' @param path path to data package
#' @return a data.frame with the names of the data files and the the
#'   corresponding hash values extracted from the data digest
#' @export
dpr_data_digest <- function(path="."){
  data_digest <- dpr_validate_data_digest_source(path)
  return(
    data.frame(
      name = gsub("_$", "", basename(data_digest)),
      data_digest_md5 = vapply(data_digest, readLines, "", USE.NAMES = FALSE)
    )
  )
}

#' Return table of current in-memory checksums for RDA files in the data directory.
#'
#' @title dpr_data_checksums
#' @param path path to data package
#' @return a data.frame containing the name of the rda object and the
#'   corresponding checksum
#' @author jmtaylor
#' @export
dpr_data_checksums <- function(path="."){
  rda <- dpr_list_rda(path)
  return(
    data.frame(
      name = basename(rda),
      data_md5 = dpr_checksum_files(rda)
    )
  )
}

#' A data digest comparison table, comparing the current data digest checksums
#' with the current file checksums in the data directory.
#'
#' Compare the current checksums for the data listed in the data directory with
#' the checksums listed in the data digest. The data digest is only updated when
#' the package is built, not when it is simply rendered using `dpr_render()`.
#' @title dpr_compare_data_digest
#' @param path path to data package
#' @return a data.frame with the file name, corresponding data in-memory
#'   checksum, existing data_digest checksum, and a boolean value indicating if
#'   they are same or not
#' @author jmtaylor
#' @export
dpr_compare_data_digest <- function(path="."){
  comp <- merge(
    dpr_data_checksums(path), # the checksums of the files in the data directory
    dpr_data_digest(path), # the checksums recorded to the data digest during the last build
    by = "name",
    all.x = TRUE,
    all.y = TRUE
  )

  comp$same <- comp$data_md5 == comp$data_digest_md5

  return(comp)
}

#' Private. Pulls rda blobs from the git objects database and returns
#' them loaded in environments with empty env as parent.
#'
#' @title dpr_git_hashes_to_envs
#' @param hashes a character vector of full sha1 hashes
#' @param path a path to a data package
#' @author jmtaylor
#' @noRd
dpr_hashes_to_envs <- function(hashes, path = ".") {
  envs <- lapply(hashes, function(hash){
    tmp <- tempfile()
    writeBin(
      git2r::content(git2r::lookup(repo=path, sha=hash), raw=TRUE),
      tmp
    )
    env <- new.env(parent = emptyenv())
    load(tmp, envir = env)
    unlink(tmp)
    return(env)
  })
  return(envs)
}

#' Private. Return a vector of md5 checksums from a list of environments.
#'
#' @title dpr_envs_to_checksums
#' @param envs environments to extract checksums
#' @author jmtaylor
#' @noRd
dpr_envs_to_checksums <- function(envs){
  vapply(
    envs,
    function(env){
      nms <- ls(envir=env)
      if(length(nms) > 1){
        msg <- "No checksum computed for RDA files containing more than 1 object."
        warning(msg)
        return(msg)
      } else {
        return(digest::digest(get(nms, envir=env), algo="md5"))
      }
    }, "")
}

#' Private. From a vector of hashes load the data and generate the checksum for
#' the objects in memory.  Return md5 hash of R file loaded into memory from the
#' path provided.
#'
#' @title dpr_hashes_to_checksums
#' @param hashes a character vector of full sha1 hashes
#' @param path a path to a data package
#' @return a character vector of checksums
#' @noRd
dpr_hashes_to_checksums <- function(hashes, path){
  dpr_envs_to_checksums(
    dpr_hashes_to_envs(hashes, path)
  )
}

#' This function will return an md5 checksum of an in-memory R object.
#'
#' @title dpr_checksum_files
#' @param paths path of RDA files to hash
#' @return a character string of the md5 hashes of a files loaded into memory.
#' @export
dpr_checksum_files <- function(paths){
  if(!all(file.exists(paths)))
    stop("Cannot generate checksums for some paths because they don't exists: ", paste(paths[!file.exists(paths)], collapse = ","))
  if(!all(grepl("\\.rda$", paths, ignore.case=TRUE)))
    stop("One or more paths provided are not RDA files. All other file types are not supported")
  return(
    vapply(paths, function(path) {
      load_env <- new.env()
      load(path, envir = load_env)
      return( dpr_envs_to_checksums(list(load_env)) )
    }, "", USE.NAMES = FALSE)
  )
}

#' Return the history of all the former and current files in the `data`
#' directory.
#'
#' @title dpr_data_history
#' @param path path to data package
#' @param include_checksums a boolean value indicating if checksums should be
#'   included in the returned data.frame object; computing checksums is less
#'   performant
#' @return a data.frame object with the git hash, file name, author name, time
#'   of creation and md5 checksum of the file
#' @export
dpr_data_history <- function(path=".", include_checksums=FALSE){
  if( !dpr_is(path) )
    stop("`path` argument is not a DataPackageR or DPR2 package.")

  if( !dpr_check_git(path) ){
    warning("Returning data digest comparision table instead of data history. This package is not under git version control.")
    return(dpr_compare_data_digest(path))
  }

  odb <- git2r::odb_blobs(path)
  odb <- odb[
    odb$path == "data",
    c("sha", "name", "author", "when")
  ]

  if(nrow(odb) == 0)
    stop("No files found at the `data` path commited to the git history.")

  names(odb)[names(odb) == "sha"] <- "blob_git_sha1"
  row.names(odb) <- 1:nrow(odb) # resetting the row names after subsetting

  if(include_checksums)
    odb$object_checksum <- dpr_hashes_to_checksums(odb$blob_git_sha1, path)

  return(odb)
}

#' Load a data version into memory by its hash. Only RDA files in the data
#' directory will be recalled.
#'
#' @title dpr_recall_data_version
#' @param hashes the git hashes of the data to recall; partial hashes allowed
#'   from 1 to 40 hexadecimal digits
#' @param path the path to the data package
#' @return a list with names of corresponding .rda object and the versions
#'   generated from the object
#' @export
dpr_recall_data_versions <- function(hashes, path = "."){
  dpr_is(path)
  if(!dpr_check_git(path))
    stop("Datapackage is not under git version control. Data can not be recalled.")
  odb <- git2r::odb_blobs(path)

  rda <- unique(
    odb[
      odb$path == "data" &
        grepl(paste0(paste0("^", hashes), collapse = "|"), odb$sha, ignore.case = TRUE) &
        grepl("\\.rda$", odb$name, ignore.case = TRUE),
      c("name", "sha")
    ]
  )

  if( nrow(rda) == 0 )
    stop("Data version not found. Either version hash is not found, or the hash does not point to an `rda` file in the `data` directory.")

  ## there can be multiple rdas with the same hash
  ## there can be multiple objects in each rda

  objs <- lapply(
    dpr_hashes_to_envs(rda$sha, path),
    function(env) mget(ls(env), envir = env)
  )

  names(objs) <- rda$name
  return(objs)
}
