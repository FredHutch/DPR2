#' Private. Generate Documentation Files for All .Rda Data Objects.
#'
#' Finds all `.rda` data objects in the data folder, generates
#' documentation using `template_doc_block`, and writes documentation
#' to `.R` files using `write_doc_file`.
#'
#' @param path Path to data package.
#' @return Creates `.R` files with documentation for each `.rda` data object.
#' @author valduran18
#' @noRd
generate_all_docs <- function(path = ".") {

  if (!is.character(path)) stop("`path` must be a character string.")

  out_dir <- file.path(path, "R")

  # check the output folder exists
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  data_env <- new.env()

  # get all .rda files in the data folder
  rda_files <- list.files(file.path(path, "data"), pattern = "\\.rda$", full.names = TRUE)
  # load into environment and rename the object to the filename in cases where object was saved using save()
  for (file in rda_files) {
    filename <- tools::file_path_sans_ext(basename(file))

    temp_env <- new.env()
    load(file, envir = temp_env)

    # get obj name
    object_name <- ls(temp_env)

    if (length(object_name) == 1) {
      if (object_name != filename) {
        warning(sprintf("'%s.rda' does not match data object name '%s'. Will skip writing documentation for it.", filename, object_name))
      } else {
        # get object
        obj <- get(object_name, envir = temp_env)

        # assign the object to the filename
        data_env[[filename]] <- obj
      }

    } else {
      warning(sprintf("'%s' contains multiple or no objects. Will skip writing documentation for it.", basename(file)))
    }
  }

  all_objects <- ls(data_env)

  # select only those objects that differ between digest file and data directory
  no_change <- tryCatch({
    digest_data <- dpr_compare_data_digest(path) #function returns error if no digest source is found. Will cause issues when building a brand new pkg.
    gsub("\\.rda$", "", digest_data$name[digest_data$same == TRUE])
  },
  error = function(e) {
    if (grepl("No digest files found. Has any data been added to the data package yet?", e$message)) {
      return(NA)
    } else {
      stop(e)
    }
  }
  )

  # if object does not have a R doc file for some reason, remove from list
  if (length(no_change) > 0) {
    doc_files <- list.files(out_dir, all.files = TRUE, no.. = TRUE)
    doc_basenames <- tools::file_path_sans_ext(basename(doc_files))
    no_change <- intersect(no_change, doc_basenames)
  }

  objects <- setdiff(all_objects, no_change)

  if (length(objects) == 0) {
    message("No new data object documentation created, as no objects have been modified and all objects are documented in `.R`.")
  }

  for (object_name in objects) {
    tryCatch({
      # return object value
      object <-  get(object_name, envir = data_env)

      # generate roxygen documentation block
      doc_block <- template_doc_block(object, object_name)

      # write the documentation block to an .R file
      write_doc_file(out_dir, doc_block, object_name)
    }, error = function(e) {
      warning(sprintf("Error processing '%s': %s", object_name, e$message))
    })
  }

  delete_unused_doc_files(path)

}

#' Private. Write Roxygen Documentation to an .R File.
#'
#' Writes the generated roxygen documentation block to an .R file in the R directory.
#'
#' @param out_dir The folder where the .R file will be saved.
#' @param doc_block A character vector containing the roxygen documentation lines.
#' @param object_name The name of the data object.
#' @author valduran18
#' @noRd
write_doc_file <- function(out_dir, doc_block, object_name) {
  doc_path <- file.path(out_dir, paste0(object_name, ".R"))
  writeLines(doc_block, con = doc_path)
}

#' Private. Generate Roxygen Documentation Block for a Data Object.
#'
#' Creates a roxygen-style documentation block for a given data object, including details on format,
#' fields, and source.
#'
#' @title template_doc_block
#' @param object an rda data object,
#' @param object_name The name of the data object.
#' @return a character vector of the roxygen documentation lines.
#' @author valduran18
#' @noRd
template_doc_block <- function(object, object_name) {
  # begin roxygen comment block
  doc_block <- c(
    "# This is data documentation created by DPR2. Do not delete this line.",
    "",
    paste0("#' ", object_name),
    "#'",
    "#' A detailed description of the data",
    "#'"
  )

  if (is.data.frame(object) || is.matrix(object)) {
    object_format <- paste0("#' ", "@format A ", class(object)[1],
                            " with ", nrow(object), " rows and ", ncol(object),
                            " columns with the following fields:")
    describe_fields <- names(object)
  } else if (is.list(object)) {
    object_format <- paste0("#' ", "@format A ", class(object)[1],
                            " with ", length(object),
                            " elements:")
    describe_fields <- names(object)
  } else if (is.atomic(object)) {
    object_format <- paste0("#' ", "@format A ", class(object)[1],
                            " vector of length ", length(object),
                            ".")
    describe_fields <- NULL
  } else {
    object_format <- paste0("#' ", "@format An object of class ", class(object)[1],
                            ".")
    describe_fields <- NULL
  }

  doc_block <- c(doc_block, object_format)

  if(!is.null(describe_fields) && length(describe_fields > 0)) {
    # add variable descriptions
    doc_block <- c(doc_block, "#' \\describe{")

    for (var in names(object)) {
      var_type <- class(object[[var]])[1]
      doc_block <- c(doc_block, paste0("#'   \\item{", var, "}{", var_type, "}", "{}"))
    }
    doc_block <- c(doc_block,  "#' }")
  }

  # end roxygen comment block
  doc_block <- c(
    doc_block,
    "#' @source Generated from script _________________",
    "#' @seealso",
    "#' \\link{}",
    paste0('"', object_name, '"')
  )
}

#' Private. Delete unused R files from the "R" directory.
#'
#' Compares files in the "R" directory with files in the "data" directory and deletes
#' files in the "R" directory that are not present in the "data" directory.
#'
#' @title delete_unused_doc_files
#' @param path Character. Path to the root of the data package (default is the current directory ".").
#' @return A logical vector indicating whether the files were successfully deleted.
#' @author valduran18
#' @noRd
delete_unused_doc_files <- function(path = ".") {

  doc_path <- file.path(path, "R")
  doc_files <- tools::file_path_sans_ext(list.files(doc_path, all.files = TRUE, no.. = TRUE))
  data_path <- file.path(path, "data")
  data_files <- tools::file_path_sans_ext(list.files(data_path, all.files = TRUE, no.. = TRUE))
  # only select files that have data documentation
  is_data_doc <- vapply(paste0(doc_files, ".R"), function(file) {
    full_path <- file.path(doc_path, file)
    lines <- tryCatch(readLines(full_path, warn = FALSE),
                      error = function(e) {
                        return("")
                      })

    any(grepl("# This is data documentation created by DPR2. Do not delete this line.", lines))
  }, logical(1))

  data_doc_files <- doc_files[is_data_doc]

  remove_files <- setdiff(data_doc_files, data_files)

  if (length(remove_files) > 0) {
    remove_files <- paste0(remove_files, ".R")
    file_paths <- file.path(doc_path, remove_files)
    invisible(file.remove(file_paths))
  } else {
    invisible(NULL)
  }
}

#' Private. Check if data directory is empty.
#'
#' Lists files in data directory and checks if the returned list of files is empty.
#'
#' @title data_is_empty
#' @param path Path to data package.
#' @return a logical value indicating TRUE for an empty directory and FALSE if not.
#' @author valduran18
#' @noRd
data_is_empty <- function(path = "."){
  data_path <- file.path(path, "data")
  files <- list.files(data_path, all.files = TRUE, no.. = TRUE)
  length(files) == 0
}
