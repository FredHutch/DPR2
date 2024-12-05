#' Private. Generate Documentation Files for All .Rda Data Objects
#'
#' Finds all `.rda` data objects in the data folder, generates documentation using `template_doc_block`,
#' and writes documentation to `.R` files using `write_doc_file`.
#'
#' @param path Path to data package.
#' @param out_dir The path to the directory where the `.R` files will be saved (default is "R")
#' @return Creates `.R` files with documentation for each `.rda` data object.
#' @author valduran18
#' @noRd
generate_all_docs <- function(path = ".", out_dir = "R", env = NULL) {

  if(!is.character(path)) stop("`path` must be a character string.")
  if(!is.character(out_dir)) stop("`out_dir` must be a character string.")
  if (!is.null(env) && !is.environment(env)) stop("`env` must be an environment or NULL.")

  out_dir <- file.path(path, out_dir)

  # Ensure the output folder exists
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  if(is.null(env)){
    data_env <- new.env()
  } else {
    data_env <- env
  }

  # get all .rda files in the data folder
  all_objects <- ls(data_env)

  if (length(all_objects) == 0) {
    warning("The environment is empty. No objects to document.")

  }

  # select only those objects that differ between digest file and data directory
  digest_data <- dpr_compare_data_digest(path)
  no_change <- gsub(".rda", "", digest_data$name[digest_data$same == TRUE])

  objects <- setdiff(all_objects, no_change)

  if (length(objects) == 0) {
    warning("No new data objects have been created, and no existing objects have been modified. There are no objects to document.")

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
}

#' Private. Write Roxygen Documentation to an .R File
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

#' Private. Generate Roxygen Documentation Block for a Data Object
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
    paste0("#' ", object_name),
    "#'",
    "#' A detailed description of the data",
    "#'",
    paste0("#' ", "@format A ", class(object)[1], " with ", nrow(object), " rows and ", ncol(object), " columns with the following fields:"
    ))

  # add variable descriptions
  doc_block <- c(doc_block, "#' \\describe{")

  for (var in names(object)) {
    var_type <- class(object[[var]])[1]
    doc_block <- c(doc_block, paste0("#'   \\item{", var, "}{", var_type, "}", "{}"))
  }

  # end roxygen comment block

  doc_block <- c(doc_block,
                 "#' }",
                 "#' @source Generated from script _________________",
                 "#' @seealso",
                 "#' \\link{}",
                 paste0('"', object_name, '"'))

}
