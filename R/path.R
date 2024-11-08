#' Construct path from data package directory
#'
#' @param ... Trailing path components passed to file.path(). All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_path <- function(...){
  normalizePath(rprojroot::find_package_root_file(...), '/', FALSE)
}

#' Construct path from data package data directory
#'
#' @param ... Trailing path components passed to file.path(). All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_data_path <- function(...) project_path('data', ...)

#' Construct path from data package inst/extdata directory
#'
#' @param ... Trailing path components passed to file.path(). All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
project_extdata_path <- function(...) project_path('inst', 'extdata', ...)
