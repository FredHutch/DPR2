#' Construct path from data package directory
#'
#' @param ... Trailing path components passed to [file.path()]. All arguments must
#'   be the same length or length one.
#' @return The normalized path with the additional path components appended.
#'   Throws an error if no root is found.
#' @export
dpr_path <- function(...) rprojroot::find_package_root_file(...)
