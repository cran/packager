#' Set Options For Packages
#'
#' See \code{\link[fritools:set_options]{fritools:set_options}}
#'
#' @param package_name The package's name.
#' @param overwrite [boolean(1)]\cr Overwrite options already set?
#' @param ... See \code{\link{options}}.
#' @return invisible(TRUE)
#' @export
#' @keywords internal
#' @family option functions
set_options <- function(package_name, ..., overwrite = TRUE) {
    fritools::set_options(..., package_name = package_name, 
                          overwrite = overwrite)
}

#' Get Options For Packages
#'
#' See \code{\link[fritools:get_options]{fritools:get_options}}
#'
#' @param package_name The package's name.
#' @param ... See \code{\link{getOption}}.
#' @param remove_names [boolean(1)]\cr Remove the names?
#' @param flatten_list [boolean(1)]\cr Return a vector?
#' @return A (possibly named) list or a vector.
#' @family option functions
#' @keywords internal
#' @export
get_options <- function(package_name, ..., remove_names = FALSE,
                        flatten_list = TRUE) {
    fritools::get_options(..., package_name = package_name,
                          remove_names = remove_names,
                          flatten_list = flatten_list)
}
