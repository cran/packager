#' Install Dependencies From a Package's DESCRIPTION
#'
#' \pkg{remotes}' version does not seem to care for the required version. So it
#' always uses the latest?
#' We use the lowest version accepted by the definition in file DESCRIPTION.
#' @param path Path to the package.
#' @param ... passed to
#' \code{\link[remotes:install_version]{remotes::install_version}}.
#' @return TRUE
#' @keywords internal
#' @export
install_deps <- function(path = ".", ...) {
    deps <- desc::desc_get_deps(path)
    for (i in 2:nrow(deps)) {
        package <- deps[i, "package"]
        version <- deps[i, "version"]
        version <- gsub("[^0-9\\.]", "", version)
        version <- if (version == "") NULL else version
        try(remotes::install_version(package = package, version = version, 
                                     upgrade = "never", ...))
    }
    return(invisible(TRUE))
}


