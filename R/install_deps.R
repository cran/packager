#' Install Dependencies From a Package's DESCRIPTION
#'
#' \pkg{remotes}' version does not seem to care for the required version. So it
#' always uses the latest?
#' We use the lowest version accepted by the definition in file DESCRIPTION.
#' @param path Path to the package.
#' @param verbose Be verbose?
#' @param ... passed to
#' \code{\link[remotes:install_version]{remotes::install_version}}.
#' @return TRUE
#' @keywords internal
#' @export
install_deps <- function(path = ".", verbose = FALSE, ...) {
    deps <- desc::desc_get_deps(path)
    pkgs <- deps[deps[["package"]] != "R", "package"]
    for (package in pkgs) {
        if (isTRUE(verbose)) message("Checking for depencency ", package, ".")

        version <- deps[deps[["package"]] == package, "version"]
        version <- gsub("[^0-9\\.]", "", version)
        version <- if (version == "") NULL else version
        spsm <-  suppressPackageStartupMessages
        is_require  <- spsm(require(package, character.only = TRUE,
                                    quietly = TRUE))
        if (!is_require) {
            if (isTRUE(verbose))
                message("Can't find package ", package, ", trying to install.")
            try(remotes::install_version(package = package, version = version,
                                         upgrade = "never", ...))
        } else {
            is_version_sufficient <-
                is_version_sufficient(get_package_version(package),
                                                 if (is.null(version)) "0.0.0" else version)
            if (isTRUE(verbose))
                message("Found package ", package, ".")
            if (!is_version_sufficient) {
                if (isTRUE(verbose))
                    message("Version ", get_package_version(package),
                            " of package ", package, " is lower than ", version,
                            ", trying to install.")
                try(remotes::install_version(package = package, version = version,
                                             upgrade = "never", ...))
            }

        }
    }
    return(invisible(TRUE))
}


remove_suggests <- function(file = system.file("DESCRIPTION",
                                               package = "packager")) {

    deps <- desc::desc_get_deps(file)
    suggested <- deps[deps[["type"]] == "Suggests", "package"]
    utils::remove.packages(suggested)
}
