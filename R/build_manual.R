#' Build a Package's Manual
#'
#' \pkg{devtools} version
#' (\code{\link[devtools:build_manual]{devtools::build_manual}}) does not run
#' \pkg{roxygen} first and by defaults puts it in ../ instead of .Rcheck
#' @param path Path to the package.
#' @param output_directory Where to put the manual. Defaults to the Rcheck
#' directory.
#' @param roxygenise Run  \pkg{roxygen} first?
#' @param verbose Be verbose?
#' @export
#' @return \code{\link[base:invisible]{Invisibly}} the value of the call to
#' \code{R CMD Rd2pdf}.
build_manual <- function(path = ".", output_directory = NULL,
                         roxygenise = TRUE, verbose = TRUE) {
    if (requireNamespace("roxygen2", quietly = TRUE) &&
        isTRUE(roxygenise)) {
        roxygen2::roxygenise(package.dir = path)
    } else {
        if (isTRUE(verbose)) {
            message("Did not update your manuals. ",
                    "Set `roxygenise` to TRUE to do so.")
        }
    }

    pkg <- packager::as.package(path)
    if (is.null(output_directory)) {
        output_directory <- file.path(pkg[["path"]],
                                      paste0(pkg[["package"]], ".Rcheck"))
    }
    name <- paste0(pkg[["package"]], "_", pkg[["version"]], ".pdf",
                   collapse = " ")
    tryCatch(msg <- callr::rcmd("Rd2pdf",
                                cmdargs = c("--force",
                                            paste0("--output=",
                                                   output_directory,
                                                   "/", name),
                                            pkg[["path"]]),
                                fail_on_status = TRUE,
                                stderr = "2>&1", spinner = FALSE),
             error = function(e) {
                 cat(e[["stdout"]])
                 stop("Failed to build manual", call. = FALSE)
             })
    if (isTRUE(verbose)) cat(msg[["stdout"]])
    invisible(msg)
}
