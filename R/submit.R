#' Release a Package to CRAN
#'
#' This is a stripped version of \pkg{devtools}'
#' \code{\link[devtools:release]{release function}}, omitting most of the
#' interactive checks.
#'
#' @param path The package's root directory.
#' @param stop_on_git Stop if git has uncommitted changes or is not synced with
#' the origin?
#' @param stop_on_devel Stop if the package has a developement version? (That is, a four part version.)
#' @param force Skip user interaction?
#' @param verbose Be verbose?
#' @return \code{\link[base:invisible]{Invisibly} \link{NULL}}
#' @export
submit <- function(path = ".", stop_on_git = TRUE, stop_on_devel = TRUE,
                   force = FALSE, verbose = TRUE) {
    if (uses_git(path) && isTRUE(stop_on_git)) {
        if (is_git_uncommitted(path = path) )
            throw("You have uncommitted changes.")
        if (is.na(get_git_upstream(path))) {
            warning("You have no upstream!")
        } else {
            if (! git_sync_status(path = path))
                throw("Your repository is not synced with it's upstream.")
        }
    }
    if (isTRUE(stop_on_devel) && is_devel_package(path)) {
        throw("This package has a developement version.")
    } else if (! isTRUE(force) && !is_yes("Ready to submit?")) {
        throw("Aborting on user request.")
    } else {
        csu <- "https://xmpalantir.wu.ac.at/cransubmit/index2.php"
        built_path <- build_cran(path, args = NULL, force = force)
        message("Submitting file: ", built_path)
        message("File size: ", format(as.object_size(file.info(built_path)$size),
                                      units = "auto"))
        upload_cran(pkg = path, built_path = built_path,
                    cran_submission_url = csu)
        if (uses_git(path)) {
            m <- paste0("- Tag commit ", get_git_commit(path = path),
                       " as ", desc::desc_get_version(),
                       ", once package is on CRAN using", "\n\t",
                       "git tag -a ", desc::desc_get_version(), " ",
                       get_git_commit(path = path), " -m 'CRAN release'"
                      )
            m <- paste0(m, "\n",
                   "  and checkout the developement version using ",
                   "packager::use_dev_version() or\n\t",
                   "make use_dev_version")
            if (isTRUE(verbose)) message(m)
            union_write(file.path(path, "TODO.md"), m)
            git_add_commit(path, "Submitted to CRAN")
        }
        return(invisible(NULL))
    }
}


#' @export
#' @rdname submit
#' @note \code{release} is just a link to \code{submit} as
#' \code{\link[devtools:release]{release}} is the original function from
#' \pkg{devtools}.
release <- submit


is_devel_version <- function(x) return(!is.na(numeric_version(x)[1,4]))

is_devel_package <- function(path) {
    version <- desc::desc_get_version(path)
    return(is_devel_version(version))
}
