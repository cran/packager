#' Check Usage with \pkg{codetools}' \code{\link{checkUsagePackage}}
#'
#' This is just a convenience wrapper to \code{\link{checkUsagePackage}} (which
#' needs loading of the [development version of the] package).
#' @template package_path
#' @return A character vector of issues found by
#' \code{\link{checkUsagePackage}}.
#' @family maintenance functions
#' @export
check_usage <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    pkgload::load_all(root)
    package_name <- as.character(desc::desc_get("Package", path))
    issues <- utils::capture.output(codetools::checkUsagePackage(package_name,
                                                                 all = TRUE))
    return(issues)
}

#' Check \verb{Cyclomatic Complexity}
#'
#' Run
#' \code{\link[cyclocomp:cyclocomp_package_dir]{cyclocomp_package_dir}} on the
#' package throwing an error when the maximum complexity is exceeded.
#' @template package_path
#' @param max_complexity The maximum \verb{cyclomatic complexity}
#'        (which must not be exceeded).
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' maximum \verb{cyclomatic complexity} is not exceeded, throws an error
#' otherwise.
#' @family maintenance functions
#' @export
#' @examples
#' \dontrun{
#' # download and untar sources of some archived package
#' package  <- "excerptr"
#' root <- paste0("http://cran.r-project.org/src/contrib/Archive/", package)
#' version <- "1.0.0"
#' tarball <- paste0(paste(package, version, sep = "_"), ".tar.gz")
#' remote_tarball <- paste(root, tarball, sep = "/")
#' local_tarball <- file.path(tempdir(), tarball)
#' utils::download.file(remote_tarball, local_tarball)
#' utils::untar(local_tarball, exdir = tempdir())
#' res <- tryCatch(check_cyclomatic_complexity(path = file.path(tempdir(),
#'                                                              package)),
#'                 error = identity)
#' print(res)
#' }
check_cyclomatic_complexity <- function(path = ".", max_complexity = 10) {
    cyclocomp <- cyclocomp::cyclocomp_package_dir(path)
    too_complex <- cyclocomp[["cyclocomp"]] > max_complexity
    if (any(too_complex)) {
        hits <- cyclocomp[too_complex, "name"]
        diff <- cyclocomp[too_complex, "cyclocomp"] - max_complexity
        msg <- paste0("Exceeding maximum cyclomatic complexity of ",
                     max_complexity, " for ", hits, " by ", diff, ".")
        throw(paste(msg, collapse = "\n"))
    }
    return(invisible(TRUE))
}

#' Check for \file{NEWS.md} Being Up to Date
#'
#' Compare your \file{NEWS.md} file to the 'Version' entry in DESCRIPTION.
#' @template package_path
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' \file{NEWS.md} matches DESCRIPTION, throws an error otherwise.
#' @family maintenance functions
#' @export
check_news <- function(path = ".") {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    description <- readLines(file.path(root, "DESCRIPTION"))
    version <- grep("^Version: ", description, value = TRUE)
    version_number <- trimws(strsplit(version, split = ":")[[1]][2])
    package <- grep("^Package: ", description, value = TRUE)
    package_name <- trimws(strsplit(package, split = ":")[[1]][2])
    news.md <- readLines(file.path(root, "NEWS.md"))
    devel_versions <- grep("[0-9]+\\.[0-9]+\\.[0-9]+\\.9000", news.md,
                           value = TRUE)
    if (length(devel_versions) > 0) {
        devel_numbers <- sapply(devel_versions,
                                function(x) strsplit(x, split = " ")[[1]][3])
        extra_devels <- setdiff(devel_numbers, version_number)
        if (length(extra_devels) > 0) {
            throw(paste("\nFound unmatched devel version: ", extra_devels))
        }
    }
    is_covered <- any(grepl(paste("^#", package_name, version_number), news.md))
    if (! is_covered) {
        throw(paste0("Version ", version_number, " not covered!"))
    } else {
        return(invisible(TRUE))
    }
}

#' Check for Code Tags
#'
#' You do use code tags
#' (see \href{https://www.python.org/dev/peps/pep-0350/}{PEP 350} for example)?
#' This function searches for files under a directory containing such tags.
#' @param path to a directory, typically a package root.
#' @param exclude_pattern A pattern for exclusions based on the file names.
#' Stronger than \code{include_pattern}.
#' @param include_pattern A pattern for inclusions based on the file names.
#' @param pattern The pattern to search for.
#' @return A character vector of hits.
#' @family maintenance functions
#' @export
#' @examples
#' dir <- system.file("templates", package = "packager")
#' check_codetags(dir)
check_codetags <- function(path = ".", exclude_pattern = "\\.Rcheck/",
                           include_pattern = "\\.[Rr]$|\\.[Rr]md$",
                           pattern =  "XXX:|FIXME:|TODO:") {
    hits <- grep_directory(path = path, exclude_pattern = exclude_pattern,
                           include_pattern = include_pattern,
                           pattern =  pattern)
    return(hits)
}




#' Check a Package Archive
#'
#' This is a wrapper to
#' \code{\link[callr:rcmd]{callr::rcmd_safe}("check")},
#' similar to, but leaner than
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}}. While
#' the latter parses the output of \code{rcmd_safe} and uses
#' \pkg{clisymbols} in the callback, we here just return bare output and use
#' \code{\link{writeLines}} as callback. This should result in a screen display
#' that is identical to the output of \command{R CMD check}.
#'
#' @param path Path to the package archive.
#' @param cmdargs Command line arguments (see
#' \code{\link[callr:rcmd]{callr::rcmd}}).
#' @return A list with standard output, standard error
#' and exit status of the check.
#' (see \code{\link[callr:rcmd]{callr::rcmd}}).
#' @family maintenance functions
#' @export
#' @examples
#' \dontrun{
#' package_path <- file.path(tempdir(), "fakepack")
#' usethis::create_package(path = package_path)
#' file.copy(system.file("templates", "throw.R", package = "fakemake"),
#'           file.path(package_path, "R"))
#' roxygen2::roxygenize(package_path)
#' print(tarball <- get_pkg_archive_path(package_path))
#' pkgbuild::build(pkg = package_path, path = package_path)
#' print(check_archive(tarball))
#' }
check_archive <- function(path, cmdargs = NULL) {
    # heavily borrowing from rcmdcheck::rcmdcheck()
    withr::with_dir(dirname(path),
                    out <- callr::rcmd_safe("check",
                                            cmdargs = c(basename(path),
                                                        cmdargs),
                                            libpath = .libPaths(),
                                            callback =  writeLines))
    invisible(out)
}


#' @note \code{check_archive_as_cran} is a convenience Wrapper to
#' \code{check_archive}.
#'
#' @family maintenance functions
#' @export
#' @rdname check_archive
check_archive_as_cran <- function(path) {
    return(check_archive(path, cmdargs = "--as-cran"))
}



#' Retrieve Check Status From a Log File
#'
#' Searches a typical \command{R CMD check} like \file{xxx.Rcheck/00check.log}
#' for Errors, Warnings and Notes.
#'
#' @family maintenance functions
#' @keywords internal
#' @param path Path to the log file.
#' @return A list :\describe{
#'     \item{status}{ A list of 
#'     \describe{
#'         \item{notes}{The number of \code{NOTE}s}
#'         \item{warnings}{The number of \code{WARNING}s}
#'         \item{error}{The number of \code{ERROR}s}
#'     }}
#'     \item{log}{A list of 
#'     \describe{
#'         \item{notes}{The log entries for \code{NOTE}s}
#'         \item{warnings}{The log entries for \code{WARNING}s}
#'         \item{error}{The log entries for \code{ERROR}s}
#'     }}
#' }
#' @export
get_check_status <- function(path) {
    log <- readLines(path)
     notdone <- function(x) grep("^Status", x, invert = TRUE, value = TRUE)
     notes <- notdone(grep("NOTE$", log, value = TRUE))
     warnings <- notdone(grep("WARNING$", log, value = TRUE))
     errors <- notdone(grep("ERROR$", log, value = TRUE))
     status <- list(notes = length(notes),
                    warnings = length(warnings),
                    errors = length(errors))
     log_entries <- list(notes = notes, warnings = warnings, errors = errors)
     return(list(status = status, log = log_entries))
}

