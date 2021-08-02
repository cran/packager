#' Provide File \command{make.R}
#'
#' @param path Where to create the file.
#' @param Rbuildignore Add the file to \file{.Rbuildignore} under the given
#' \code{path}?
#' @param ... Arguments passed to
#' \code{\link[packager:use_template]{packager::use_template}}.
#' @return \code{\link[=invisible]{Invisibly}}
#' \code{\link[=logical]{TRUE}} on success,
#' \code{\link[=invisible]{Invisibly}}
#' \code{\link[=logical]{FALSE}} otherwise.
#' @keywords internal
#' @export
provide_make <- function(path, Rbuildignore = TRUE, ...) {
    status <- use_template("make.R", pkg = path, ignore = Rbuildignore, ...)
    return(status)
}

#' Use a Template
#'
#' The \pkg{devtools}' or, later \pkg{usethis}' function hardcoded the source
#' package, it had dialogs on opened files in RStudio. Did Stuff I do not want.
#'
#' @param template The name of the package's template (a file in the template
#' directory).
#' @param save_as The path where the template should be written to.
#' @param data A named list of information used to render the new file from the
#' template, typically the output of
#' \code{\link[devtools:as.package]{as.package}}.
#' @param ignore Add the new file to \file{.Rbuildignore}?
#' @param git_commit Add the new file to git and commit?
#' @param pkg The path to the package template shall be used for.
#' @param source_package The name of the package from which the template should
#' be read.
#' @param force Overwrite an existing file?
#' @return \code{\link{TRUE}} on success, \code{\link{FALSE}} otherwise.
#' @keywords internal
#' @export
use_template <- function(template, save_as = template, data = list(),
                         ignore = FALSE, pkg = ".",
                         source_package = "packager",
                         git_commit = TRUE,
                         force = isTRUE(getOption("packager")[["force"]])) {
  status <- FALSE
  checkmate::qassert(template, "S1")
  checkmate::qassert(save_as, "S1")
  checkmate::qassert(data, "L")
  checkmate::qassert(source_package, "S1")
  checkmate::qassert(ignore, "B1")
  checkmate::qassert(git_commit, "B1")
  checkmate::qassert(force, "B1")
  pkg <- as.package(pkg)
  checkmate::assert_class(pkg, "package")
  path <- file.path(pkg$path, save_as)
  if (!file.exists(path) || isTRUE(force)) {
    template_path <- system.file("templates", template,
      package = source_package, mustWork = TRUE
    )
    template_out <- whisker::whisker.render(
      readLines(template_path),
      data
    )
    message("* Creating `", save_as, "` from template.")
    writeLines(template_out, path)
    if (isTRUE(ignore)) {
      message("* Adding `", save_as, "` to `.Rbuildignore`.")
      use_build_ignore(save_as, pkg = pkg)
    }
    if (isTRUE(git_commit)) {
        try({
        if (isTRUE(ignore)) {
            gert::git_add(repo = pkg[["path"]], ".Rbuildignore")
        }
        gert::git_add(repo = pkg[["path"]], save_as)
        git_commit(path = pkg[["path"]], paste0("Adding `", save_as, "` from template `",
                                     template, "` of package` ", source_package,
                                     "`."))
        })
    }
    status <- TRUE
  } else {
    warning("`", save_as, "` already exists.", call. = FALSE)
  }
  return(invisible(status))
}


#' Mark Lints by Name Suffix
#'
#' I often use internals from other packages and save them in files
#' named ..._internals..., ..._verbatim... or ..._modified... .
#' \cr I want these to be marked in \pkg{lintr}'s output.
#'
#' @param x A list of lints.
#' @param file_name_markers Parts of the file name which mark copied code.
#' @param sort Sort by file name suffix?
#' @param invert Invert the sorting?
#' @return The list of lints with names marked.
#' @keywords internal
#' @export
mark_lints <- function(x, sort = TRUE, invert = FALSE,
                 file_name_markers = c("_internals", "_verbatim", "_modified")
                 ) {

    mark <- "COPY:"
    nomark <- "NO COPY:"
    pattern <- paste0("(",
                      paste0("^.*", file_name_markers, ".*$",
                             collapse = "|"),
                      ")")
    set_mark <- function(x) {
        if (grepl(pattern, x$filename))
            x$mark <- mark
        else
            x$mark <- nomark
        return(x)
    }
    x <- lapply(x, function(x) {
                    set_mark(x)
                      }
    )

    if (length(x) > 0) {
        if (isTRUE(sort)) {
            marked <- sapply(x, function(x) return(x[["mark"]] == mark))
            if (isTRUE(invert))
                x <- c(x[marked], x[! marked])
            else
                x <- c(x[! marked], x[marked])

        }
        invisible(lapply(x, print_lint))
    }
    return(invisible(x))
}

#' @note \code{print_lints} is an old, stale name for \code{mark_lints}.
#' @export
#' @rdname mark_lints
print_lints <- mark_lints

#' Is a Directory an R Package Root Directory?
#'
#' Just a convenience wrapper to
#' \code{\link[rprojroot:criteria]{rprojroot::is_r_package}}.
#' @param path The path to the directory.
#' @return TRUE if the directory is an \R package root directory.
#' @keywords internal
#' @export
is_r_package <-
    rprojroot::as.root_criterion(rprojroot::is_r_package)[["testfun"]][[1]]



#' Create a Package's Archive Path From the Package's \file{DESCRIPTION}
#'
#' The archive file does not have to exist. Use
#' \code{file.exists(get_pkg_archive_path())} to test existence.
#' @template package_path
#' @param absolute Return the absolute path?
#' @return Path to the package's archive file.
#' @export
#' @examples
#' package_path <- file.path(tempdir(), "anRpackage")
#' usethis::create_package(path = package_path)
#' print(tarball <- get_pkg_archive_path(package_path))
#' file.exists(tarball)
get_pkg_archive_path <- function(path = ".", absolute = TRUE) {
    pkg <- as.package(path)
    tgz <- normalizePath(file.path(pkg$path,
                                   paste0(pkg$package, "_",
                                          pkg$version, ".tar.gz")),
                         mustWork = FALSE)
    if (! isTRUE(absolute)) tgz <- sub(paste0(getwd(), "/"), "", tgz)
    return(tgz)
}
