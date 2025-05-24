#' Customize \code{\link[lintr:lint_package]{lintr::lint_package}}
#'
#' \pkg{lintr} now runs \pkg{cyclocomp}, which we use independently and we don't
#' want to run it twice. So this is just a wrapper to
#' \code{\link[lintr:lint_package]{lintr::lint_package}} where we hardcode the
#' exclusion of unwanted linters (more may be added to \pkg{lintr}) so other
#' packages using \pkg{packager}'s \file{Makefile} or
#' \code{\link{get_package_makelist}} don't have to care of changes to the
#' default linters in \pkg{lintr}.
#' @param path The path to the package, passed to
#' \code{\link[lintr:lint_package]{lintr::lint_package}}.
#' @return See the return value of
#' \code{\link[lintr:lint_package]{lintr::lint_package}}.
#' @export
lint_package <- function(path) {
    # We use cyclocomp anyways, plus it takes time..
    llwd <- lintr::linters_with_defaults
    linters <- llwd(cyclocomp_linter = NULL,
                    lintr::indentation_linter(indent = 4L,
                                              hanging_indent_style = "always")
                    )
    lints <- lintr::lint_package(path = path, linters = linters)
    return(lints)
}
