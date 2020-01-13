#' Update Dependencies for a Package Directory
#'
#' \pkg{cyclocomp} updates the dependencies using \pkg{remotes}. Well, for some
#' strange reason it does so on every run (temporarily). So I permanently do
#' this, and \code{\link[remotes:update_packages]{remotes::update_packages}}
#' does so only for CRAN packages, and \code{remotes:::update.package_deps} is
#' internal to \pkg{remotes} only. So I need copies of internal functions from
#' \pkg{remotes}.
#'
#' @param path the package's root directory.
#' @export
#' @keywords internal
update_deps <- function(path) {
    deps <- remotes::dev_package_deps(path)
    update(deps)
}
