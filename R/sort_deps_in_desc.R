#' Sort Fields `Import`, `Depends` and `Suggests`  \file{DESCRIPTION}
#'
#' @param file see
#' \code{\link[desc:desc_get_deps]{desc::desc_get_deps}}).
#' @return \code{\link{NULL}}, but called for its side effect.
#' @export
#' @keywords internal
#' @examples
#' path <- file.path(tempdir(), "myFirstPackage")
#' usethis::create_package(path = path, rstudio = FALSE, open = FALSE)
#' withr::with_dir(path, usethis::use_package("withr"))
#' withr::with_dir(path, usethis::use_package("cleanr"))
#' desc::desc_get_deps(file.path(path, "DESCRIPTION"))
#' packager::sort_deps_in_desc(file.path(path, "DESCRIPTION"))
#' usethis::proj_set(NULL)
#' desc::desc_get_deps(file.path(path, "DESCRIPTION"))
#' unlink(path, recursive = TRUE)
sort_deps_in_desc <- function(file) {
    deps <- desc::desc_get_deps(file)
    d  <- by(deps, deps[["type"]], function(x) x[order(x[["package"]]), ])
    d <-  do.call("rbind", d)
    row.names(d) <- NULL
    desc::desc_del_deps(file)
    desc::desc_set_deps(d, file = file, normalize = TRUE)
    return(NULL)
}
