## #' Check on \code{rhub}
## #'
## #' Create rhub logs that will be queried using
## #' \code{get_local_rhub}, a helper function for
## #' \code{\link{provide_cran_comments}}.
## #' @template package_path
## #' @param os Character string specifying the operation systems to test on, stick
## #' with the default.
## #' @return A list of rhub package checks (which is incomplete if not called
## #' interactively. But called for the side effect of starting the rhub processes.
## #' @family maintenance functions
## #' @keywords internal
## #' @export
## #' @examples
## #' \dontrun{
## #' res <- check_rhub(".")
## #' str(res)
## #' cat(capture.output(print(res)), file = "log/rhub.log", sep = "\n")
## #' get_local_rhub(".")
## #' }
check_rhub <- function(path = ".", os = c("solaris", "windows")) {
    root <- rprojroot::find_root(path = path, rprojroot::is_r_package)
    platforms <- as.data.frame(rhub::platforms())
    res <- list()
    if ("solaris" %in% os) {
        index <- platforms[["os-type"]] == "Solaris"
        if (sum(index) < 1) throw("Can get solaris for rhub")
        if (sum(index) > 1) {
            jndex <- grepl("Developer Studio", platforms[["compilers"]],
                           ignore.case = TRUE)
            index <- index & jndex
        }
        if (sum(index) > 1) index <- index[1]
        platform <- platforms[index, "name"]
        check <- rhub::check_for_cran(path = root, platform = platform,
                                      show_status = TRUE)
        res[["solaris"]] <- check
    }
    if ("windows" %in% os) {
        index <- platforms[["os-type"]] == "Windows"
        if (sum(index) < 1) throw("Can get windows for rhub")
        if (sum(index) > 1) {
            jndex <- platforms[["rversion"]] == "r-devel"
            index <- index & jndex
        }
        if (sum(index) > 1) index <- index[1]
        platform <- platforms[index, "name"]
        check <- rhub::check_for_cran(path = root, platform = platform,
                                      show_status = TRUE)
        res[["windows"]] <- check
    }
    if (!interactive()) message("The return value is meaningless. Use")
    return(invisible(res))
}

get_rhub_latest <- function(path = ".") {
    rhub <- rhub::list_package_checks(package = path)
    platform_names <- unique(rhub$platform_name)
    checks <- list()
    for (platform in platform_names) {
        latest_id <- rhub[rhub[["platform_name"]] == platform, ][1, ][["id"]]
        check <- rhub::get_check(latest_id)
        checks[[platform]] <- check
    } 
    return(checks)
}
