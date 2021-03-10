.onLoad <- function(libname, pkgname) {
    fritools::run_r_tests_for_known_hosts()
    return(invisible(NULL))
}
