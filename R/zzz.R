.onLoad <- function(libname, pkgname) {
    probably_me <- identical(fritools::call_safe(whoami::fullname,
                                                 dependency = "whoami",
                                                 args = list(fallback = "Foo"),
                                                 fallback = "Foo"),
                             "fvafrcu")
    if (probably_me) {
        adc <- utils::person(given = "Andreas Dominik",
                             family = "Cullmann",
                             email = "fvafrcu@mailbox.org",
                             role = c("aut", "cre"))
        pop <- as.list(getOption("packager"))
        pop[["whoami"]] <- adc
        options(packager = pop)
    }
    fritools::run_r_tests_for_known_hosts()
    return(invisible(NULL))
}
