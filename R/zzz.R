.onLoad <- function(libname, pkgname) {
    probably_me <- identical(whoami::fullname(fallback = "Foo Bar"), "fvafrcu")
    if (probably_me) {
        adc <- utils::person(given = "Andreas Dominik",
                             family = "Cullmann",
                             email = "fvafrcu@mailbox.org",
                             role = c("aut", "cre"))
        pop <- as.list(getOption("packager"))
        pop[["whoami"]] <- adc
        options(packager = pop)
    }
    return(invisible(NULL))
}
