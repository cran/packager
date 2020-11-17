if (interactive()) pkgload::load_all(".")
test_submit <- function() {
    if (!identical(environment(), .GlobalEnv)) {
        old_opts <- options(warn = 1)
        on.exit(old_opts)
        on.exit(unlink(d, recursive = TRUE))
    }
    d <- file.path(tempdir(), "prutp")
    if (interactive()) unlink(d, recursive = TRUE)
    if (require("roxygen2")) { 
        # https://cran.r-project.org/doc/manuals/R-exts.html, 
        # cran pretest on windows seems not to have roxygen2, albeit being in
        # 'Suggests'.
        packager::create(path = d, fakemake = "roxygen2") 
        packager::use_dev_version(d)
        RUnit::checkException(packager::submit(d, stop_on_git = FALSE, stop_on_devel = TRUE))
    }
}
if (interactive()) test_submit() 
