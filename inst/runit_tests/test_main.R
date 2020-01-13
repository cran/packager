if (interactive()) pkgload::load_all(".")

test_create <- function() {
    old_opts <- options(warn = 1)
    on.exit(old_opts)
    d <- file.path(tempdir(), "prutp")
    on.exit(unlink(d, recursive = TRUE))
    packager::create(path = d, fakemake = "roxygen2")
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    # checking on file contents does not work as covr and RUnit give
    # different digest::sha1()-values.
    result <- files
    expected <- c("DESCRIPTION", "devel.R",
                  "inst/runit_tests/runit-throw.R", "LICENSE", "Makefile",
                  "man/prutp-package.Rd", "man/throw.Rd", "NAMESPACE",
                  "NEWS.md", "R/prutp-package.R", "R/throw.R", "README.Rmd",
                  "tests/runit.R", "tests/testthat.R",
                  "tests/testthat/test-throw.R",
                  "vignettes/An_Introduction_to_prutp.Rmd")
    RUnit::checkTrue(all(expected %in% result))
    # FIXME: I cannot test on fakemake = "check" as this fails with strange
    # conditions. So I ran it 
    # in create()'s examples, now it is located in test/testthat/test-main.R
}
if (interactive()) test_create() 
