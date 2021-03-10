if (interactive()) pkgload::load_all(".")
test_create_rmd <- function() {
    if (!identical(environment(), .GlobalEnv)) {
        old_opts <- options(warn = 1)
        on.exit(old_opts)
    }
    d <- file.path(tempdir(), "prutp")
    on.exit(unlink(d, recursive = TRUE))
    unlink(d, recursive = TRUE)
    # use the classic Rmd-vignette
    if (!require("roxygen2")) { 
        # https://cran.r-project.org/doc/manuals/R-exts.html, 
        # cran pretest on windows seems not to have roxygen2, albeit being in
        # 'Suggests'.
        packager::create(path = d, fakemake = FALSE, 
                         use_rasciidoc_vignette = FALSE)
        expected <- c("DESCRIPTION", "devel.R",
                      "inst/runit_tests/runit-throw.R", "LICENSE", 
                      "Makefile",
                      "NEWS.md", "R/prutp-package.R", "R/throw.R",
                      "README.Rmd",
                      "tests/runit.R", "tests/testthat.R",
                      "tests/testthat/test-throw.R",
                      "vignettes/An_Introduction_to_prutp.Rmd")
    } else {
        packager::create(path = d, fakemake = "roxygen2", 
                         use_rasciidoc_vignette = FALSE)
        expected <- c("DESCRIPTION", "devel.R",
                      "inst/runit_tests/runit-throw.R", "LICENSE", 
                      "Makefile",
                      "man/prutp-package.Rd", "man/throw.Rd", "NAMESPACE",
                      "NEWS.md", "R/prutp-package.R", "R/throw.R", 
                      "README.Rmd",
                      "tests/runit.R", "tests/testthat.R",
                      "tests/testthat/test-throw.R",
                      "vignettes/An_Introduction_to_prutp.Rmd")
    }
    files <- sort(dir(d, recursive = TRUE, full.names = FALSE))
    # checking on file contents does not work as covr and RUnit give
    # different digest::sha1()-values.
    result <- files
    if (interactive()) {
        expected[!(expected %in% result)]
        dir(file.path(d, "log"))
        readLines(file.path(d, "DESCRIPTION"))
        readLines(file.path(d, "NAMESPACE"))
        readLines(file.path(d, "log", "roxygen2.Rout"))
        ml <- get_package_makelist(is_cran = TRUE)
        withr::with_dir(d, print(fakemake::make(name = "roxygen2",
                                                   make_list = ml, 
                                                   verbose = FALSE)))
    }
    RUnit::checkTrue(all(expected %in% result))
    # FIXME: I cannot test on fakemake = "check" as this fails with strange
    # conditions. So I ran it 
    # in create()'s examples, now it is located in test/testthat/test-main.R
}
if (interactive()) test_create_rmd() 
