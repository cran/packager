if (interactive()) pkgload::load_all(".")
test_create_rmd <- function() {
    if (!identical(environment(), .GlobalEnv)) {
        old_opts <- options(warn = 1)
        on.exit(old_opts)
        on.exit(unlink(d, recursive = TRUE))
    }
    d <- file.path(tempdir(), "prutp")
    if (interactive()) unlink(d, recursive = TRUE)
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
    RUnit::checkTrue(all(expected %in% result))
    # FIXME: I cannot test on fakemake = "check" as this fails with strange
    # conditions. So I ran it 
    # in create()'s examples, now it is located in test/testthat/test-main.R

    #% check for the git pre-commit hook
    packager::git_tag(path = d, message = "Test")
    # modify something
    unlink(file.path(d, "TODO.md"))
    status <- withr::with_dir(d, system2("git", args = "commit -am'foo'"))
    RUnit::checkTrue(!isTRUE(status))
    # git2r ignores the hooks: 
    git <- git2r::commit(d, message = "foo", all = TRUE)
    RUnit::checkIdentical(class(git), "git_commit")
}
if (interactive()) test_create_rmd() 
