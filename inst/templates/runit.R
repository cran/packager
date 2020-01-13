#!/usr/bin/Rscript --vanilla
is_failure <- function(result) {
    res <- RUnit::getErrors(result)
    names(res) <- tolower(names(res)) # soothe lintr
    sum_of_exceptions <- res[["nerr"]] + res[["nfail"]]
    fail <- as.logical(sum_of_exceptions)
    return(fail)
}

if (interactive()) {
    pkgload::load_all(path = ".") # needed to use pkgload's shim version of
    # base's system.file
    unit_dir <- system.file("inst", "runit_tests", package = "{{{ package }}}")
} else {
    require("{{{ package }}}", quietly = TRUE, character.only = TRUE) ||
        pkgload::load_all(path = ".") # needed to use pkgload's shim version of
    r_call <- commandArgs(trailingOnly = FALSE)
    if (any(grepl("--file", r_call))) {
        unit_dir <- file.path("inst", "runit_tests")
    } else {
        unit_dir <- system.file("runit_tests", package = "{{{ package }}}")
    }
}
if (! dir.exists(unit_dir)) {
    stop("Can not find RUnit test directory ", unit_dir,
         ". Try to (re)install the package first.")
}
setup_file <- file.path(unit_dir, "setup.R")
if (file.exists(setup_file)) source(setup_file)
package_suite <- RUnit::defineTestSuite("{{{ package }}}_unit_test",
                                        dirs = unit_dir,
                                        testFileRegexp = "^runit.*\\.[rR]",
                                        testFuncRegexp = "^test_+")
test_result <- RUnit::runTestSuite(package_suite)
RUnit::printTextProtocol(test_result, showDetails = TRUE, fileName = "")
if (is_failure(test_result)) {
    RUnit::printTextProtocol(test_result, showDetails = TRUE)
    stop("RUnit failed.")
}
