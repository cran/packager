use_tinytest <- function(path) {
    pkg <- as.package(path)
    tinytest::setup_tinytest(pkgdir = pkg$path)
    testfile <- file.path(pkg$path, "tests", "tinytest.R")
    fmt <- paste0("if (requireNamespace(\"tinytest\", quietly = TRUE)) ",
                  "{\n  tinytest::test_package(\"%s\")\n}")
    test_statement <- sprintf(fmt = fmt, pkg$package) 
    catf <- function(fmt, ...) cat(sprintf(fmt, ...))
    catf("Re-creating %s\n", testfile)
    write(test_statement, file = testfile)
}

