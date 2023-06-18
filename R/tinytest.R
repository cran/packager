use_tinytest <- function(path) {
    pkg <- as.package(path)
    tinytest::setup_tinytest(pkgdir = pkg$path)
    # re-write of tinytest.R
    testfile <- file.path(pkg$path, "tests", "tinytest.R")
    fmt <- paste0("if (requireNamespace(\"tinytest\", quietly = TRUE)) ",
                  "{\n  tinytest::test_package(\"%s\")\n}")
    test_statement <- sprintf(fmt = fmt, pkg$package)
    catf <- function(fmt, ...) cat(sprintf(fmt, ...))
    catf("Re-creating %s\n", testfile)
    write(test_statement, file = testfile)
    # create test_throw.R

    example_test <- paste0("if (interactive()) {\n",
                          "    pkgload::load_all(\".\")\n",
                          "    library(\"tinytest\")\n",
                          "}\n",
                          "expect_error(", pkg$package,
                          "::throw(\"hello, error\"))"
                          )
    ttdir <- file.path(pkg$path, "inst", "tinytest")
    if (!dir.exists(ttdir)) {
        catf("Creating %s\n", ttdir)
        dir.create(ttdir)
    }
    ttfile <- file.path(ttdir, "test_throw.R")
    if (!file.exists(ttfile) || force) {
        catf("Creating %s\n", ttfile)
        write(example_test, file = ttfile)
    }
}
