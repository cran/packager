if (interactive()) pkgload::load_all(".")

test_convert_vigentte <- function() {
    path <- tempfile()
    dir.create(path)
    file.copy(system.file("files", "An_Introduction_to_packager.Rmd", package = "packager"),
              path)
    rmd_file <- file.path(path, "An_Introduction_to_packager.Rmd")
    result <- readLines(packager:::rmd2rasciidoc(rmd_file))
    expectation <- readLines(system.file("files", "An_Introduction_to_packager.Rasciidoc", package = "packager"))
    RUnit::checkIdentical(expectation, result)
    RUnit::checkTrue(file.exists(rmd_file))
    result <- readLines(packager:::convert_vignette(rmd_file))
    RUnit::checkIdentical(expectation, result)
    RUnit::checkTrue(!file.exists(rmd_file))
}
