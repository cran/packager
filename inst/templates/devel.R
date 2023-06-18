if (FALSE) {
    packager::set_package_info(path = ".", title = "YOUR TITLE HERE",
                     description = paste("Multiline",
                                         "description here."),
                     details = paste("More",
                                     "details here."),
                     force = packager:::is_force()
                     )
}
pkgload::load_all(".")
build_manual()
fritools::view(fritools::file_modified_last(path = ".", pattern = "\\.pdf$",
                                            recursive = TRUE), "evince")

fritools::find_missing_see_also(".")
fritools::find_missing_family(".")
fritools::delete_trailing_whitespace(path = "R", pattern = "\\.[rR]$")
fritools::delete_trailing_whitespace(path = file.path("inst", "runit_tests"),
                                     pattern = "\\.[rR]$")
fritools::delete_trailing_whitespace(path = "inst", recursive = TRUE,
                                     pattern = "\\.[rR]$")
