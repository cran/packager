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
