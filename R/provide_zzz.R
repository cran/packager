provide_zzz <- function(path, force = is_force(), ...) {
    use_dependency("fritools", "Imports", path = path, min_version = "1.3.0")
    pkg <- as.package(path)
    file <- "zzz.R"
    file_path <- file.path("R", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = TRUE, pkg = pkg[["path"]], force = force, ...)
}
