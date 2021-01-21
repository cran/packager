# Helpers for interactive scripting
update_make <- function(path) {
    use_runit(path = path, force = TRUE)
    use_makefile(path = path, force = TRUE)
    provide_make(path = path, force = TRUE)
}

