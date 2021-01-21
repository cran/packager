get_r_binary <- function() {
    if (rasciidoc:::is_installed("R-devel")) {
        r <- "R-devel"
    } else {
        r <- "R"
    }
    return (r)
}

sys_build <- function(..., path = ".", defaults = c("--no-build-vignettes"),
                      r_args = "--vanilla") {
    args <- unique(c(defaults, unlist(list(...))))
    system2(get_r_binary(), args = c(r_args, "CMD build", args, path))
    gct <- get_current_tarball(path)
    return(invisible(gct))
}

get_current_tarball <- function(path) {
    if (!dir.exists(path) && file.exists(path)) {
        tarball <- normalizePath(path, mustWork = TRUE)
    } else {
        d <- desc::desc(path)
        tarball <- paste0(desc::desc_get_field("Package", d), "_",
                          desc::desc_get_field("Version", d), ".tar.gz")
        tarball <- normalizePath(file.path(path, tarball), mustWork = TRUE)
    }
    return(tarball)
}


sys_check <- function(..., path = ".", defaults = c("--no-build-vignettes"),
                      r_args = "--vanilla") {
    tarball <- get_current_tarball(path)
    args <- unique(c(defaults, unlist(list(...))))
    system2(get_r_binary(), args = c(r_args, "CMD check", args, tarball))
}

sys_install <- function(..., path = ".", defaults = NULL,
                      r_args = "--vanilla") {
    tarball <- get_current_tarball(path)
    system2(get_r_binary(), args = c(r_args, "CMD INSTALL", tarball))
}
