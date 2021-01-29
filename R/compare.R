#%  compare practically
compare_logs <- function(path = ".", force = TRUE, tolerance = 0.5,
                         call_diff = FALSE) {
    res <- FALSE
    msg <- paste("Are you sure you have cleaned the log directory",
                 "and rerun make and make.R?")
    if (interactive() && !isTRUE(force)) {
        is_yes(msg)
    } else  {
        warning(msg)
    }
    logs <- list.files(path = file.path(path, "log"), pattern = ".*\\.log$",
                       full.names = TRUE)
    routs <- list.files(path = file.path(path, "log"), pattern = ".*\\.Rout$",
                        full.names = TRUE)

    logs <- sub("\\.log$", "", logs)
    routs <- sub("\\.Rout$", "", routs)
    comparison <- fritools::compare_vectors(logs, routs)
    if (requireNamespace("stats", quietly = TRUE)) {
        is_both <- stats::complete.cases(comparison)
    } else {
        complete_cases <- function(x) {
            !apply(apply(x, 2, is.na), 1, any)
        }
        is_both <- complete_cases(comparison)
    }

    check <- NULL
    for (i in comparison[is_both, 1]) {
        if (!exists("i", inherits = FALSE)) i  <- comparison[1, 1]
        log_file <- paste0(i, ".log")
        log <- readLines(log_file)
        rout_file <- paste0(i, ".Rout")
        rout <- readLines(rout_file)
        rout <- grep("^ *$", rout, value = TRUE, invert = TRUE)
        if (abs(length(rout) / length(log) - 1) > tolerance) {
            check <- c(check, i)
            if (interactive() && isTRUE(call_diff) &&
                fritools::is_installed("diffuse")) {
                system2("diffuse", args = c(log_file, rout_file))
            }
        }
    }


    res <- list("Better_check" = check,
                "Missings" = comparison[!is_both, TRUE])
    return(res)
}


clean_make_output <- function(x, discard, remove) {
    targets <- grep("remake", x, value = TRUE)
    no_givens <- grep("(inst|R|tests|man)/", targets, value = TRUE,
                      invert = TRUE)
    no_make <- grep("Makefile", no_givens, value = TRUE,
                     invert = TRUE)
    targets <- sub(".*'(.*)'.*", "\\1", no_make)
    clean_vignette <- sub("An_Introduction.*Rasciidoc$", "AIR", targets)
    res <- clean_vignette
    for (d in discard) {
    res <- res[!grepl(paste0("^", d, "$"), res)]
    }
    res <- sub(remove, "", res)
    return(res)
}

compare_make <- function(package_directory = ".") {
    wd <- setwd(package_directory)
    on.exit(setwd(wd))
    #%  makeLIST
    ml <- packager::get_package_makelist()
    make_list_path <- tempfile()
    on.exit(unlink(make_list_path), add = TRUE)
    fakemake::write_makefile(ml, make_list_path)
    made <- system(paste("make -Bnd -f", make_list_path, "cran-comments.md"),
                   intern = TRUE)
    targets_list <- clean_make_output(made, discard = make_list_path,
                                      remove = "\\.Rout$")
    #% makeFILE
    make_file_path <- system.file("templates", "nomakefile",
                                  package = "packager")
    made <- system(paste("make -Bnd -f", make_file_path, "cran-comments.md"),
                   intern = TRUE)
    targets_file <- clean_make_output(made, discard = make_file_path,
                                      remove = "\\.log$")
    fritools::compare_vectors(targets_list, targets_file)
}

