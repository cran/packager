if (FALSE) {
    use_build_ignore <- function(files, path, escape = TRUE) {
        if (escape) {
            files <- escape_path(files)
        }
        union_write(file.path(path, ".Rbuildignore"), files)
    }
    escape_path <- function(x) {
        x <- gsub("\\.", "\\\\.", x)
        x <- gsub("/$", "", x)
        paste0("^", x, "$")
    }

    use_directory <- function(root = ".", path, ignore = TRUE) {
        create_directory(file.path(root, path))
        if (ignore) {
            use_build_ignore(path = root, files = path)
        }
        invisible(TRUE)
    }

    create_directory <- function(root = NULL, path) {
        if (is.null(root)) {
            path <- path
        } else {
            path <- file.path(root, path)
        }
        if (dir.exists(path)) {
            return(invisible(FALSE))
        } else {
            if (file.exists(path)) {
                throw(paste(path,
                            " exists but is not a directory."))
            } else {
                dir.create(path, recurse = TRUE)
                invisible(TRUE)
            }
        }
    }
}

use_git_hook <- function(path, hook, script) {
    stopifnot(uses_git(path))
    hook_path <- file.path(path, ".git", "hooks", hook)
    dir.create(dirname(hook_path), showWarnings = FALSE)
    writeLines(con = hook_path, text = script)
    fs::file_chmod(hook_path, "0744")
    invisible()
}

version_spec <- function(x) {
    x <- gsub("(<=|<|>=|>|==)\\s*", "", x)
    numeric_version(x)
}

use_dependency <- function(package, type, min_version = NULL, path = ".") {
    stopifnot(is.character(package))
    stopifnot(is.character(type))
    if (package != "R" && ! fritools::is_r_package_installed(package)) {
        stop(paste(package, "must be installed before you can",
                   "take a dependency on it."))
    }
    if (isTRUE(min_version)) {
        min_version <- utils::packageVersion(package)
    }
    version <- if (is.null(min_version)) {
        "*"
    } else {
        paste0(">= ", min_version)
    }
    types <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    names(types) <- tolower(types)
    type <- types[[match.arg(tolower(type), names(types))]]
    deps <- desc::desc_get_deps(path)
    existing_dep <- deps$package == package
    existing_type <- deps$type[existing_dep]
    existing_ver <- deps$version[existing_dep]
    is_linking_to <- (existing_type != "LinkingTo" & type ==
                      "LinkingTo") | (existing_type == "LinkingTo" & type !=
                  "LinkingTo")
    if (!any(existing_dep) || any(is_linking_to)) {
        message("Adding ", package, " to ", type," field in DESCRIPTION")
        desc::desc_set_dep(package, type, version = version,
                           file = path)
        return(invisible())
    }
    existing_type <- setdiff(existing_type, "LinkingTo")
    delta <- sign(match(existing_type, types) - match(type, types))
    if (delta < 0) {
        warning(paste0("Package ", package, " is already listed in\n", existing_type, " in DESCRIPTION, no change made."))
    }
    else if (delta == 0 && !is.null(min_version)) {
        upgrade <- existing_ver == "*" || numeric_version(min_version) >
        version_spec(existing_ver)
        if (upgrade) {
            message("Increasing ", package, " version to ",  version, " in DESCRIPTION")
            desc::desc_set_dep(package, type, version = version,
                               file = path)
        }
    }
    else if (delta > 0) {
        if (existing_type != "LinkingTo") {
            message("\n    Moving ", package, " from ", existing_type, " to ", type, "\n    field in DESCRIPTION\n")
            desc::desc_del_dep(package, existing_type, file = path)
            desc::desc_set_dep(package, type, version = version,
                               file = path)
        }
    }
    invisible()
}
is_package <- function(path) {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package, path = path),
                     error = function(e) return(path))
    if (inherits(root, "error")) {
        status <- FALSE
    } else {
        status <- TRUE
    }
    return(status)
}



use_testthat <- function(path) {
    stopifnot(fritools::is_r_package_installed("testthat"))
    if (utils::packageVersion("testthat") < "2.1.0") {
        throw(paste("testthat 2.1.0 or greater needed.",
                    "Please install before re-trying."))
    }
    if (is_package(path = path)) {
        use_dependency("testthat", "Suggests", path = path)
    }
    use_directory(file.path("tests", "testthat"), pkg = path)
    use_template(template = "testthat.R", pkg = path,
                 save_as = file.path("tests", "testthat.R"),
                 data = list(name =  as.package(path)[["package"]])
                 )
}
