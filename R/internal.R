get_news <- function(path = ".") {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package,
                                          path = path),
                     error = function(e) return(FALSE)
                     )
    if (root == FALSE) throw("Can't find the R package")
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    news <- readLines(file.path(root, "NEWS.md"))
    version_pattern <- paste0("^", " ", description[1, "Version"])
    news_by_version <- unlist(strsplit(paste(news,
                                             collapse = "\n"),
                                       split = paste("#",
                                                     description[1, "Package"])
                                       ))
    news <- grep(version_pattern, news_by_version, value = TRUE)
    news <- sub(paste0(version_pattern, "\n"), "", news)
    return(news)
}

create_package_help <- function(path = ".",
                                title = NULL,
                                description = NULL,
                                details = NA,
                                force = isTRUE(getOption("packager")[["force"]])
                                ) {
    if (is.null(title)) title  <- "Here Goes the Title"
    if (is.null(description)) {
        description  <- paste("A description is a paragraph consisting of one",
                              "or more sentences.")
        if (is.null(details))
            description <- paste(description, "You may add another paragraph",
                                 "for a 'Details' section.")
    }
    pkg <- as.package(path)
    if (is.na(details))
        details <- paste0("You will find the details in\\cr\n",
                         "\\code{vignette(\"An_Introduction_to_",
                         pkg[["package"]], "\", package = \"", pkg[["package"]],
                         "\")}.")
    package_roxygen_file <- file.path(pkg[["path"]], "R",
                                      paste0(pkg[["package"]], "-package.R"))
    package_roxygen_end <- c(paste0("@name ", pkg[["package"]], "-package"),
                             paste0("@aliases ", pkg[["package"]], "-package"),
                             "@docType package",
                             "@keywords package")
    content <- c(strwrap(title, prefix = "#' "), "#'")
    content <- c(content, strwrap(description, prefix = "#' "), "#'")
    content <- c(content, strwrap(details, prefix = "#' ", width = 80), "#'")
    content <- c(content, strwrap(package_roxygen_end, prefix = "#' "))
    content <- c(content, "NULL")
    if (file.exists(package_roxygen_file) && !isTRUE(force)) {
        warning(package_roxygen_file, " exists, not overwriting.")
        package_roxygen_file <- paste0(package_roxygen_file, "-packager")
    }
    writeLines(content, con = package_roxygen_file, sep = "\n")
    return(invisible(content))
}

update_description <- function(path = ".",
                               title = "A Fake Title",
                               description = "This is a fake package.",
                               author_at_r = NULL
                               ) {
    if (is.null(author_at_r)) {
        warning("Argument 'author_at_r' is missing, using default.")
        name <- fritools::call_safe(whoami::fullname, dependency = "whoami",
                                    args = list(fallback = "Foo Bar"),
                                    fallback = "Foo Bar")
        name <- unlist(strsplit(name, split = " "))
        family <- name[length(name)]
        given <- setdiff(name, family)
        email <- fritools::call_safe(whoami::email_address,
                                     dependency = "whoami",
                                     args = list(fallback = "foo@nowhere.com"),
                                     fallback = "foobar@nowhere.com")
        author_at_r <- utils::person(family = family, given = given,
                                     email = email, role = c("aut", "cre"))
    }
    d <- desc::desc(path)
    if (! is.null(title))
        d$set(Title = title)
    if (! is.null(description)) {
        description <-
            sub("(.*)\\\\pkg\\{(.*)\\}(.*)", "\\1 package \"\\2\"\\3",
                description)
        d$set(Description = description)
    }
    d$set_authors(author_at_r)
    d$write()
    desc::desc_normalize(file = path)
    return(invisible(NULL))
}

unpatch_r_version <- function(path = ".") {
    deps <- desc::desc_get_deps(path)
    Rdep <- deps[deps[["package"]] == "R", "version"]
    if (identical(Rdep, character(0))) {
        r_version <- getRversion()
    } else {
        match <- regexpr("[0-9]*\\.[0-9]*\\.[0-9]*", Rdep)
        start <- as.numeric(match)
        stop <- start + attr(match, "match.length") - 1
        r_version <- as.package_version(substring(Rdep, start, stop))
    }
    numeric_version <- unlist(r_version[[1]])
    numeric_version[3] <- 0
    if (identical(Rdep, character(0))) {
        Rdep <- paste0(">= ", paste(numeric_version, collapse = "."))
    } else {
        substr(Rdep, start, stop) <- paste(numeric_version, collapse = ".")
    }
    if (!as.logical(nrow(deps))) {
        deps <- data.frame(type = "Depends", package = "R", version = Rdep)
    } else {
        deps[deps[["package"]] == "R", "version"] <- Rdep
    }
    desc::desc_set_deps(deps, file = path, normalize = TRUE)
    return(invisible(NULL))
}


remove_Rproj <- function(path = rprojroot::find_root(rprojroot::is_r_package)) {
                         file_name <- list.files(path,
                                                 pattern = ".*\\.Rproj$",
                                                 full.names = TRUE)
                         return(unlink(file_name))
}

warn_and_stop <- function(...) {
    cat(...)
    throw(...)
}

use_runit <- function(path = ".", force = is_force(),
                      ignore = TRUE, source_package = "packager", ...) {
    file <- "runit.R"
    file_path <- file.path("tests", file)
    use_template(file, save_as = file_path, data = as.package(path),
                 pkg = as.package(path)[["path"]], force = force,
                ...)
}

provide_throw <- function(path = ".",
                          force = is_force(),
                          ...) {
    pkg <- as.package(path)
    use_testthat(path = pkg[["path"]])

    file <- "throw.R"
    file_path <- file.path("R", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    suppressMessages(use_dependency("RUnit", type = "Suggests",
                                    path = pkg[["path"]]))
    suppressMessages(use_dependency("pkgload", type = "Suggests",
                                    path = pkg[["path"]]))
    suppressMessages(use_dependency("rprojroot", type = "Suggests",
                                    path = pkg[["path"]]))
    file <- "test-throw.R"
    file_path <- file.path("tests", "testthat", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    use_runit(path = path, force = force, ...)

    relative_path <- file.path("inst", "runit_tests")
    dir.create(file.path(pkg[["path"]], relative_path),
               showWarnings = FALSE, recursive = TRUE)
    file <- "runit-throw.R"
    file_path <- file.path(relative_path, file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)


    return(NULL)
}

use_devel <- function(path = ".",
                      force = is_force(),
                      ignore = TRUE) {
    pkg <- as.package(path)
    use_template("devel.R", data = pkg, pkg = pkg, force = force,
                 ignore = ignore)
    invisible(NULL)
}

use_makefile <- function(path = ".",
                         force = is_force(),
                         ignore = TRUE) {
    pkg <- as.package(path)
    use_template("nomakefile", "Makefile", data = pkg, pkg = pkg, force = force,
                 ignore = ignore)
    invisible(NULL)
}

use_devtools <- function(path = ".") {
    result <- NULL
    result <- c(result, use_news_md(pkg = path))
    result <- c(result, use_readme_rmd(path = path))
    return(result)
}

get_remote_url <- function(path = ".", discover = TRUE) {
    res <- tryCatch(gert::git_remote_list(repo = path)[["url"]],
                     error = identity
                     )
    if (inherits(res, "error")) {
        res <- NULL
    }
    return(res)
}

get_git_url <- function(x,
                        type = c("gitlab", "github"),
                        force = is_force(),
                        return_only_one = FALSE) {
    type <- match.arg(type)
    if (length(x) == 0) {
        res <- NULL
    } else {
        index_github <- grep(paste0("^https://", type, ".com"), x)
        if (length(index_github) == 0) {
            res <- NULL
        } else {
            if (isTRUE(return_only_one) && length(index_github) > 1) {
                if (isTRUE(force)) {
                    warning("Found multiple ", type, " URL, ",
                            "using the first.")
                    res <- x[index_github[1]]
                } else {
                    throw(paste0("Found multiple ", type, " URL."))
                }
            } else {
                res <- x[index_github]
            }
        }
    }
    return(res)
}

provide_man_roxygen <- function(path, force = is_force(), ...) {
    use_directory("man-roxygen", pkg = path, ignore = TRUE)
    pkg <- as.package(path)
    files <- list.files(system.file("templates", "man-roxygen",
                                    package = "packager"))
    file_paths <- file.path("man-roxygen", files)
    for (file_path in file_paths) {
        use_template(file_path, save_as = file_path, data = pkg,
                     ignore = TRUE, pkg = pkg[["path"]], force = force, ...)
    }

}
