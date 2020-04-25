get_git_upstream <- function(path) {
    r <- git2r::repository(path, discover = TRUE)
    up <- git2r::branch_get_upstream(git2r::repository_head(r))
    return(up)
}

isFALSE <- function(x) {
    # I still use R 3.3.3 for testing, isFALSE() was defined in R 3.5.0
    if (exists('isFALSE', where='package:base', mode='function')) {
        base::isFALSE(x)
    } else {
        is.logical(x) && length(x) == 1L && !is.na(x) && !x
    }
}

strip_off_attributes <- function(x) {
    attributes(x) <- NULL
    return(x)
}

is_null_or_true <- function(x) isTRUE(x) || is.null(x)
is_force <- function() return(is_null_or_true(getOption("packager")[["force"]]))

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
        name <- whoami::fullname(fallback = "Foo Bar")
        name <- unlist(strsplit(name, split = " "))
        family <- name[length(name)]
        given <- setdiff(name, family)
        email <- whoami::email_address(fallback = "foobar@nowhere.com")
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

grep_directory <- function(path, pattern, include_pattern = NULL,
                           exclude_pattern = NULL) {
    hits <- NULL
    if (is.null(include_pattern)) {
        files <- list.files(path, full.names = TRUE, recursive = TRUE)
    } else {
        files <- list.files(path, full.names = TRUE, recursive = TRUE,
                            pattern = include_pattern)
    }
    if (! is.null(exclude_pattern))
        files <- grep(exclude_pattern, files, value = TRUE, invert = TRUE)
    for (f in files) {
        l <- readLines(f)
        if (any(grepl(l, pattern = pattern, perl = TRUE))) {
            found <- paste(f, sep = ": ",
                         grep(l, pattern = pattern, perl = TRUE, value = TRUE))
            hits <- c(hits, found)
        }
    }
    return(hits)
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
    usethis::use_testthat()
    pkg <- as.package(path)

    file <- "throw.R"
    file_path <- file.path("R", file)
    use_template(file, save_as = file_path, data = pkg,
                 ignore = FALSE, pkg = pkg[["path"]], force = force, ...)

    suppressMessages(usethis::use_package("RUnit", type = "Suggests"))
    suppressMessages(usethis::use_package("devtools", type = "Suggests"))
    suppressMessages(usethis::use_package("rprojroot", type = "Suggests"))
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
    res <- tryCatch(git2r::remote_url(repo = path),
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


use_gitlab_ci <- function(path = ".",
                         force = is_force(),
                         ignore = TRUE) {
    pkg <- as.package(path)
    use_template("dot_gitlab-ci.yml", ".gitlab-ci.yml", ignore = ignore,
        force = force, pkg = pkg)
    return(invisible(NULL))
}

update_make <- function(path) {
    use_runit(path = path, force = TRUE)
    use_makefile(path = path, force = TRUE)
    provide_make(path = path, force = TRUE)
}
