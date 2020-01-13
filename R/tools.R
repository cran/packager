#' Use the BSD-2-Clause License
#'
#' It's my favorite and \pkg{devtools} provides
#' \code{\link[usethis:use_mit_license]{use_mit_license}} only.
#' @template package_path 
#' @return Invisibly \code{\link{NULL}}.
#' @keywords internal
#' @export
#' @examples
#' withr::with_dir(tempdir(),
#'                 {
#'                     unlink("myPackage", recursive = TRUE)
#'                     usethis::create_package("myPackage", open = FALSE)
#'                     use_bsd2clause_license("myPackage")
#'                     list.files("myPackage")
#'                     grep("^License", readLines(file.path("myPackage",
#'                                                          "DESCRIPTION")))
#'                     readLines(file.path("myPackage", "LICENSE"))
#'                 }
#' )
use_bsd2clause_license <- function (path = ".") {
    pkg <- devtools::as.package(path)
    license  <- list(License = "BSD_2_clause + file LICENSE")
    d <- desc::desc(path)
    d$set(License = license)
    d$write()
    copyright_holder <- sub(" <.*$", "", d$get_author())
    cat("YEAR: ", format(Sys.Date(), "%Y"), "\n",
        "COPYRIGHT HOLDER: ", copyright_holder,
        sep = "", file = file.path(pkg[["path"]], "LICENSE"))
    return(invisible(NULL))
}


#' Add a \verb{github} \acronym{URL} to File \file{DESCRIPTION}
#'
#" When writing packages, I often forget to add the appropriate \verb{github}
#' \acronym{URL}.
#'
#' The \acronym{URL} is constructed by the package's name as read from it's file
#' \file{DESCRIPTION}, and the username returned by
#' \code{\link[whoami:gh_username]{whoami::gh_username}}.
#' \code{\link[whoami:gh_username]{whoami::gh_username}} allows for a fallback,
#' this is given by \code{default_gh_user}.
#' You can specify \code{default_gh_user = NA}, to try to retrieve the username
#' by searching remotes on \verb{github} if the
#' package is a git repository. We do not use
#' \code{\link[git2r:config]{git2r::config}} since there's no way to make sure
#' the configured git user name, locally or globally, is a \verb{github}
#' username.
#'
#' @template package_path 
#' @param default_gh_user See details.
#' @param normalize Passed to
#' \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}} if
#' adding a \verb{github} \acronym{URL}, \code{\link[base:logical]{FALSE}}
#' otherwise.
#' @keywords internal
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' usethis::create_package(path, open = FALSE)
#' add_github_url_to_desc(path)
#' grep("^URL:", readLines(file.path(path, "DESCRIPTION")), value = TRUE)
#' unlink(path, recursive = TRUE)
add_github_url_to_desc <- function(path = ".", default_gh_user = NULL,
                                   normalize = TRUE) {
    status <- FALSE
    gh_username <- tryCatch(whoami::gh_username(fallback = default_gh_user),
                            error = function(e) return(default_gh_user))
    desc_url <- desc::desc_get_urls(path)

    package_dir <- basename(rprojroot::find_root(path = path,
                                                 rprojroot::is_r_package))
    package_name <- strip_off_attributes(desc::desc_get("Package", file = path))
    if (package_name != package_dir)
        warning("The package's name and root directory differ, ",
                "sticking with the name as retrieved from file DESCRIPTION.")
    if (! is.null(gh_username) && is.na(gh_username)) {
        git_url <- get_git_url(get_remote_url(path), type = "github")
        num_of_remotes <- length(grep(paste0(package_name, "$"), git_url))
        if (num_of_remotes == 1) {
            gh_username <- sub(paste0("^https://github.com/(.*)/", package_name,
                                      "$"),
                               "\\1", git_url)
        } else {
            warning("Found ", num_of_remotes,
                    " git remotes refering to `", package_name, "`.")
        }
    }
    if (is.null(gh_username) || is.na(gh_username)) {
        warning("Could not retrieve github user name. ",
                "Set the URL in DESCRIPTION manually!")
        manual_url <- NULL
    } else {
        manual_url <- paste("https://github.com", gh_username, package_name,
                            sep = "/")
    }
    if (! is.null(manual_url) && ! any(grepl(manual_url, desc_url))) {
        desc_url <- c(desc_url, manual_url)
        desc::desc_set_urls(desc_url, file = path, normalize = normalize)
        status <- TRUE
    }
    return(invisible(status))
}

#' Provide File \command{make.R}
#'
#' @param path Where to create the file.
#' @param Rbuildignore Add the file to \file{.Rbuildignore} under the given
#' \code{path}?
#' @param ... Arguments passed to
#' \code{\link[packager:use_template]{packager::use_template}}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:logical]{TRUE}} on success,
#' \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:logical]{FALSE}} otherwise.
#' @keywords internal
#' @export
provide_make <- function(path, Rbuildignore = TRUE, ...) {
    status <- use_template("make.R", pkg = path, ignore = Rbuildignore, ...)
    return(status)
}

#' Use a Template
#'
#' \pkg{devtools}' or, later \pkg{usethis}' function hardcoded the source
#' package, the had dialogs on opened files in RStudio. Did Stuff I do not want.
#'
#' @param template The name of the packages's template (a file in the template
#' directory). 
#' @param save_as The path where the template should be written to.
#' @param data A named list of information used to render the new file from the
#' template, typically the output of
#' \code(\link[devtools:as.package]{devtools::as.package}}.
#' @param ignore Add the new file to \file{.Rbuildignore}?
#' @param git_commit Add the new file to git and commit?
#' @param pkg The path to the package template shall be used for.
#' @param source_package The name of the package from which the template should
#' be read.
#' @param force Overwrite an existing file?
#' @return \code{\link{TRUE}} on success, \code{\link{FALSE}} otherwise.
#' @keywords internal
#' @export
use_template <- function(template, save_as = template, data = list(),
                         ignore = FALSE, pkg = ".",
                         source_package = "packager",
                         git_commit = TRUE,
                         force = isTRUE(getOption("packager")[["force"]])) {
  status <- FALSE
  pkg <- devtools::as.package(pkg)
  path <- file.path(pkg$path, save_as)
  if (!file.exists(path) || isTRUE(force)) {
    template_path <- system.file("templates", template,
      package = source_package, mustWork = TRUE
    )
    template_out <- whisker::whisker.render(
      readLines(template_path),
      data
    )
    message("* Creating `", save_as, "` from template.")
    writeLines(template_out, path)
    if (isTRUE(ignore)) {
      message("* Adding `", save_as, "` to `.Rbuildignore`.")
      use_build_ignore(save_as, pkg = pkg)
    }
    if (isTRUE(git_commit)) {
        try({
        repository <- git2r::repository(path = pkg[["path"]])
        if (isTRUE(ignore)) {
            git2r::add(repository, ".Rbuildignore")
        }
        git2r::add(repository, save_as)
        git_commit(repository, paste0("Adding `", save_as, "` from template `", 
                                     template, "` of package` ", source_package,
                                     "`."))
        })
    }
    status <- TRUE
  } else {
    warning("`", save_as, "` already exists.", call. = FALSE)
  }
  return(invisible(status))
}


#' Mark Lints by Name Suffix
#'
#' I often use internals from other packages and save them in files
#' named ..._internals..., ..._verbatim... or ..._modified... .
#' \cr I want these to be marked in \pkg{lintr}'s output.
#'
#' @param x A list of lints.
#' @param file_name_markers Parts of the file name which mark copied code.
#' @param sort Sort by file name suffix?
#' @param invert Invert the sorting?
#' @return The list of lints with names marked.
#' @export
#' @examples
#' files <- file.path(system.file("files", package = "packager"),
#'                    c("a_verbatim.R", "a.R"))
#' lints <- lintr:::flatten_lints(lapply(files, 
#'                                       function(file) {
#'                                           lintr::lint(file,
#'                                                       parse_settings = FALSE)}))
#' 
#' mark_lints(lints, invert = FALSE)
#' mark_lints(lints, invert = TRUE)
mark_lints <- function(x, sort = TRUE, invert = FALSE,
                 file_name_markers = c("_internals", "_verbatim", "_modified")
                 ) {

    mark <- "COPY:"
    nomark <- "NO COPY:"
    pattern <- paste0("(",
                      paste0("^.*", file_name_markers, ".*$",
                             collapse = "|"),
                      ")")
    set_mark <- function(x) {
        if (grepl(pattern, x$filename))
            x$mark <- mark
        else
            x$mark <- nomark
        return(x)
    }
    x <- lapply(x, function(x) {
                    set_mark(x)
                      }
    )

    if (length(x) > 0) {
        if (isTRUE(sort)) {
            marked <- sapply(x, function(x) return(x[["mark"]] == mark))
            if (isTRUE(invert))
                x <- c(x[marked], x[! marked])
            else
                x <- c(x[! marked], x[marked])

        }
        invisible(lapply(x, print_lint))
    }
    return(invisible(x))
}

#' @note \code{print_lints} is an old, stale name for \code{mark_lints}.
#' @export
#' @rdname mark_lints
print_lints <- mark_lints

#' Is a Directory an R Package Root Directory?
#'
#' Just a convenience wrapper to
#' \code{\link[rprojroot:is_r_package]{rprojroot::is_r_package}}.
#' @param path The path to the directory.
#' @return TRUE if the directory is an \R package root directory.
#' @keywords internal
#' @export
is_r_package <-
    rprojroot::as.root_criterion(rprojroot::is_r_package)[["testfun"]][[1]]

#' Provide a \verb{gitlab} \acronym{URL} for a Given Path
#'
#' @template package_path 
#' @return a character string giving a \verb{github} \acronym{URL}.
#' @keywords internal
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' unlink(path, recursive = TRUE)
#' usethis::create_package(path, open = FALSE)
#' try(provide_gitlab_url(path))
#' git2r::init(path)
#' provide_gitlab_url(path)
#' invisible(desc::desc_set(Package = "bar", file = path))
#' provide_gitlab_url(path)
provide_gitlab_url <- function(path = ".") {
    url <- get_git_url(get_remote_url(path))
    if (is.null(url)) {
        repository <- tryCatch(git2r::repository(path = path),
                               error = identity)
        # could use uses_git(path) as condition, but if TRUE,
        # I would have to call gitr::repository in the TRUE suite of the if.
        # So I do it above.
        if (inherits(repository, "error")) {
            throw(paste(path, "is not a git repository"))
        } else {
            directory <- basename(git2r::workdir(repository))
            if (is_r_package(path)) {
                package_name <- strip_off_attributes(desc::desc_get("Package",
                                                                    file = path)
                )
                if (package_name != directory) {
                    warning("The package's name and root directory differ, ",
                            "sticking with the name as retrieved from file ",
                            "DESCRIPTION.")
                    directory <- package_name
                }
            }
            git_signature <- tryCatch(git2r::default_signature(repository),
                                    error = identity)
            if (inherits(git_signature, "error")) {
                user_name <- "foobar"
                user_email <- "foobar@nowhe.re"
                git2r::config(repository,
                              user.name = user_name, user.email = user_email)
                git_signature <- git2r::default_signature(repository)
            }
            name <- getElement(git_signature, "name")
            url <- paste("https://gitlab.com", name, directory, sep = "/")
        }
    }
    return(url)
}

#' Set a \file{DESCRIPTION} File's \acronym{URL} Field
#'
#' I frequently forget to add an \acronym{URL} to my packages'
#' \file{DESCRIPTION} files,
#' and when I do not, I often forget to check that the \acronym{URL} is valid,
#' respectively the one I want. \cr
#' So this is a wrapper to functions from \pkg{desc} and \pkg{git2r} and i
#' messaging and/or adding
#' a reminder to file \code{TODO.md}.
#' @param url A character string giving the \acronym{URL} to set or add in
#' \file{DESCRIPTION}.
#' @param path Path to the \file{DESCRIPTION} file, see
#' \code{\link[desc:desc_get_urls]{desc::desc_get_urls}}.
#' @param normalize See \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @param do_commit Commit the updated \file{DESCRIPTION}?
#' @param do_remind Write a reminder into the package's \file{TODO.md}?
#' @param verbose Be verbose?
#' @param overwrite Set (overwrite) the \acronym{URL} field in
#' \file{DESCRIPTION}
#' instead
#' adding the \acronym{URL} given to the \acronym{URL} field in
#' \file{DESCRIPTION}?
#' @return \code{\link[base:invisible]{Invisibly} \link[base:logical]{TRUE}}
#' @keywords internal
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' unlink(path, recursive = TRUE)
#' usethis::create_package(path, open = FALSE)
#' repo <- git2r::init(path)
#' git2r::config(repo, user.name = "foobar", user.email = "foobar@nowhe.re")
#' git2r::add(repo = repo, path = "*")
#' git2r::commit(repo = repo, message = "Initial commit")
#' url <- provide_gitlab_url(path = path)
#' set_desc_url(url, path = path)
#' git2r::commits(repo)
#' git2r::status(repo)
#' readLines(file.path(path, "TODO.md"))
set_desc_url <- function(url, path = ".", normalize = TRUE,
                         overwrite = FALSE,
                         do_commit = is_force(),
                         do_remind = !isTRUE(getOption("packager")[["force"]]),
                         verbose = getOption("packager")[["verbose"]]
                         ) {
    status <- FALSE
    if (isTRUE(overwrite)) {
        # do nothing
    } else {
        desc_url <- desc::desc_get_urls(path)
        url <- c(desc_url, url)
    }
    desc::desc_set_urls(url, file = path, normalize = normalize)
    if (isTRUE(do_commit) && uses_git(path)) {
        repository <- git2r::repository(path = path)
        git2r::add(repository, "DESCRIPTION")
        # File DESCRIPTION may not have changed, so try():
        invisible(tryCatch(git2r::commit(repository,
                                         "Update URL in DESCRIPTION"),
                           error = identity))
    }
    m <- paste0("- make sure ", url, " exists!")
    if (isTRUE(do_remind)) {
        union_write(file.path(path, "TODO.md"), m, prepend = TRUE)
    }
    if (!isTRUE(verbose)) message(m)
    status <- TRUE
    return(invisible(status))
}

#' Create a Package's Archive Path From the Package's \file{DESCRIPTION}
#'
#' The archive file does not have to exist. Use
#' \code{file.exists(get_pkg_archive_path())} to test existence.
#' @template package_path 
#' @param absolute Return the absolute path?
#' @return Path to the package's archive file.
#' @export
#' @examples
#' package_path <- file.path(tempdir(), "anRpackage")
#' usethis::create_package(path = package_path)
#' print(tarball <- get_pkg_archive_path(package_path))
#' file.exists(tarball)
get_pkg_archive_path <- function(path = ".", absolute = TRUE) {
    pkg <- devtools::as.package(path)
    tgz <- normalizePath(file.path(pkg$path,
                                   paste0(pkg$package, "_", 
                                          pkg$version, ".tar.gz")),
                         mustWork = FALSE)
    if (! isTRUE(absolute)) tgz <- sub(paste0(getwd(), "/"), "", tgz)
    return(tgz)
}
