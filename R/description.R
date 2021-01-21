#' Use the BSD-2-Clause License
#'
#' It's my favorite and \pkg{devtools} provides
#' \code{\link{use_mit_license}} only.
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
    pkg <- as.package(path)
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
#' When writing packages, I often forget to add the appropriate \verb{github}
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
#' \code{\link[gert:git_signature_default]{gert::git_signature_default}}
#' since there's no way to make sure
#' the configured git user name, locally or globally, is a \verb{github}
#' username.
#'
#' @template package_path
#' @param default_gh_user See details.
#' @param normalize Passed to
#' \code{\link[desc:desc_set_urls]{desc::desc_set_urls}}.
#' @return \code{\link[=invisible]{Invisibly} \link[=logical]{TRUE}} if
#' adding a \verb{github} \acronym{URL}, \code{\link[=logical]{FALSE}}
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
    gh_username <- tryCatch(fritools::call_safe(whoami::gh_username,
                                                dependency = "whoami",
                                                args = list(fallback =
                                                            default_gh_user),
                                                fallback = default_gh_user),
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

#' Set a \file{DESCRIPTION} File's \acronym{URL} Field
#'
#' I frequently forget to add an \acronym{URL} to my packages'
#' \file{DESCRIPTION} files,
#' and when I do not, I often forget to check that the \acronym{URL} is valid,
#' respectively the one I want. \cr
#' So this is a wrapper to functions from \pkg{desc} and \pkg{gert} and
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
#' @return \code{\link[=invisible]{Invisibly} \link[=logical]{TRUE}}
#' @keywords internal
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' unlink(path, recursive = TRUE)
#' usethis::create_package(path, open = FALSE)
#' gert::git_init(path)
#' gert::git_add(repo = path, files = ".")
#' signature <- gert::git_signature(name = "Foobar", email = "no@where.com")
#' gert::git_commit(repo = path, message = "Initial commit",
#'                  author = signature, committer = signature)
#' url <- provide_gitlab_url(path = path)
#' set_desc_url(url, path = path)
#' grep("URL:", readLines(file.path(path, "DESCRIPTION")), value = TRUE)
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
        gert::git_add(repo = path, files = "DESCRIPTION")
        # File DESCRIPTION may not have changed, so try():
        invisible(tryCatch(git_commit(path, "Update URL in DESCRIPTION"),
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
