#' Git Add All Changes and Commit
#'
#' Much like \command{git commit -am"M"}, where \command{M} is the
#' \code{message}.
#' @param path The path to the repository.
#' @param message The commit message to use.
#' @param untracked Add files not tracked yet before committing?
#' @param ... Arguments passed to \code{\link[git2r:status]{git2r::status}}.
#' @return The return value of \code{\link[git2r:commit]{git2r::commit}}.
#' @family git wrappers
#' @export
git_add_commit <- function(path, message = "Uncommented Changes: Backing Up",
                           untracked = FALSE, ...) {
    repository <- git2r::repository(path = path)
    # TODO: I get strange results for repositories created with devtools,
    # unstaged files disappear when passing 'untracked' = TRUE to
    # git2r::status().
    git_status <- git2r::status(repository, ...)
    files <- unlist(git_status[["unstaged"]])
    if (isTRUE(untracked)) files <- c(files, unlist(git_status[["untracked"]]))
    tryCatch(git2r::add(repository, files),
             error = function(e) warning("Nothing added."))
    res <- git_commit(repository = repository, commit_message = message)
    return(res)

}

#' Create a Git Tag Based on the Current Version Number
#'
#' This is basically the same as \command{git tag -a T -m M} where T is the
#' version read from the package's DESCRIPTION file and M is given by
#' \code{message} (see below).
#' @param path Path to the package.
#' @param tag_uncommited Tag if there are uncommitted changes?
#' @param message The tag message to be used.
#' @return \code{\link{FALSE}} or the value of
#' \code{\link[git2r:tag]{git2r::tag}}.
#' @family git wrappers
#' @export
git_tag <- function(path = ".", tag_uncommited = FALSE,
                    message = "CRAN release") {
    status <- FALSE
    root <- tryCatch(rprojroot::find_root(rprojroot::is_r_package, path = path),
                     error = function(e) return(path))
    version <- desc::desc_get_version(path)
    if (! is_git_clone(root))
        warn_and_stop("Not a git repository.")
    if (is_git_uncommitted(root) && ! isTRUE(tag_uncommited))
        warn_and_stop("Uncommited changes, aborting.")
    repo <- git2r::repository(root)
    tags <- git2r::tags(repo)
    is_first_tag <- length(tags) == 0
    if (! is_first_tag) {
        old_tag_names <- names(tags)
        old_versions <- sub("^v", "", old_tag_names)
        description_version_is_newer <-
            vapply(strip_off_attributes(old_versions),
                   function(x)
                       utils::compareVersion(x,
                                             as.character(version)) < 0,
                   logical(1)
                   )
    }
    if (is_first_tag || all(description_version_is_newer)) {
        status <- git2r::tag(repo, as.character(version), message)
    } else {
        future_versions <- old_versions[! description_version_is_newer]
        warn_and_stop(paste0("File DESCRIPTION has version ", version,
                             ", but I found ",
                             strip_off_attributes(future_versions),
                             " in the git repository's tags."
                             ))

    }
    return(status)
}

git_commit <- function(repository, commit_message,
                       verbose = getOption("packager")[["verbose"]]) {
    repo_config <- tryCatch(git2r::default_signature(repository),
                            error = identity)
    if (inherits(repo_config, "error")) {
        user_name <- "foobar"
        user_email <- "foobar@nowhe.re"
        if (isTRUE(verbose)) message("Could not find user and email for git. ",
                                     "Setting local git config user.name to ",
                                     user_name, " and user.email to ",
                                     user_email, ". Change as apropriate.")
        git2r::config(repository,
                      user.name = user_name, user.email = user_email)
    }
    res <- git2r::commit(repository, commit_message)
    return(res)
}

is_git_repository <- function (path) {
    !is.null(git2r::discover_repository(path, ceiling = 0))
}

is_git_clone <- function(path = ".") {
    is_git_clone <- ! is.null(git2r::discover_repository(path, ceiling = 0))
    return(is_git_clone)
}

is_git_uncommitted <- function (path) {
    if (!is_git_repository(path))
        throw(paste(path, "does not appear to be a git repository."))
    r <- git2r::repository(path, discover = TRUE)
    st <- vapply(git2r::status(r), length, integer(1))
    any(st != 0)
}
