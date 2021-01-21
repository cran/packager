#' Git Add All Changes and Commit
#'
#' Much like \command{git commit -am"M"}, where \command{M} is the
#' \code{message}.
#' @param path The path to the repository.
#' @param message The commit message to use.
#' @param untracked Add files not tracked yet before committing?
#' @return The return value of
#' \code{\link[gert:git_commit_all]{gert::git_commit_all}}.
#' @family git wrappers
#' @export
git_add_commit <- function(path, message = "Uncommented Changes: Backing Up",
                           untracked = FALSE) {
    git_status <- gert::git_status(repo = path)
    if (isTRUE(untracked)) {
        new_files <- as.data.frame(git_status)[git_status[["status"]] == "new",
                                               "file"]
        git_add(path = path, files = new_files)
    }
    signature <- get_git_signature(path,
                                   verbose = getOption("packager")[["verbose"]])
    res <- gert::git_commit_all(message  = message, author = signature,
                                committer = signature, repo = path)
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
#' \code{\link[gert:git_tag_list]{gert::git_tag_list}}.
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
    tags <- gert::git_tag_list(repo = path)[["name"]]
    is_first_tag <- length(tags) == 0
    if (! is_first_tag) {
        old_versions <- sub("^v", "", tags)
        description_version_is_newer <-
            vapply(strip_off_attributes(old_versions),
                   function(x)
                       utils::compareVersion(x,
                                             as.character(version)) < 0,
                   logical(1)
                   )
    }
    if (is_first_tag || all(description_version_is_newer)) {
        status <- git_tag_create(path = path, version = version,
                                 message = message)

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

git_tag_create <- function(path, version, message) {
    status <- FALSE
    if (fritools::is_running_on_gitlab_com()) {
        # NOTE: For reasons unkown, on gitlab.com we get
        # # Error in libgit2::git_signature_default :
        # #    config value 'user.name' was not found
        # albeit not using git_signature_default. This error was the reason
        # I wrote get_git_signature(), but we can't apply it here.
        tryCatch({
            gert::git_tag_create(name = as.character(version),
                                 message = message,
                                 repo = path)
            status <-  utils::tail(gert::git_tag_list(repo = path), 1)
        }, error = identity)
    } else {
        gert::git_tag_create(name = as.character(version),
                             message = message,
                             repo = path)
        status <-  utils::tail(gert::git_tag_list(repo = path), 1)
    }
    return(status)
}

git_add <- function(path, files, force = FALSE, catch = TRUE) {
    if (isTRUE(catch)) {
        tryCatch(gert::git_add(files = files, repo = path, force = force),
                 error = function(e) warning("Nothing added."))
    } else {
        gert::git_add(files = files, repo = path, force = force)
    }
}

get_git_signature <- function(path, verbose = FALSE) {
    signature <- tryCatch(gert::git_signature_default(path),
                          error = identity)
    if (inherits(signature, "error")) {
        user_name <- "foobar"
        user_email <- "foobar@nowhe.re"
        if (isTRUE(verbose)) message("Could not find user and email for git. ",
                                     "Setting local git config user.name to ",
                                     user_name, " and user.email to ",
                                     user_email, ". Change as apropriate.")
        signature <- gert::git_signature(name = user_name, email = user_email)
    }
    return(signature)
}

git_commit <- function(path, message,
                       verbose = getOption("packager")[["verbose"]]) {
    signature <- get_git_signature(path, verbose = verbose)
    res <- gert::git_commit(message = message,
                             author = signature, committer = signature,
                             repo = path)
    return(res)
}

is_git_repository <- function(path = ".") {
    tc <- tryCatch(gert::git_find(path), error = identity)
    success <- !inherits(tc, "error")
    return(success)
}
is_git_clone <- is_git_repository
uses_git <- is_git_repository

is_git_uncommitted <- function(path, consider_untracked = FALSE) {
    if (!is_git_repository(path))
        throw(paste(path, "does not appear to be a git repository."))
    git_status <- as.data.frame(gert::git_status(repo = path))
    if (!isTRUE(consider_untracked))
        git_status <- git_status[git_status$status == "modified" |
                                 git_status$staged == TRUE, TRUE]
    is_uncommitted <- nrow(git_status) > 0
    return(is_uncommitted)
}

git_sync_status <- function(path = ".") {
    current <- gert::git_branch(repo = path)
    branches <- as.data.frame(gert::git_branch_list(repo = path))
    remotes <- as.data.frame(gert::git_remote_list())
    current_remotes <- paste0(remotes[["name"]], "/", current)
    remotes_date <- branches[branches[["name"]] %in% current_remotes, "updated"]
    current_date <- branches[branches[["name"]] == current, "updated"]
    is_synced <- current_date == remotes_date
    if (any(!is_synced)) {
        msg <- paste0("Current branch `", current,
                      "` is not in sync with remote ",
                      remotes[!is_synced, "name"], ". Sync first.")
        throw(paste(msg, collapse = "\n"))
    }
    return(TRUE)
}

get_git_upstream <- function(path) {
    current <- gert::git_branch(repo = path)
    branches <- as.data.frame(gert::git_branch_list(repo = path))
    remotes <- as.data.frame(gert::git_remote_list())
    current_upstream <- branches[branches[["name"]] == current, "upstream"]
    if (!is.na(current_upstream)) {
    upstream_info <- branches[branches[["ref"]] == current_upstream, c("name", "commit")]
    upstream_remote <- sub(paste0("/", current), "", upstream_info[["name"]])
    current_upstream <- paste(c(current,
                                remotes[remotes[["name"]] == upstream_remote,
                                        TRUE],
                                upstream_info[["commit"]]),
                              collapse = ": ")
    }
    return(current_upstream)
}

get_git_commit <- function(path) {
    current <- gert::git_branch(repo = path)
    branches <- as.data.frame(gert::git_branch_list(repo = path))
    commit <- branches[branches[["name"]] == current, "commit"]
    return(commit)
}
