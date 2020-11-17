#' Check to Not Commit to a Tagged Version
#'
#' I sometimes forget to bump the package version of a tagged state in git, 
#' then commiting git to that version. Nothing servere, but should be avoided.
#' So we add a check (to) a pre-commit hook for git.
#'
#' \code{\link[git2r:commit]{git2r::commit}} somehow ignores hooks,
#'  so this works only for proper git.
#' @template package_path
#' @template return_invisibly_null
#' @export 
#' @keywords internal
#' @family infrastructure functions
use_git_check_version_not_tagged <- function(path) {
    script <- system.file("templates", "check_version_not_tagged.sh",
                          package = "packager")
    use_git_pre_commit_script(path, script)
}


#' Use a Script as git pre-commit hook
#'
#' Provide a pre-commit.d/ in ./git/hooks, a pre-commit hook that executes
#' scripts from that directory and copy the \emph{script_file} there.
#'
#' \code{\link[git2r:commit]{git2r::commit}} somehow ignores hooks,
#'  so this works only for proper git.
#' @template package_path
#' @param script_file A path to a script file.
#' @export 
#' @keywords internal
#' @family infrastructure functions
use_git_pre_commit_script <- function(path, script_file) {
    root <- as.package(path)[["path"]]
    if (!uses_git(root)) {
        throw(paste0(path, " is not within a git repository!"))
    } else {
        use_git_pre_commit_hook(root)
        dir <- use_git_pre_commit_directory(root)
        file_path <- file.path(dir, basename(script_file))
        file.copy(from = script_file, to = file_path)
        Sys.chmod(file_path, mode = "0744")
        return(invisible(file_path))
    }
}

use_git_pre_commit_directory <- function(git_root) {
    dir <- file.path(git_root, ".git", "hooks", "pre-commit.d")
    if (!dir.exists(dir)) dir.create(dir, mode = "0755")
    return(invisible(dir))
}

use_git_pre_commit_hook <- function(git_root) {
    lines <- c("#!/bin/sh",
               "for script in $(find $(dirname $0)/pre-commit.d/ -type f)", 
               "do", 
               "  ${script}", 
               "  RESULT=$?",
               "  if [ $RESULT != 0 ]; then",
               "      echo \"pre-commit.d/$script returned non-zero: $RESULT, abort commit\"",
               "      exit $RESULT",
               "  fi",
               "done",
               "exit 0"
               )
    file_path <- file.path(git_root, ".git", "hooks", "pre-commit")
    # Just overwrite, checking for content and stuff takes longer...:
    writeLines(text = lines, con = file_path)
    Sys.chmod(file_path, mode = "0744")
    return(invisible(NULL))
}

sanitize_usethis_git_hook <- function(path) {
    res <- FALSE
    # usethis clutters pre-commit with an exit 0... this has to move
    root <- as.package(path)[["path"]]
    file_path <- file.path(root, ".git", "hooks", "pre-commit")
    if (file.exists(file_path)) {
        use_git_pre_commit_directory(root)
        res <- file.rename(file_path, file.path(root, ".git", "hooks", "pre-commit.d", 
                                         "99_usethis_readme_rmd.sh"))
    }
    return(invisible(res))
}
