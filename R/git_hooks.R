#' Check to Not Commit to a Tagged Version
#'
#' I sometimes forget to bump the package version of a tagged state in git, 
#' then commiting git to that version. Nothing servere, but should be avoided.
#' So we add a check (to) a pre-commit hook for git.
#' @template package_path
#' @template return_invisibly_null
use_git_check_version_not_tagged <- function(path) {
    root <- as.package(path)[["path"]]
    if (!uses_git(root)) {
        throw(paste0(path, " is not within a git repository!"))
    } else {
        file_path <- file.path(root, ".git", "hooks", "pre-commit")
        lines <- readLines(system.file("templates",
                                       "check_version_not_tagged.sh",
                                       package = "packager"))
        res <- union_write(path = file_path, new_lines = lines, prepend = FALSE)
        fs::file_chmod(file_path, "0744")
        return(invisible(res))
    }
}
