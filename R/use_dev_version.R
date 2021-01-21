#' Use a Development Version in DESCRIPTION and \file{NEWS.md}
#'
#' This is much like
#' \code{\link[usethis:use_version]{usethis::use_dev_version}}, but their
#' conventions keep changing.
#' @template package_path
#' @param force Set to \code{TRUE} to force version bumping with uncommitted git
#' changes.
#' @export
use_dev_version <- function(path = ".", force = FALSE) {
    if (!isTRUE(force) && is_git_uncommitted(path))
        stop("Found uncommitted changes.")
    desc::desc_bump_version(which = "dev", file = path)
    add_news(path)
    gert::git_add(repo = path, files = c("DESCRIPTION", "NEWS.md"))
    git_commit(path = path, message = "Using Developement Version")
}

#' @export
#' @rdname use_dev_version
#' @note From \code{\link[usethis:use_version]{usethis::use_dev_version}},
#' the name was \code{use_dev_version}, but \code{use_devel_version} seems
#' more natural. But it is just a link.
use_devel_version <- use_dev_version

#' Add a Development Section to \file{NEWS.md}
#'
#' @param path Path to your package's directory or the \file{NEWS.md} file.
#' @return TRUE on success, FALSE otherwise.
#' @export
#' @keywords internal
add_news <- function(path) {
    status <- FALSE
    if (file.info(path)[["isdir"]]) {
        news_file <- file.path(path, "NEWS.md")
    } else {
        news_file <- path
    }
    if (! file.exists(news_file)) {
        warning(news_file, " does not exist!")
    } else {
        news <- readLines(news_file)
        dev_paragraph <- paste("#", desc::desc_get("Package", file = path),
                               desc::desc_get_version(file = path))
        if (any(grepl(dev_paragraph, news))) {
            warning("Section `", dev_paragraph, "` already exists, skipping.")
        } else {
            news <- c(dev_paragraph, "", "* FIXME", "", news)
            writeLines(news, news_file)
        }
        status  <- TRUE
    }
    return(status)
}

