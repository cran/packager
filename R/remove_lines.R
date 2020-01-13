#' Remove Lines From a File
#'
#' Delete lines from a files programmatically, for example from
#' \file{.gitignore}.
#'
#' @param file_path Path to the file.
#' @param pattern A character vector of patterns.
#' @param overwrite Overwrite the file on disk?
#' @return Used for its side effects, returns the lines to be left in the file.
#' @keywords internal
#' @export
#' @examples
#' temp_file <- file.path(tempdir(), ".gitignore")
#' content <- c("doc", "Meta", ".RData", "*.tar.gz", "doc/*")
#' cat(content, sep = "\n")
#' writeLines(content, temp_file)
#' cat(remove_lines(temp_file,  c("^doc$", "^doc/.*$")), sep = "\n")
#' cat(readLines(temp_file), sep = "\n")
remove_lines <- function(file_path = file.path(".", ".gitignore"),
                         pattern,
                         overwrite = !isFALSE(getOption("packager")[["force"]])) {
    content  <- readLines(file_path)
    if (length(pattern) == 1L) {
        idx <- grepl(pattern, content)
    } else {
        idx <- apply(sapply(as.list(pattern), function(x) grepl(x, content)), 1, any)
    }
    content <- content[!idx]
    if (isTRUE(overwrite)) writeLines(content, con = file_path)
    return(invisible(content))
}

