write_info <- function(prefix = "=== packager info:") {
    obj <- utils::sessionInfo()
    invisible(utils::capture.output(info <- deparse(dput(obj))))
    cat(paste0(prefix, info), sep = "\n")
    return(invisible(NULL))
}

write_rcmdcheck <- function(prefix = "=== packager rcmdcheck:", path = ".", 
                            args = "--as-cran", 
                            build_args = args[-which(args == "--as-cran")]) {
    obj <- rcmdcheck::rcmdcheck(path = path, args = args, 
                                build_args = build_args)
    invisible(utils::capture.output(info <- deparse(dput(obj))))
    cat(paste0(prefix, info), sep = "\n")
    return(invisible(obj))
}

#' Run \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}} and Write to Log
#'
#' The \code{\link{deparse}}d  \code{\link{dput}}s of \code{\link{Sys.info}} and
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}} are tagged and
#' \code{\link{cat}}ed so we can evaluate them from reading logs (on
#' \verb{gitlab}, for example).
#' @template package_path 
#' @param args Arguments passed to
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}}.
#' @param build_args Arguments passed to
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}}.
#' @export
#' @keywords internal
#' @return \code{\link[=invisible]{Invisibly}  \link{NULL}}.
rcmdcheck_and_log <- function(path = ".", args = "--as-cran", 
                              build_args = args[-which(args == "--as-cran")]) {
    write_info()
    check <- write_rcmdcheck(path = path, args = args, build_args = build_args)
    if (! identical(check$errors, character(0))) {
        throw(paste0("rcmdcheck::rcmdcheck(",  shQuote(path), ") failed."))
    }

    return(invisible(NULL))
}

#' \code{\link[=grep]{Grep}} Lines From a File
#'
#' @param file The path to the file or a character vector holding the lines.
#' @param pattern The pattern to \code{\link[=grep]{grep}} for.
#' @param strip  \code{\link[=sub]{Substitute}} the pattern with the empty
#' string before returning the lines \code{\link[=grep]{grepped}}?
#' @export
#' @keywords internal
#' @return A character vector giving the lines.
grep_log <- function(file, pattern, strip = TRUE) {
    if (is.null(file)) {
        result <- NULL
    } else {
        if (length(file) == 1 && file.exists(file)) {
            lines <- readLines(file)
        } else {
            if (is.character(file)) {
                lines <- file
            } else {
                throw(paste0("File given was neither an existing file ",
                             "nor a character vector."))
            }
        }
        matching_lines <- grep(pattern, lines, value = TRUE)
        if (isTRUE(strip)) {
            stripped_lines <- sub(pattern, "", matching_lines)
            result <- stripped_lines
        } else {
            result <- matching_lines
        }
    }
    return(result)
}

#' Evaluate a File's Tagged Lines
#'
#' Just a wrapper for evaluating the tagged code obtained via
#' \code{\link{grep_log}}.
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}} are
#' \code{\link{cat}}ed so we can evaluate them from reading logs (on
#' \verb{gitlab}, for
#' example)
#' @param ... Arguments passed to \code{\link{grep_log}}.
#' @export
#' @keywords internal
#' @return The object obtained by evaluating the file.
#' @examples
#' \dontrun{
#' # We need "." to be a package directory, and it takes quite some time.
#' sink_file <- tempfile()
#' sink(sink_file)
#' rcmdcheck_and_log(".")
#' sink()
#' rcmdcheck <- eval_from_log(sink_file, pattern = "=== packager rcmdcheck:")
#' }
eval_from_log <- function(...) {
    log <- grep_log(..., strip = TRUE)
    res <- tryCatch(eval(parse(text = log)), error = identity)
    if (inherits(res, "error")) {
    # TODO: There might be things like 'cleaner = <environment>' in the log,
    # which cannot be evaluated!
    log <- gsub("<.*[^>]>", "''", log)
    res <- tryCatch(eval(parse(text = log)), error = identity)
    }
    if (inherits(res, "error")) {
    # TODO: I don't know where these came from...:
    log <- gsub("\\r", "", log)
    res <- tryCatch(eval(parse(text = log)), error = identity)
    }
    return(res)
}
