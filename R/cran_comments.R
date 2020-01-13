info <- function(session = NULL) {
    if (is.null(session))
        session <- utils::sessionInfo()
    if (!identical(class(session), "sessionInfo"))
        warning("Argument session is not of class `sessionInfo`!")
    info <- c(session[["R.version"]][["version.string"]],
              paste0("Platform: ", session[["platform"]]),
              paste0("Running under: ", session[["running"]]))
    return(info)
}

get_gitlab_info <- function(path = ".", private_token, ...) {
    info <- NULL
    if (missing(private_token) || is.null(private_token)) {
        warning("You need a private token to access gitlab.")
    } else {
        url <- get_git_url(get_remote_url(path))
        if (!is.null(url)) {
            project <- sub("\\.git$", "", basename(url))
            user <- tolower(basename(dirname(url)))
            log <- tryCatch(get_gitlab_log(user = user, project = project,
                                           private_token, ...),
                            error = function(e) return(NULL))
            if (!is.null(log)) {
                info <- eval_from_log(file = log, 
                                      pattern = "=== packager info:")
                info <- info(info)
                rcmdcheck <- eval_from_log(log, 
                                           pattern = "=== packager rcmdcheck:")
                if (is.null(rcmdcheck)) {
                    status <- "FAILED"
                } else {
                    status <- grep("^Status",
                                   strsplit(rcmdcheck[["stdout"]],
                                            split = "\n")[[1]],
                                   value = TRUE)
                }
                info <- c(info, status)
            }
        }
    }
    return(info)
}

#' Provide a Template for Your Comments To CRAN
#'
#'  \code{\link{submit}} reads a file \file{cran-comments.md}. This
#' function provides a template based on your R version, your
#' \command{R CMD check} output and
#' the package's \file{NEWS.md}.
#' @template package_path 
#' @param initial Is this an initial submission?
#' @param check_log Path to the check log relative to \code{path}. Typically
#' file.path("log", "check.Rout").
#' @param name The name to sign with, if NA, the given name of the package
#' maintainer as stated in file DESCRIPTION is used.
#' @param write_to_file Do write the comment to \file{cran-comment.md}?
#' @param private_token Provide a private token to access
#' \url{https://gitlab.com}. 
#' @param proxy A proxy to use.
#' @note By default this function writes to disk as side effect.
#' @return Character vector containing the \acronym{CRAN} comments, which are
#' written to \file{cran-comments.md} (see Note).
#' @export
#' @examples
#' \dontrun{
#'
#' if (Sys.info()[["nodename"]] == "fvafrdebianCU") {
#'     gitlab_token <- readLines(file.path("~", ".gitlab_private_token.txt"))
#'     proxy <- httr::use_proxy("10.127.255.17", 8080)
#'     comments <- provide_cran_comments(path = ".",
#'                                       write_to_file = TRUE,
#'                                       private_token = gitlab_token,
#'                                       proxy = proxy)
#'
#' } else {
#'     gitlab_token <- readLines(file.path("~", ".gitlab_private_token.txt"))
#'     comments <- provide_cran_comments(path = ".",
#'                                       write_to_file = TRUE,
#'                                       private_token = gitlab_token)
#' }
#' cat(comments, sep = "")
#' }
provide_cran_comments <- function(check_log = NULL,
                                  path = ".",
                                  initial = FALSE,
                                  write_to_file = TRUE,
                                  private_token = NULL,
                                  name = NA, proxy = NULL) {
    if (is.na(name)) {
        name <- tryCatch({
            maintainer <- desc::desc_get_author(role = "cre", file = path)
            # Begin Exclude Linting
            # NOTE: a person object is a strange thing, we seem to unclass() it,
            # see https://stackoverflow.com/questions/9765493/
            #      how-do-i-reference-specific-tags-in-the-bibentry
            #      -class-using-the-or-conv
            # End Exclude Linting
            paste(getElement(unclass(maintainer)[[1]], "given"), collapse = " ")
        },
        error = function(e) return(name)
        )
    }
    pkg <- devtools::as.package(path)
    comments_file <- file.path(pkg[["path"]], "cran-comments.md")
    here <- info(session = NULL)
    if (! is.null(check_log)) {
        check <- parse_check_results(check_log)
        check_output <- utils::capture.output(print.check_results(check),
                                              type = "message")[2]
    } else {
        check_candidates <- list.files(file.path(path, "log"), 
                                       pattern =  "check\\.(log|Rout)$", 
                                       full.names = TRUE)
        if (!is.na(check_candidates[1])) { 
            check <- parse_check_results(check_candidates[1])
            check_output <- utils::capture.output(print.check_results(check),
                                                  type = "message")[2]
        } else {
            check_output  <- "ERROR: No check log given!"
        }
    }
    here <- c(here, check_output)
    if (file.exists(file.path(pkg[["path"]], "NEWS.md"))) {
        news <- get_news(path = path)
    } else {
        news <- "\nXXX: State your changes and consider using a NEWS.md\n\n"
    }
    comments <- c("Dear CRAN Team,\n")
    if (isTRUE(initial)) {
        comments <- c(comments, "this is the initial commit of package '",
                      pkg[["package"]], "'.\n\n", 
                      "XXX: Describe what it does.\n\n",
                      "Please consider uploading it to CRAN.\n")

    } else {
        comments <- c(comments, "this is a resubmission of package '",
                      pkg[["package"]], 
                      "'. I have added the following changes:\n",
                      news,
                      "Please upload to CRAN.\n")
    }
    comments <- c(comments, "Best, ", name, "\n\n")
    comments <- c(comments, "# Package ", pkg[["package"]], " ",
                  pkg[["version"]], 
                  "\n\nReporting is done by packager version ",
                  as.character(desc::desc_get_version(system.file("DESCRIPTION", 
                                                     package = "packager"))),
                  "\n\n## Test  environments ", "\n")
    comments <- c(comments, "- ", paste(here, collapse = "\n    "), "\n")
    if (is.null(proxy)) {
        gitlab_info <- get_gitlab_info(path = path,
                                       private_token = private_token)
    } else {
        gitlab_info <- get_gitlab_info(path = path,
                                       private_token = private_token, proxy)
    }
    if (!is.null(gitlab_info))
        comments <- c(comments, paste(c("- gitlab.com", gitlab_info),
                                      collapse = "\n  "), "\n")
    comments <- c(comments, "- win-builder (devel)", "\n")
    comments <- c(comments, get_local_tests(path))
    comments <- c(comments, get_local_meta(path))

    if (! as.logical(file.access(".", mode = 2))) # see ?file.acces return/note
        if (isTRUE(write_to_file))
            writeLines(comments, con = comments_file, sep = "")
    return(invisible(comments))
}

pick_log_file <- function(path, basename, extension = "(log|Rout)") {
    candidates <- list.files(file.path(path, "log"), 
                             pattern =  paste0(basename, "\\.", extension, "$"), 
                             full.names = TRUE)
    newest <- candidates[order(file.mtime(candidates), decreasing = TRUE)][1]
    return(newest)
}

get_local_tests <- function(path) {
    comments <- c("\n## Local test results\n")
    if (!is.na(log_file <- pick_log_file(path, "runit"))) {
        result <- grep(pattern = "test functions?.*errors.*failures", 
                       x = readLines(log_file), value = TRUE)
        comments <- c(comments, "- RUnit:\n    ", result, 
                      "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "testthat"))) {
        result <- grep(pattern = paste0("^(OK|Failed|Warnings|Skipped):"), 
                       x = readLines(log_file), value = TRUE)
        comments <- c(comments, "- Testthat:\n    ", 
                      paste(sub("\\s+", " ", result), collapse = ", "), "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "covr"))) {
        pkg <- devtools::as.package(path)
        result <- grep(pattern = paste0("^", pkg[["package"]], " Coverage:"), 
                       x = readLines(log_file), value = TRUE)
        comments <- c(comments, "- Coverage by covr:\n    ", result, 
                      "\n")
    }
    return(comments)
}

get_local_meta <- function(path) {
    comments <- c("\n## Local meta results\n")
    if (!is.na(log_file <- pick_log_file(path, "cyclocomp"))) {
        lines <- readLines(log_file)
        hits <- grep("Exceeding maximum cyclomatic complexity", lines, value = TRUE)
        if (length(hits) > 0) {
            result <- sub(">$", "", sub(".*: ", "", hits))
        } else {
            result <- "no issues."
        }
        comments <- c(comments, "- Cyclocomp:", paste("\n    ", result), 
                      "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "lintr"))) {
        lines <- readLines(log_file)
        lints_in_R <- sum(grepl("R/", lines))
        lines_in_R <- sum(sapply(lapply(list.files(file.path(path, "R"), 
                                                   full.names = TRUE), 
                                        readLines), length))
        comments <- c(comments, "- lintr:\n    ", 
                      paste0("found ", lints_in_R, " lints in ", lines_in_R, 
                            " lines of code (a ratio of ", 
                            round(lints_in_R / lines_in_R, 4), ")."),  
                      "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "cleanr"))) {
        lines <- readLines(log_file)
        comments <- c(comments, "- cleanr:\n    ", 
                      paste("found", max(length(lines) - 1, 0), 
                            "dreadful things about your code."),  
                      "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "usage"))) {
        lines <- readLines(log_file)
        comments <- c(comments, "- codetools::checkUsagePackage:\n    ", 
                      paste("found", max(length(lines) - 1, 0), 
                            "issues."),  
                      "\n")
    }
    if (!is.na(log_file <- pick_log_file(path, "spell"))) {
        lines <- readLines(log_file)
        result <- sum(!grepl("^ ", grep(":[1-9]", lines, value = TRUE)))
        comments <- c(comments, "- devtools::spell_check:\n    ", 
                      paste("found", result, 
                            "unkown words."),  
                      "\n")
    }
    return(comments)
}
