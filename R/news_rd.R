#' Convert \file{NEWS.md} to \file{inst/NEWS.rd}
#'
#' The file \file{inst/NEWS.rd} will get listed in the package's help index.
#' Christian Vonderach uses the index thus wants \file{inst/NEWS.rd} to
#' automatically be in sync with \file{./NEWS.md}.
#'
#' @template package_path
#' @template verbose
#' @param force Overwrite an existing file?
#' @export
#' @keywords internal
#' @return \code{\link{TRUE}} on success, \code{\link{FALSE}} otherwise.
provide_news_rd <- function(path = ".",
                            force = is_force(),
                            verbose = TRUE) {
    status <- FALSE
    temp_file <- file.path(tempdir(), "NEWS.R")
    root <- as.package(path)[["path"]]
    out_file <- file.path(root, "inst", "NEWS.rd")
    if (requireNamespace("document", quietly = TRUE)) {
        file  <- file.path(root, "NEWS.md")
        news <- readLines(file)
        news <- sub("^(# ).* ([0-9].*$)", "\\1Changes in version \\2", news)
        news <- gsub("\\\\_", "\\_", news)
        writeLines(c(paste0("#' ",
                            c("@title NEWS",
                              "@description DESCREMOVE",
                              "",
                              news,
                              "@name NEWS",
                              "@md"
                              )
                            ),
                     "NULL"),
                   temp_file)
        temp_dir <- tempfile()
        suppressWarnings(document::document(temp_file, check_package = FALSE,
                                            working_directory = temp_dir))
        # R CMD check warns about:
        # ===
        # Problems with news in ‘inst/NEWS.Rd’:
        #     Cannot extract version info from the following section titles:
        # ===
        # So we use NEWS.rd, which passes R CMD check without warnings:
        temp_file <- file.path(temp_dir, "NEWS", "man", "NEWS.Rd")
        news <- readLines(temp_file)
        # need to get a blank line before first section.
        # This is quick and dirty:
        news[grep("^\\\\title", news)] <- ""
        news[grep("^\\\\alias", news)] <- "\\title{NEWS}"
        # remove the description
        drm <-  grep("^DESCREMOVE", news)
        i <- c(grep("^%", news), grep("^\\\\alias", news),
               seq(drm - 1, drm + 1))
        index <- !(seq(along = news) %in% i)
        info <- c(paste("This file is auto-generated from ../NEWS.md",
                        "using packager::provide_news_rd()."),
                  "Do not edit here!")
        info <- paste("#", info)
        # do not add info, CRAN gives a warning...
        writeLines(c(news[index]), temp_file)
    } else {
        msg <- paste("Cannot load package `document`. Please install first.",
                     "Use",
                     "\tinstall.packages(\"document\")", ".\n", sep = "\n")
        if (isTRUE(verbose)) message(msg)
        cat(msg, file = temp_file)
    }
    status <- file.copy(temp_file, out_file, overwrite = force)
    return(status)
}
