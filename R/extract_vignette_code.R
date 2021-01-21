#' Extract and Save R Code Vignettes
#'
#' I want R code from different kinds of vignettes stored in inst/doc.
#' @export
#' @keywords internal
#' @template package_path
#' @template return_invisibly_null
extract_vignette_codes <- function(path = ".") {
    dir <- as.package(".")[["path"]]
    dir.create(file.path(dir, "inst", "doc"))
    lapply(tools::pkgVignettes(dir = dir)[["docs"]],
           function(x) {
               if (grepl("\\.Rnw$$", x)) {
                   withr::with_dir(file.path(dir, "inst", "doc"), utils::Stangle(x))
               } else {
                   knitr::purl(x,
                               output = file.path(dir, "inst", "doc",
                                                  sub("\\.[rR](md|asciidoc)$$",
                                                      ".R", basename(x))
                                                  ),
                               documentation = 0)
               }
           }
           )
    return(invisible(NULL))
}
