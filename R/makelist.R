#' Provide a \code{makelist} Suitable for Packages with \pkg{packager}
#'
#' @param is_cran Streamline \code{makelist} for usage on CRAN?
#' @param gitlab_token A private gitlab token. Used to query logs on
#' \url{https://about.gitlab.com}.
#' @return A list for
#' \code{\link[fakemake:make]{fakemake::make}}.
#' @export
#' @examples
#' ml <- packager::get_package_makelist()
#' cbind(lapply(ml, function(x) x[["target"]]),
#'       lapply(ml, function(x) x[["alias"]]))
#'
#' cl <- packager::get_package_makelist(is_cran = TRUE)
#' setdiff(sapply(ml, function(x) x[["target"]]),
#'         sapply(cl, function(x) x[["target"]]))
get_package_makelist <- function(is_cran = FALSE, gitlab_token = NULL) {
    fat <- fakemake::add_target
    frt <- fakemake::remove_target
    ml <- get_basic_makelist()
    build_index <- which(sapply(ml, function(x) x["alias"] == "build"))
    build_target  <- ml[[build_index]][["target"]]
    cyclocomp_code <- paste0("print(tryCatch(",
                             "packager::check_cyclomatic_complexity(\".\")",
                             ", error = identity))")
    tinytest_code <- paste0("if (file.exists(file.path(\"tests\", ",
                            "\"tinytest.R\"))) ",
                            "print(tinytest::build_install_test(\".\"))")
    ml <- fat(makelist = ml,
                     target = file.path("README.md"),
                     sink = file.path("log", "readme.Rout"),
                     code = "knitr::knit(\"README.Rmd\")",
                     prerequisites = file.path("README.Rmd"),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "testthat.Rout"),
                     code = "devtools::test(\".\")",
                     prerequisites = c(list_files("R"), list_files("inst"),
                                       list_files("tests")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "tinytest.Rout"),
                     sink = file.path("log", "tinytest.Rout"),
                     code = tinytest_code,
                     prerequisites = c(list_files("R"), list_files("inst"),
                                       list_files("tests")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "cyclocomp.Rout"),
                     code = cyclocomp_code,
                     prerequisites = c(list_files("R"),
                                       file.path("log", "roxygen2.Rout")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "vignettes.Rout"),
                     code = paste0("devtools::build_vignettes(); ",
                                   "packager::extract_vignette_codes()"),
                     prerequisites =  c(list_files("R"), list_files("tests"),
                                        list_files("vignettes")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "check_codetags.Rout"),
                     code = paste0("tryCatch(print(packager::check_codetags())",
                                   ", error = identity)"),
                     prerequisites = c(list_files("R")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "news_rd.Rout"),
                     code = "print(packager::provide_news_rd())",
                     prerequisites = file.path("log", "news.Rout"),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "news.Rout"),
                     code = "print(packager::check_news())",
                     prerequisites = c("DESCRIPTION", "NEWS.md"),
                     prerequisite_to = file.path("log", "news_rd.Rout"))
    ml <- fat(makelist = ml,
                     target = file.path("log", "usage.Rout"),
                     code = "print(packager::check_usage())",
                     prerequisites =  c(list_files("R"), list_files("tests"),
                                        list_files("vignettes")),
                     prerequisite_to = build_target)
    ml <- fat(makelist = ml,
              target = file.path("log", "dev_install.Rout"),
              code =  "devtools::install()",
              prerequisites = c(list_files("R"), list_files("inst"),
                                list_files("tests")))
    ml <- fat(makelist = ml,
              target = file.path("log", "runit.Rout"),
              code =  paste0("pkg <- packager::as.package(\".\"); ",
                             "callr::rscript(file.path(pkg[[\"path\"]],",
                             "\"tests\", ", "\"runit.R\"))"),
              prerequisites = file.path("log", "dev_install.Rout"),
              prerequisite_to = build_target)
    ml <- fat(makelist = ml,
                     target = file.path("log", "winbuilder.Rout"),
                     code = "devtools::check_win_devel()",
                     prerequisites = NULL,
                     prerequisite_to = NULL)
    if (isTRUE(is_cran)) {
        ml <- frt(ml, file.path("log", "check.Rout"))
        ml <- fat(ml,
                  target = file.path("log", "check.Rout"),
                  code = "packager::check_archive_as_cran(packager::get_pkg_archive_path())",
                  prerequisites = "packager::get_pkg_archive_path(absolute = FALSE)")
    }
    ml <- fat(makelist = ml,
                     target = file.path("log", "install.Rout"),
                     code = "fritools::r_cmd_install(path = \".\")",
                     prerequisites = file.path("log", "check.Rout"),
                     prerequisite_to = NULL)
    ml <- fat(makelist = ml,
                     target = "cran-comments.md",
                     alias = "cran_comments",
                     sink = file.path("log", "cran_comments.Rout"),
                     code = paste0("packager::provide_cran_comments(",
                                   "check_log = file.path(\"log\", \"check.Rout\"), ",
                                   "private_token = ", deparse(gitlab_token),
                                   ")"),
                     prerequisites = file.path("log", "install.Rout"),
                     prerequisite_to = NULL)
    ml <- fat(ml,
              target = file.path("log", "submit.Rout"),
              code = paste0("is_clean <- ! packager:::is_git_uncommitted(\".\");",
                            "packager::submit(path = \".\", ",
                            "force = TRUE, stop_on_git = ! is_clean)"
                            ),
              prerequisites = file.path("log", "cran_comments.Rout")
              )
    if (isTRUE(is_cran)) {
        ml <- frt(frt(frt(ml, file.path("log", "runit.Rout")),
                      file.path("log", "dev_install.Rout")),
                  file.path("log", "cyclocomp.Rout"))

    }
    return(add_detach(add_log(ml)))
}

get_basic_makelist <- function() {
    roxygen_code  <- "print(roxygen2::roxygenize(\".\"))"
    cleanr_code <- paste0("print(tryCatch(cleanr::check_directory(\"R\", ",
                         "check_return = FALSE), ",
                         "cleanr = identity, ",
                         "error = identity",
                         "))")
    spell_code <- paste0("spell <- devtools::spell_check(); ",
                        "if (length(spell) > 0) {print(spell); ",
                        "warning(\"Spell check failed, see \",",
                        "file.path(getwd(), \"log\", \"spell.Rout\"),",
                        "\" for details.\")}")
    # detach package after covr attached it. Required by R-devel 4.0.0 on
    # win_builder, else the vignette crashes, because covr throws an exception
    # due to not loading the readily attached library.
    covr_code <- paste0("co <- covr::package_coverage(path = \".\"); ",
                       "print(covr::zero_coverage(co)); print(co); ")
    build_code <- paste0("print(pkgbuild::build(path = \".\", ",
                        "dest_path = \".\", vignettes = TRUE))")
    r_codes <- paste0("grep(list.files(\".\", ",
                                  "pattern = \".*\\\\.[rR]$\", ",
                                  "recursive = TRUE), ",
                                  "value = TRUE, ",
                                  "pattern = \"^R/|^inst/|^tests/\")")
    deps_code = paste0("packager::install_deps(path = \".\", verbose = TRUE, ",
                       "repos = \"https://cloud.r-project.org/\");",
                       " packager::install_deps(path = system.file(",
                       "\"DESCRIPTION\", package = \"packager\"),",
                       " verbose = TRUE,",
                       " repos = \"https://cloud.r-project.org/\")")
    pl <- list(list(alias = "roxygen2",
                    target = file.path("log", "roxygen2.Rout"),
                    code = roxygen_code,
                    prerequisites = c(file.path("log", "dependencies.Rout"),
                                      list_files("R"))),
               list(alias= "dependencies",
                    target = file.path("log", "dependencies.Rout"),
                    code = deps_code,
                    prerequisites = "DESCRIPTION"),
               list(alias = "spell",
                    target = file.path("log", "spell.Rout"),
                    code = spell_code,
                    prerequisites = c("DESCRIPTION",
                                      file.path("log", "roxygen2.Rout"))),
               list(alias = "cleanr",
                    target = file.path("log", "cleanr.Rout"),
                    code = cleanr_code,
                    prerequisites = r_codes),
               list(alias = "lint",
                    target = file.path("log", "lintr.Rout"),
                    code = "print(packager::lint_package(path = \".\"))",
                    prerequisites = r_codes),
               list(alias = "covr",
                    target = file.path("log", "covr.Rout"),
                    code = covr_code,
                    prerequisites = c(list_files("R"), list_files("tests"),
                                      list_files("inst"))),
               list(alias = "build",
                    target = "packager::get_pkg_archive_path(absolute = FALSE)",
                    code = build_code,
                    sink = "log/build.Rout",
                    prerequisites = c(list_files("R"), list_files("man"),
                                      "DESCRIPTION", "LICENSE",
                                      "file.path(\"log\", \"lintr.Rout\")",
                                      "file.path(\"log\", \"cleanr.Rout\")",
                                      "file.path(\"log\", \"spell.Rout\")",
                                      "file.path(\"log\", \"covr.Rout\")",
                                      "file.path(\"log\", \"roxygen2.Rout\")")),
               list(alias = "check", target = "log/check.Rout",
                    code = paste0("packager::check_archive(packager::get_pkg_archive_path(), ",
                                  "cmdargs = \"--no-manual\")"),
                    prerequisites = "packager::get_pkg_archive_path(absolute = FALSE)"))
    return(add_log(pl))
}

list_files <- function(x) {
    return(paste0("list.files(\"", x,
                 "\", full.names = TRUE, recursive = TRUE)"))
}

add_detach <- function(makelist) {
    ml <- makelist
    prepend_detach_to_all_codes <- function(x) {
        detach_code <-  paste0("pkg <- packager::as.package(\".\")",
                               "[[\"package\"]]; ",
                               "if (pkg %in% .packages()) ",
                               "detach(paste0(\"package:\", pkg), ",
                               "unload = TRUE, ",
                               "character.only = TRUE)")
        x[["code"]] <- paste(detach_code, x[["code"]], sep = " ;")
        return(x)
    }
    ml <- lapply(ml, prepend_detach_to_all_codes)
}

add_log <- function(x) {
    fat <- fakemake::add_target
    fat(x, target = ".log.Rout",
        code = c("packager::use_directory(\"log\", ignore = TRUE)"),
           prerequisite_to = TRUE, prerequisites = NULL)
}
