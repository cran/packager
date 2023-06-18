#' Create a Package Template
#'
#' This is just a wrapper to create a package and
#' infect it using \code{\link{infect}}.
#'
#'
#' @param path The package to create.
#' @param force Recursively \code{\link{unlink}} \code{path} before calling
#' creating the package?
#' @param ... Arguments to be passed to \code{\link{infect}}.
#' @template return_invisibly_null
#' @export
#' @seealso \code{\link{infect}}
#' @examples
#' path <- file.path(tempdir(), "myFirstPackage")
#' packager::create(path = path, fakemake = "roxygen2")
#' list.files(path, recursive = TRUE)
#' \dontrun{
#' if (require("roxygen2")) {
#'   ml <- packager::get_package_makelist(is_cran = TRUE)
#'   d <- file.path(tempdir(), "somePackage")
#'   dir.create(d)
#'   packager::create(d, fakemake = FALSE, fakemake = FALSE)
#'   withr::with_dir(d, fakemake::make("check", ml))
#'   check_log <- file.path(d, "log", "check.Rout")
#'   status <- packager::get_check_status(check_log)
#'   RUnit::checkEqualsNumeric(status[["status"]][["errors"]], 0)
#'   list.files(d, recursive = TRUE)
#'   unlink(d, recursive = TRUE)
#' }
#' }
create <- function(path, force = TRUE, ...) {
    checkmate::qassert(force, "B1")
    checkmate::qassert(path, "S1")
    if (isTRUE(force)) unlink(path, recursive = TRUE)
    package_skeleton(path = path, force = force)
    r <- gert::git_init(path = path)
    git_add_commit(path = r, message = "Initial Commit", untracked = TRUE)
    unpatch_r_version(path = path)
    desc::desc_set_version("0.1.0", file = file.path(path, "DESCRIPTION"),
                           normalize = FALSE)
    git_add_commit(path, "Clean DESCRIPTION")
    infect(path = path, ...)
    return(invisible(NULL))
}

#' Adjust a Package
#'
#' Add a variety of extensions to a package (skeleton) and run
#' \code{\link[fakemake:make]{fakemake::make}} on
#' it.
#'
#' @template package_path
#' @param git_add_and_commit Add and commit changes in git?
#' @param fakemake The \code{name} for a
#' \code{\link[packager:get_package_makelist]{makelist}} for \pkg{fakemake}.
#' Set to \code{\link{NULL}} or \code{\link{FALSE}} to disable running
#' \code{\link[fakemake:make]{fakemake::make}}.
#' @param ... Arguments to be passed to \code{\link{set_package_info}}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' \code{\link[base:NULL]{NULL}}.
#' @seealso \code{\link{create}}
#' @export
#' @examples
#' \dontrun{
#' if (require("roxygen2")) {
#' path <- file.path(tempdir(), "mySecondPackage")
#' usethis::create_package(path = path, open = FALSE)
#' l1 <- list.files(path, recursive = TRUE)
#' packager::infect(path = path, fakemake = "roxygen2", fakemake = FALSE)
#' l2 <- list.files(path, recursive = TRUE)
#' print(l1); print(l2)
#' unlink(path, recursive = TRUE)
#' }
#' }
infect <- function(path, fakemake = "check", git_add_and_commit = TRUE, ...) {
    checkmate::qassert(git_add_and_commit, "B1")
    checkmate::assert_directory_exists(path)
    checkmate::qassert(fakemake, c("S1", 0, "B1"))
    r <- gert::git_init(path = path)
    use_build_ignore("^.*\\.tar\\.gz$", escape = FALSE, pkg = path)
    use_build_ignore(paste0(as.package(path)[["package"]],
                             ".Rcheck"), pkg = path)
    use_build_ignore("cran-comments.md", pkg = path)
    use_build_ignore(".Rprofile", pkg = path)
    use_build_ignore("TODO.md", pkg = path)
    use_build_ignore("index.html", pkg = path)
    use_git_ignore(".Rprofile", path = path)
    use_git_ignore("*.tar.gz", path = path)
    use_git_ignore(paste0(as.package(path)[["package"]],
                                    ".Rcheck"), path = path)
    use_makefile(path = path)
    set_package_info(path = path, ...)
    remove_Rproj(path = path)
    use_devtools(path = path)
    use_travis(path = path)
    use_gitlab_ci(path = path)
    use_devel(path = path)
    set_desc_url(provide_gitlab_url(path = path), path = path, overwrite = TRUE,
                 do_remind = TRUE, do_commit = FALSE)
    use_bsd2clause_license(path = path)
    use_testthat(path = path)
    use_tinytest(path = path)

    provide_throw(path = path)
    provide_make(path = path)
    provide_zzz(path = path)
    provide_man_roxygen(path = path)


    use_build_ignore(".log.Rout", pkg = path)
    use_directory("log", pkg = path, ignore = TRUE)
    if (!(is.null(fakemake) || fritools::is_false(fakemake))) {
        ml <- get_package_makelist(is_cran = TRUE)
        fritools::with_dir(path, print(fakemake::make(name = fakemake,
                                                      make_list = ml,
                                                      verbose = FALSE)))
    }
    if (isTRUE(git_add_and_commit)) {
        git_add_commit(path = r, message = "Packager Changes", untracked = TRUE)
    }
    sanitize_usethis_git_hook(path)
    use_git_check_version_not_tagged(path)
    return(invisible(NULL))
}

#' Set a Package's Info
#'
#' Fill DESCRIPTION, R/xxx-package.R and an introductory vignette with the same
#' Title, Description and possibly Details,
#' keeping the info given in different places identical.
#'
#' @param path Path to the package.
#' @param author_at_r A \code{\link[utils:person]{person}} object.
#' @param title A string giving the title.
#' @param description A string giving the description.
#' @param details A string giving the details. Defaults to NA in which case a
#' default details are inserted. Set to NULL to have no details at all.
#' @param ... Arguments to be passed to internal function
#' \code{packager:::use_intro}.
#' @return \code{\link[base:invisible]{Invisibly}}
#' a list of results of setting the xxx-package.R and the DESCRIPTION.
#' @keywords internal
#' @export
#' @examples
#' path <- file.path(tempdir(), "myPackage")
#' usethis::create_package(path = path, open = FALSE)
#' a  <- utils::person("Your", "Name", "some@whe.re", role = c("aut", "cre"))
#' set_package_info(path = path, author_at_r = a, title = "What Now?",
#'                  description = "This package does nothing.",
#'                  details = "Details do not show up in DESCRIPTION.")
#' package_desc <- file.path(path, "DESCRIPTION")
#' package_info_file <- file.path(path,
#'                                "R", paste0(basename(path), "-package.R"))
#' readLines(package_desc)
#' readLines(package_info_file)
#' unlink(path, recursive = TRUE)
set_package_info <- function(path,
                             author_at_r = getOption("packager")[["whoami"]],
                             title = "What it Does (One Line, Title Case)",
                             description = NULL, details = NA, ...) {
    checkmate::assert_directory_exists(path)
    checkmate::qassert(title, "S1")
    checkmate::qassert(description, c(0, "S1"))
    checkmate::assert_string(details, na.ok = TRUE)
    checkmate::assert_class(author_at_r, "person", null.ok = TRUE)
    r1 <- update_description(path = path, title = tools::toTitleCase(title),
                             description = description,
                             author_at_r = author_at_r)
    r2 <- create_package_help(path = path, title = tools::toTitleCase(title),
                              description = description, details = NA)
    r3 <- use_intro(path = path, details = details, ...)
    return(invisible(list(r1, r2, r3)))
}
