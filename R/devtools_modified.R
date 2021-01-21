# Begin Exclude Linting
# infrastructure.R in devtools 1.13.3 states:
## #' @details
## #' Instead of the use_xyz functions from devtools use
## #' \link[usethis]{use_testthat}.
## #' @rdname devtools-deprecated
# and then heavily uses
## #'@importsFrom usethis

# Now devtools 1.13.3 was released on CRAN:
# # devtools_1.13.3.tar.gz	2017-08-02 09:05
# But the first release of usethis appeared on CRAN:
# # usethis_1.0.0.tar.gz	2017-10-22 19:36
# I do not know how they imported from a package not yet released, but that is
# what they did.
# So I just got copies of the imported functions from usethis by calling the
# functions via devtools at the time. And then I modified most of them.
# End Exclude Linting

# added from devtools 2.0.0, where args has no default.
# Pass force to pkgbuild::build(), move messages to submit.
build_cran <- function(path, args = NULL, force = FALSE) {
    message("Building")
    built_path <- pkgbuild::build(path, tempdir(), manual = TRUE,
                                  clean_doc = force,
                                  args = args)
    return(built_path)
}


# get rid of the interactive() part using yesno() to create the package.
# Blow if there is none!
load_pkg_description <- function(path, create) {
  path_desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(path_desc)) {
    stop("No description at ", path_desc, call. = FALSE)
  }
  desc <- as.list(read.dcf(path_desc)[1, ])
  names(desc) <- tolower(names(desc))
  desc$path <- path
  structure(desc, class = "package")
}


# change "devtools" to "usethis"
render_template <- function(name, data = list()) {
  path <- system.file("templates", name, package = "usethis")
  template <- readLines(path)
  whisker::whisker.render(template, data)
}

# adjust use_readme_rmd to not pass the argument \code{open} with use_template()
# and fix the test on file.exists()
# added git_user to pkg
use_readme_rmd <- function(path = ".", ...) {
  pkg <- as.package(path)
  if (uses_git(path)) {
    pkg[["git_user"]] <- tryCatch(sub(" <.*>$", "",
                                      gert::git_signature_default(path)),
      error = function(e) return(NULL)
    )
  }

  pkg$Rmd <- TRUE
  use_template("README.Rmd",
    save_as = "README.Rmd", data = pkg,
    ignore = TRUE, pkg = pkg, ...
  )
  use_build_ignore("^README-.*\\.png$", escape = FALSE, pkg = pkg)
  if (uses_git(pkg$path) && !file.exists(file.path(
    pkg$path, ".git",
    "hooks", "pre-commit"
  ))) {
    message("* Adding pre-commit hook")
    usethis::use_git_hook("pre-commit",
      render_template("readme-rmd-pre-commit.sh")
    )
  }
  return(invisible(NULL))
}

use_news_md <- function(pkg = ".", ...) {
  pkg <- as.package(pkg)
  use_template("NEWS.md", data = pkg, pkg = pkg, ...)
  invisible(NULL)
}

use_intro <- function(path = ".", ..., details = NA,
                      use_rasciidoc_vignette = FALSE) {
  checkmate::assert_directory_exists(path)
  checkmate::qassert(use_rasciidoc_vignette, "B1")
  checkmate::assert_character(details)
  if (is.na(details)) details <- NULL # NA would get printed into vignette.
  pkg <- as.package(path)
  pkg$details <- details
  pkg$date <- format(Sys.time(), "%Y-%m-%d, %H:%M:%S")
  vignette_name <- paste0(
    "An_Introduction_to_",
    pkg[["package"]], ".Rmd"
  )
  check_suggested("rmarkdown")
  add_desc_package(pkg, "Suggests", "knitr")
  add_desc_package(pkg, "Suggests", "rmarkdown")
  add_desc_package(pkg, "Suggests", "pkgload")
  add_desc_package(pkg, "VignetteBuilder", "knitr")
  use_directory("vignettes", pkg = pkg)
  file_path <- file.path("vignettes", vignette_name)
  use_template("vignette.Rmd",
    save_as = file_path, data = pkg,
    ignore = FALSE, pkg = pkg, ...
  )
  if (isTRUE(use_rasciidoc_vignette))
      convert_package_vignettes(path = path)
  return(invisible(NULL))
}

use_travis <- function(path = ".", ...) {
  pkg <- as.package(path)
  use_template("travis.yml", ".travis.yml",
    ignore = TRUE,
    pkg = pkg, ...
  )
  return(invisible(NULL))
}

# devtools' version works only for R packages, which does not make much sense to
# me.
use_git <- function(path = ".", message = "Initial commit") {
  if (is_git_repository(path)) {
    message("* Git is already initialized")
    return(invisible())
  }
  message("* Initialising repo")
  r <- gert::git_init(path)
  use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), path = path)
  message("* Adding files and committing")
  git_add_commit(path = r, untracked = TRUE, message = message)
  invisible(path)
}

# Modified copy of devtools' unexported version
# added argument 'prepend'
union_write <- function(path, new_lines, prepend = FALSE) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
  }
  else {
    lines <- character()
  }
  if (isTRUE(prepend)) {
      all <- union(new_lines, lines)
  } else {
      all <- union(lines, new_lines)
  }
  writeLines(all, path)
}

# Modified copy of devtools' unexported version
use_git_ignore <- function(ignores, path = ".") {
  paths <- paste0("`", ignores, "`", collapse = ", ")
  message("* Adding ", paths, " to ", file.path(
    path,
    ".gitignore"
  ))
  path <- file.path(path, ".gitignore")
  union_write(path, ignores)
  invisible(TRUE)
}


# call utils::
check_suggested <- function(pkg, version = NULL, compare = NA) {
  if (is.null(version)) {
    if (!is.na(compare)) {
      stop("Cannot set ", sQuote(compare), " without setting ",
        sQuote(version),
        call. = FALSE
      )
    }
    dep <- suggests_dep(pkg)
    version <- dep$version
    compare <- dep$compare
  }
  if (!fritools::is_r_package_installed(pkg) || !check_dep_version(
    pkg, version,
    compare
  )) {
    msg <- paste0(
      sQuote(pkg), if (is.na(version)) {
        ""
      } else {
        paste0(" >= ", version)
      },
      " must be installed for this functionality."
    )
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      }
      else {
        stop(msg, call. = FALSE)
      }
    }
    else {
      stop(msg, call. = FALSE)
    }
  }
}

# call utils::
yesno <- function(...) {
  yeses <- c(
    "Yes", "Definitely", "For sure", "Yup", "Yeah",
    "I agree", "Absolutely"
  )
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
  cat(paste0(..., collapse = ""))
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))
  utils::menu(qs[rand]) != which(rand == 1)
}
# twirk the return value. Should be TRUE on yeses! Rename function accordingly
is_yes <- function(...) {
  yeses <- c(
    "Yes", "Definitely", "For sure", "Yup", "Yeah",
    "I agree", "Absolutely"
  )
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
  cat(paste0(..., collapse = ""))
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))
  utils::menu(qs[rand]) == which(rand == 1)
}
