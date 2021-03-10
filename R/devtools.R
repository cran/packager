# verbatim copies of internals and dependencies of internals from devtools
# 1.13.3 but with "#' @export" tags removed.
has_description <- function(path) {
  file.exists(file.path(path, "DESCRIPTION"))
}

strip_slashes <- function(x) {
  x <- sub("/*$", "", x) # Exclude Linting
  x
}

is_root <- function(path) {
  identical(path, dirname(path))
}

is_dir <- function(x) file.info(x)$isdir

is.package <- function(x) inherits(x, "package")


#' Coerce Input to a Package
#'
#' Possible specifications of package:
#' \itemize{
#'   \item path
#'   \item package object
#' }
#' @param x object to coerce to a package
#' @export
#' @keywords internal
as.package <- function(x) {
    if (is.package(x)) {
        root <- x
    } else {
        root <- rprojroot::find_root(path = x, rprojroot::is_r_package)
        root <- load_pkg_description(root)
    }
    return(root)
}

#' Add Files to \file{.Rbuildignore}
#'
#' This is verbatim copy of  git commit
#' \verb{a5e5805ecd630ebc46e080bd78ebcf32322efe3c} of \pkg{usethis}.\cr
#' \file{.Rbuildignore} has a regular expression on each line, but it's
#' usually easier to work with specific file names. By default, will (crudely)
#' turn filenames into  regular expressions that will only match these
#' paths. Repeated entries will be silently removed.
#'
#' @param pkg Path to the package directory (see
#' \code{\link[devtools:as.package]{as.package}}).
#' @param files Paths of files.
#' @param escape If \code{TRUE}, the default, will escape \code{.} to
#'   \code{\\.} and surround with \code{^} and \code{$}.
#' @return Nothing, called for its side effect.
#' @export
use_build_ignore <- function(files, escape = TRUE, pkg = ".") {
  pkg <- as.package(pkg)

  if (escape) {
    files <- paste0("^", gsub("\\.", "\\\\.", files), "$") # Exclude Linting
  }

  path <- file.path(pkg$path, ".Rbuildignore")
  union_write(path, files)

  invisible(TRUE)
}

#' Use a Directory
#'
#' Create a directory.\cr
#' this is verbatim copy of  git commit
#' \verb{a5e5805ecd630ebc46e080bd78ebcf32322efe3c} of \pkg{usethis}.
#'
#' @param path Path of the directory to create, relative to the project.
#' @inheritParams use_build_ignore
#' @param ignore Add the directory to \file{.Rbuildignore}?
#'
#' @export
use_directory <- function(path, ignore = FALSE, pkg = ".") {
  pkg <- as.package(pkg)
  pkg_path <- file.path(pkg$path, path)

  if (file.exists(pkg_path)) {
    if (!is_dir(pkg_path)) {
      stop("`", path, "` exists but is not a directory.", call. = FALSE)
    }
  } else {
    message("* Creating `", path, "`.")
    dir.create(pkg_path, showWarnings = FALSE, recursive = TRUE)
  }

  if (ignore) {
    message("* Adding `", path, "` to `.Rbuildignore`.")
    use_build_ignore(path, pkg = pkg)
  }

  invisible(TRUE)
}

check_dep_version <- function(dep_name, dep_ver = NA, dep_compare = NA) {
  if (!requireNamespace(dep_name, quietly = TRUE)) {
    stop("Dependency package ", dep_name, " not available.")
  }

  if (xor(is.na(dep_ver), is.na(dep_compare))) {
    stop("dep_ver and dep_compare must be both NA or both non-NA")
  } else if (!is.na(dep_ver) && !is.na(dep_compare)) {
    compare <- match.fun(dep_compare)
    if (!compare(
      as.numeric_version(getNamespaceVersion(dep_name)),
      as.numeric_version(dep_ver)
    )) {
      warning(
        "Need ", dep_name, " ", dep_compare,
        " ", dep_ver,
        " but loaded version is ", getNamespaceVersion(dep_name)
      )
    }
  }
  return(TRUE)
}


parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, "[[:space:]]*,[[:space:]]*")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if (!all(compare_valid)) {
    stop(
      "Invalid comparison operator in dependency: ",
      paste(compare_nna[!compare_valid], collapse = ", ")
    )
  }

  deps <- data.frame(
    name = names, compare = compare,
    version = versions, stringsAsFactors = FALSE
  )

  # Remove R dependency
  deps[names != "R", ]
}

suggests_dep <- function(pkg) {
  suggests <- read_dcf(system.file("DESCRIPTION",
                                   package = "devtools"))$Suggests
  deps <- parse_deps(suggests)

  found <- which(deps$name == pkg)[1L]

  if (!length(found)) {
    stop(sQuote(pkg), " is not in Suggests: for devtools!", call. = FALSE)
  }
  deps[found, ]
}


can_overwrite <- function(path) {
  name <- basename(path)

  if (!file.exists(path)) {
    TRUE
  } else if (interactive() && !yesno("Overwrite `", name, "`?")) {
    TRUE
  } else {
    FALSE
  }
}


add_desc_package <- function(pkg = ".", field, name) {
  pkg <- as.package(pkg)
  desc_path <- file.path(pkg$path, "DESCRIPTION")

  desc <- read_dcf(desc_path)
  old <- desc[[field]]
  if (is.null(old)) {
    new <- name
    changed <- TRUE
  } else {
    if (!grepl(paste0("\\b", name, "\\b"), old)) {
      new <- paste0(old, ",\n    ", name)
      changed <- TRUE
    } else {
      changed <- FALSE
    }
  }
  if (changed) {
    desc[[field]] <- new
    write_dcf(desc_path, desc)
  }
  invisible(changed)
}


read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function(path, desc) {
  desc <- unlist(desc)
  # Add back in continuation characters
  desc <- gsub("\n[ \t]*\n", "\n .\n ", desc, perl = TRUE, useBytes = TRUE)
  desc <- gsub("\n \\.([^\n])", "\n  .\\1", desc, perl = TRUE, useBytes = TRUE)

  starts_with_whitespace <- grepl("^\\s", desc, perl = TRUE, useBytes = TRUE)
  delimiters <- ifelse(starts_with_whitespace, ":", ": ")
  text <- paste0(names(desc), delimiters, desc, collapse = "\n")

  # If the description file has a declared encoding, set it so nchar() works
  # properly.
  if ("Encoding" %in% names(desc)) {
    Encoding(text) <- desc[["Encoding"]]
  }

  if (substr(text, nchar(text), 1) != "\n") {
    text <- paste0(text, "\n")
  }

  cat(text, file = path)
}

parse_check_results <- function(path) {
  lines <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # Strip off trailing NOTE and WARNING messages
  lines <- gsub("^NOTE: There was .*\n$", "", lines)
  lines <- gsub("^WARNING: There was .*\n$", "", lines)

  pieces <- strsplit(lines, "\n\\* ")[[1]]

  structure(
    list(
      errors = pieces[grepl("... ERROR", pieces, fixed = TRUE)],
      warnings = pieces[grepl("... WARN", pieces, fixed = TRUE)],
      notes = pieces[grepl("... NOTE", pieces, fixed = TRUE)]
    ),
    path = path,
    class = "check_results"
  )
}

print.check_results <- function(x, ...) {
  message("R CMD check results")
  message(summarise_check_results(x))

  cat(format(x), "\n", sep = "")
  invisible(x)
}

summarise_check_results <- function(x, colour = FALSE) {
  n <- lapply(x, length)
  paste0(
    show_count(n$errors, "error ", "errors", colour && n$errors > 0), " | ",
    show_count(n$warnings, "warning ", "warnings", colour && n$warnings > 0),
    " | ",
    show_count(n$notes, "note ", "notes")
  )
}

show_count <- function(n, singular, plural, is_error = FALSE) {
  out <- paste0(n, " ", ngettext(n, singular, plural))
  if (is_error && requireNamespace("crayon", quietly = TRUE)) {
    out <- crayon::red(out)
  }
  out
}

cran_comments <- function(pkg = ".") {
  pkg <- as.package(pkg)

  path <- file.path(pkg$path, "cran-comments.md")
  if (!file.exists(path)) {
    warning("Can't find cran-comments.md.\n",
      "This file gives CRAN volunteers comments about the submission,\n",
      "and it must exist. Create it with use_cran_comments().\n",
      call. = FALSE
    )
    return(character())
  }

  paste0(readLines(path, warn = FALSE), collapse = "\n")
}

as.object_size <- function(x) structure(x, class = "object_size")

maintainer <- function(pkg = ".") {
  pkg <- as.package(pkg)

  authors <- pkg$`authors@r`
  if (!is.null(authors)) {
    people <- eval(parse(text = authors))
    if (is.character(people)) {
      maintainer <- utils::as.person(people)
    } else {
      maintainer <- Find(function(x) "cre" %in% x$role, people)
    }
  } else {
    maintainer <- pkg$maintainer
    if (is.null(maintainer)) {
      stop("No maintainer defined in package.", call. = FALSE)
    }
    maintainer <- utils::as.person(maintainer)
  }

  list(
    name = paste(maintainer$given, maintainer$family),
    email = maintainer$email
  )
}
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
load_pkg_description <- function(path) {
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
  use_git_hook(path = path, hook = "pre-commit",
               script = render_template("readme-rmd-pre-commit.sh")
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
