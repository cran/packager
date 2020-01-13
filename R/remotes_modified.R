# from remotes package version 1.1.1
`%::%` <- function(p, f) get(f, envir = asNamespace(p))

has_package <- function(pkg) {
  if (pkg %in% loadedNamespaces()) {
    TRUE
  }
  else {
    requireNamespace(pkg, quietly = TRUE)
  }
}

safe_install_packages <- function(...) {
  lib <- paste(.libPaths(), collapse = ":")
  if (has_package("crancache") && has_package("callr")) {
    i.p <- "crancache" %::% "install_packages"
  } else {
    i.p <- utils::install.packages
  }
  withr::with_envvar(c(
    R_LIBS = lib, R_LIBS_USER = lib, R_LIBS_SITE = lib,
    R_PROFILE_USER = tempfile()
  ), i.p(...))
}

install_packages <- function(packages, repos = getOption("repos"),
                             type = getOption("pkgType"),
                             ..., dependencies = FALSE, quiet = NULL) {
  if (identical(type, "both")) {
    type <- "binary"
  }
  if (is.null(quiet)) {
    quiet <- !identical(type, "source")
  }
  message(
    "Installing ", length(packages), " packages: ",
    paste(packages, collapse = ", ")
  )
  safe_install_packages(packages,
    repos = repos, type = type,
    ..., dependencies = dependencies, quiet = quiet
  )
}

update <- function(object, ..., quiet = FALSE, upgrade = TRUE) {
  ahead <- object$package[object$diff == 2L]
  if (length(ahead) > 0 && !quiet) {
    message(
      "Skipping ", length(ahead), " packages not available: ",
      paste(ahead, collapse = ", ")
    )
  }
  missing <- object$package[object$diff == 1L]
  if (length(missing) > 0 && !quiet) {
    message(
      "Skipping ", length(missing), " packages ahead of CRAN: ",
      paste(missing, collapse = ", ")
    )
  }
  if (upgrade) {
    behind <- object$package[object$diff < 0L]
  } else {
    behind <- object$package[is.na(object$installed)]
  }
  if (length(behind) > 0L) {
    install_packages(behind,
      repos = attr(object, "repos"),
      type = attr(object, "type"), ...
    )
  }
}
