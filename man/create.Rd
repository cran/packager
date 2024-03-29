% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{create}
\alias{create}
\title{Create a Package Template}
\usage{
create(path, force = TRUE, ...)
}
\arguments{
\item{path}{The package to create.}

\item{force}{Recursively \code{\link{unlink}} \code{path} before calling
creating the package?}

\item{...}{Arguments to be passed to \code{\link{infect}}.}
}
\value{
\code{\link[base:invisible]{Invisibly}}
\code{\link[base:NULL]{NULL}}.
}
\description{
This is just a wrapper to create a package and
infect it using \code{\link{infect}}.
}
\examples{
path <- file.path(tempdir(), "myFirstPackage")
packager::create(path = path, fakemake = "roxygen2")
list.files(path, recursive = TRUE)
\dontrun{
if (require("roxygen2")) {
  ml <- packager::get_package_makelist(is_cran = TRUE)
  d <- file.path(tempdir(), "somePackage")
  dir.create(d)
  packager::create(d, fakemake = FALSE, fakemake = FALSE)
  withr::with_dir(d, fakemake::make("check", ml))
  check_log <- file.path(d, "log", "check.Rout")
  status <- packager::get_check_status(check_log)
  RUnit::checkEqualsNumeric(status[["status"]][["errors"]], 0)
  list.files(d, recursive = TRUE)
  unlink(d, recursive = TRUE)
}
}
}
\seealso{
\code{\link{infect}}
}
