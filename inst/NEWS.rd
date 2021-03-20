\name{NEWS}
\title{NEWS}

\section{Changes in version 1.11.1}{
\itemize{
\item Fixed missing codes in vignette.
}
}

\section{Changes in version 1.11.0}{
\itemize{
\item Gitlab logs for cran-comments.md are now checked for the appropriate version.
\item Added more templates for man-roxygen.
\item Devel versions for \code{check_news()} only match first order sections (lines
starting with \verb{# }). So we can keep documentation of changes in devel versions
in the subsections.
\item Substituted imports from \code{usethis} and \code{withr}.
}
}

\section{Changes in version 1.10.0}{
\itemize{
\item Updated the \code{makelist} returned  by \code{get_package_makelist()} to
\itemize{
\item install,
\item dev\_install before running RUnit tests,
\item knit README.md from README.Rmd,
\item rename the target from \code{log/cran_comments.Rout} (which is now the sink) to
\code{cran-comments.md}, as the later \emph{is} the target file.
\item make build depend on file LICENSE,
\item build with vignettes instead of without vignettes.
So now
}\preformatted{fakemake::make("cran-comments.md", get_package_makelist())
}

and\preformatted{system(paste("make -f", system.file("templates", "nomakefile", package = "packager"), "cran-comments.md"))
}

will run identical make chains.
Added internal function \code{compare_make()} to ensure for this.
\item Switched vignette and unit tests from \code{git2r} to \code{gert}, so now \code{git2r} is not
suggested anymore.
\item Added git wrapper \code{git_diff()}.
\item Renamed makelist target from \code{codetags.Rout} to \code{check_codetags.Rout}
according to the function's name.
\item \code{extract_vignette_codes()} now extracts to \verb{inst/vignette_codes/}.
}
}

\section{Changes in version 1.9.0}{
\itemize{
\item Now importing package \code{fritools}.
\item Call package \code{whoami} only if system dependency \code{whoami} is available or the
system running is windows.
\item Switched from \code{git2r} to \code{gert}.
\item Updated the \code{makelist} returned  by \code{get_package_makelist()} to
\itemize{
\item install the current and packager's dependencies and suggested packages.
\item report RUnit testing correctly.
}
\item Added function \code{provide_news_rd()} which will derive file \code{inst/NEWS.rd} from
file \code{NEWS.md}. The former will be shown in the package's help index, so it's
more prominent to people using that index than the latter which will only be
shown by \code{utils::news()} if the latter is not available.
The new function is incorporated in the package's Makelist and it's template
for a Makfile.
}
}

\section{Changes in version 1.8.0}{
\itemize{
\item Added \code{rhub} checks.
New internal functions
\itemize{
\item \code{check_rhub()} to trigger checks on rhub for solaris and windows --
given that you've configured \code{rhub}.
\item \code{get_rhub_latest()} to read summaries from rhub
\item \code{get_local_rhub()} which is called by \code{provide_cran_comments()} to read
output from \code{get_rhub_latest()} that was written to log/rhub.(log|Rout).
So the workflow is
}
\enumerate{
\item trigger rhub checks
\item wait a while
\item read summaries from rhub and save them to log/rhub.(log|Rout)
\item run \code{provide_cran_comments()} which will then incorporate the rhub check log.
}
\item Fixed number of \code{cleanr} issues reported by \code{provide_cran_comments()}.
\item Updated vignette to \code{rasciidoc}, which provides a fancy floating table of contents.
}
}

\section{Changes in version 1.7.0}{
\itemize{
\item Added argument \code{stop_on_devel = TRUE} to \code{submit()} that enables a check on
whether the package has a developement version (that is,
following the \href{https://semver.org/}{semantic versioning} definition of a
pre-release version, the version has a fourth part) and, if so, stops the
submission.
\item Function \code{use_git_check_version_not_tagged()} is now exported.
\item New Function \code{use_git_pre_commit_script} to add git pre-commit scripts and
infrastructure.
}
}

\section{Changes in version 1.6.0}{
\itemize{
\item \code{infect()} now (via \code{use_git_check_version_not_tagged()}) adds a git hook to
prevent from commiting to a package version that is already tagged in git.
\item Added function \code{lint_package} as a wrapper to \code{lintr::lint_package}.
Why? lintr added the \code{cyclocomp_linter} to it's default linters. Which sucks,
because we run cyclocomp independently. And lintr should lint, not check for
cyclomatic complexity. As this might reoccur in future and we don't want to
adapt all our calls to \code{lint_package} excluding crappy linters, this is a
hardcoded wrapper.
\item Now passing \code{build_args} to \code{rcmdcheck::rcmdcheck()}.
\item Changed default value for \code{build_args} from \code{character()} a derivation from
the default value for \code{args} for function \code{rcmdcheck_and_log()}.
So now all \code{args} except "--as-cran" will be used as \code{build_args} by default.
Comes in handy, because usually we want to exclude actions (via "--no-manual"
or the like) from both build and check. Now don't need to pass them to two
arguments explicitely.
}
}

\section{Changes in version 1.5.0}{
\itemize{
\item Added argument \code{args} to \code{rcmdcheck_and_log()} that is passed to
\code{rcmdcheck::rcmdcheck()}.
\item Fix help links to package \code{callr} following
Deepayan Sarkar (see https://deepayan.github.io/tmp/topichelp/funs.html).
\item Fixed \code{get_package_makelist()} to use \code{packager::check_archive()} instead of
\code{check_archive()}.
}
}

\section{Changes in version 1.4.0}{
\itemize{
\item Returned to Rmd vignettes since rasciidoc vignettes fail on CRAN.
}
}

\section{Changes in version 1.3.0}{
\itemize{
\item \code{infect()} now creates man-roxygen/return_invisibly_null.R as a template for
roxygen.
}
}

\section{Changes in version 1.2.1}{
\itemize{
\item Internal function \code{convert_package_vignettes()} now works for multiple Rmd
files.
\item Fixed package creation broken by missing package \code{rasciidoc} in field Suggests
of file DESCRIPTION.
\item \code{submit()} now tells you to use a developement version after submission.
}
}

\section{Changes in version 1.2.0}{
\itemize{
\item Now using rasciidoc vignettes.
Pass \code{use_rasciidoc_vignette = FALSE} to \code{create()} or \code{infect()} to stick
with the original rmarkdown vignette.
\item Added an function internal function \code{extract_vignette_codes()} to extract R
code from different vignettes.
}
}

\section{Changes in version 1.1.0}{
\itemize{
\item provide\_cran\_comments() now reports changes read from NEWS.md that may
contain sections.
\item provide\_cran\_comments() now reports the number of checks run by RUnit.
}
}

\section{Changes in version 1.0.0}{
\itemize{
\item Removed reading travis.com logs.
\item Only use files starting with "runit" for RUnit testing.
\item Allow for file setup.R for RUnit testing.
\item Function use\_template() now optionally adds and commits files to disk an is
exported now.
\item Function provide\_make() now passes ellpsis to use\_template().
\item Fixed cleanr Issue #1: RUnit test now no longer write results to disk on their own.
\item Comments to CRAN now report package meta stats:
The package is searched for files in ./log/ that correspond to cyclocomp,
spell checking, cleanr, lintr and usage.
\item Comments to CRAN now report unit testings stats:
The package is searched for files in ./log/ that correspond to RUnit, testthat
and covr.
\item Added option is\_cran to get\_package\_makelist() to omit targets \code{cyclocomp}
and \code{runit} instead of option is\_runit.
\item Pass upload error messages if package submission fails.
\item New function sort\_deps\_in\_desc() sorts the dependencies in file DESCRIPTION.
\item release() is now linked to submit() which is the better name.
\item Added a function get\_check\_status() that retrieves the status from a \verb{R CMD check}-log.
\item Now using package fakemake in infect().
}
}

\section{Changes in version 0.22.4}{
\itemize{
\item use packager's version of use\_dev\_version().
\item Fix infect() for new behaviour of usethis.
}
}

\section{Changes in version 0.22.3}{
\itemize{
\item Fix create() for new behaviour of usethis.
}
}

\section{Changes in version 0.22.2}{
\itemize{
\item Added remove\_lines().
\item Added pkgload to Field Suggests in file DESCRIPTION.
}
}

\section{Changes in version 0.22.1}{
\itemize{
\item Fixed linking to vignettes on gitlab.
}
}

\section{Changes in version 0.22.0}{
\itemize{
\item Force to use usethis version 1.4.0, as usethis version 1.5.0 breaks loads of
stuff.
\item Added function install\_deps(). Installs dependencies from DESCRIPTION with
minimum version.
\item Save the install log, too.
\item Provide template for link to vignette on CRAN.
\item Fixed link to vignette on gitlab.
\item Prevent R/PKGNAME-package.R from being overwritten by default.
\item Use new cleanr::check\_package() instead of cleanr::check\_directory()
\item Got rid of github\_document in README.
}
}

\section{Changes in version 0.21.0}{
\itemize{
\item Fixed link to vignette on gitlab.
}
}

\section{Changes in version 0.20.0}{
\itemize{
\item Caught possibly failing get\_gitlab\_log as to make provide\_cran\_comments()
more stable.
\item Fix devel.R template.
}
}

\section{Changes in version 0.19.0}{
\itemize{
\item Create a devel.R from the template on infection.
\item Not using deprecated packager::use\_dev\_version() in Makefile.
\item Using path with pkgbuild::build() instead of pkg from devtools.
}
}

\section{Changes in version 0.18.2}{
\itemize{
\item Fixed use\_dev\_version.
}
}

\section{Changes in version 0.18.1}{
\itemize{
\item Did not fix use\_dev\_version.
}
}

\section{Changes in version 0.18.0}{
\itemize{
\item Adapted to devtools 2.0.0 (using pkgbuild, pkgload, remotes, rcmdcheck,
usethis where appropriate).
\item Added personal options on load.
\item Enhanced template for devel.R
}
}

\section{Changes in version 0.17.0}{
\itemize{
\item Provided minimal example in vignette.
\item Hardened eval\_from\_log() against logging output like XXX = <environmend> and
\verb{\\r}.
\item Fixed bug in print\_lints().
\item Do not call add\_github\_url\_to\_desc() any more.
\item Fixed default for argument details to internal function use\_intro().
\item Use devtools::upload\_cran() from version 1.13.6.
}
}

\section{Changes in version 0.16.0}{
\itemize{
\item provide\_cran\_comments() now reads info from logs on gitlab.com, given that
.gitlab-ci.yml from this package (via packager:::use_gitlab_ci()) is used.
}
}

\section{Changes in version 0.15.3}{
\itemize{
\item Roxygenized the examples from 0.15.2.
}
}

\section{Changes in version 0.15.2}{
\itemize{
\item Hardboiled examples for invect() against missing git default config.
}
}

\section{Changes in version 0.15.1}{
\itemize{
\item Fixed broken pipeline by hardening provide\_gitlab\_url() against missing git
default config.
}
}

\section{Changes in version 0.15.0}{
\itemize{
\item Added new functions set\_desc\_url(), which, using provide\_gitlab\_url(),
sets the DESCRIPTION's URL to a hopefully reasonable URL when running
infect().
\item Added new functions is\_r\_package() and provide\_gitlab\_url().
}
}

\section{Changes in version 0.14.0}{
\itemize{
\item Fixed for using gitlab instead of github.
}
}

\section{Changes in version 0.13.0}{
\itemize{
\item Added fixed for git2r: Stefan Widgren switched from S4 back to S3.
\item Added codetools to DESCRIPTION.
}
}

\section{Changes in version 0.12.0}{
\itemize{
\item Added function check\_usage() as a wrapper to codetools::checkUsagePackage().
}
}

\section{Changes in version 0.11.2}{
\itemize{
\item Added auto git commit of changed devel NEWS file.
}
}

\section{Changes in version 0.11.1}{
\itemize{
\item Fix Makefile
}
}

\section{Changes in version 0.11.0}{
\itemize{
\item Added functions use\_dev\_version() and use\_dev\_news().
}
}

\section{Changes in version 0.10.1}{
\itemize{
\item Using names for old tags in tagging.
}
}

\section{Changes in version 0.10.0}{
\itemize{
\item Added update\_deps() for updating package dependencies like internals from
package remotes does.
\item provide\_cran\_comments() now optionally reads a travis log from file.
\item Fixed git commit in release().
}
}

\section{Changes in version 0.9.0}{
\itemize{
\item Added function print\_lints() to print lints sorted by patterns
matching source file names.
}
}

\section{Changes in version 0.8.0}{
\itemize{
\item Setting initial package version to '0.1.0'.
\item Using the minor R version in DESCRIPTION (not the patched one).
\item Fixed setting a package's title if no description is given.
\item Set the argument author\_at\_r for function set\_package\_info to default to
option packager/whoami.
\item Add a failsafe version of git2r::commit called git\_commit().
\item Added exception handling if reading the git config throws an error by
conditionally setting a local git config.
\item Added option \code{verbose} to create().
\item Sanitized the return value of git\_sync\_status().
\item Linted the codes heavily.
\item Added an inclusion pattern to check\_codetags() and set reasonable defaults
for patterns.
}
}

\section{Changes in version 0.7.0}{
\itemize{
\item Added function release() which skips the usual interactive questions done
by devtools::release().
\item Added function provide\_make\_list() which is an extension to
fakemake::provide\_make\_list().
\item Enhanced docs for provide\_cran\_comments().
}
}

\section{Changes in version 0.6.0}{
\itemize{
\item Wrapped the travis-cli interface into tryCatch to be able to use my Makefile as
template on systems where travis-cli will fail.
}
}

\section{Changes in version 0.5.0}{
\itemize{
\item Fixed testing.
\item Added internal function strip\_off\_attributes(), mainly to get rid of object
names.
\item Fixed querying the package's maintainer's name.
\item Updated package's info.
}
}

\section{Changes in version 0.4.1}{
\itemize{
\item Fixed adding github url to DESCRIPTION.
}
}

\section{Changes in version 0.4.0}{
\itemize{
\item Hotfixed git\_tag()
\item Added internal function git\_add\_commit() to mimic \verb{git commit -am"MESSAGE"}.
}
}

\section{Changes in version 0.3.1}{
\itemize{
\item provide\_cran\_comments(name = ) now defaults to NA, using the DESCRIPTION's
maintainer's given name.
}
}

\section{Changes in version 0.3.0}{
\itemize{
\item Resetting RUnit tests.
}
}

\section{Changes in version 0.2.2}{
\itemize{
\item Vignette defaults are now set from details and description passed to
set\_package\_info.
}
}

\section{Changes in version 0.2.1}{
\itemize{
\item Added bugfix for get\_news().
}
}

\section{Changes in version 0.2.0}{
\itemize{
\item Added function to add github url to DESCRIPTION.
}
}

\section{Changes in version 0.1.0}{
\itemize{
\item Added a \code{NEWS.md} file to track changes to the package.
}
}

