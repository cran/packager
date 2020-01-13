# packager 1.0.0

* Removed reading travis.com logs.
* Only use files starting with "runit" for RUnit testing.
* Allow for file setup.R for RUnit testing. 
* Function use\_template() now optionally adds and commits files to disk an is
  exported now.
* Function provide\_make() now passes ellpsis to use\_template().
* Fixed cleanr Issue #1: RUnit test now no longer write results to disk on their own.
* Comments to CRAN now report package meta stats:
  The package is searched for files in ./log/ that correspond to cyclocomp,
  spell checking, cleanr, lintr and usage.
* Comments to CRAN now report unit testings stats:
  The package is searched for files in ./log/ that correspond to RUnit, testthat
  and covr.
* Added option is\_cran to get\_package\_makelist() to omit targets `cyclocomp`
  and `runit` instead of option is\_runit.
* Pass upload error messages if package submission fails.
* New function sort\_deps\_in\_desc() sorts the dependencies in file DESCRIPTION.
* release() is now linked to submit() which is the better name.
* Added a function get\_check\_status() that retrieves the status from a `R CMD
  check`-log.
* Now using package fakemake in infect().

# packager 0.22.4

* use packager's version of use\_dev\_version().
* Fix infect() for new behaviour of usethis.

# packager 0.22.3

* Fix create() for new behaviour of usethis.

# packager 0.22.2

* Added remove\_lines().
* Added pkgload to Field Suggests in file DESCRIPTION.

# packager 0.22.1

* Fixed linking to vignettes on gitlab.

# packager 0.22.0

* Force to use usethis version 1.4.0, as usethis version 1.5.0 breaks loads of
  stuff.
* Added function install\_deps(). Installs dependencies from DESCRIPTION with
  minimum version.
* Save the install log, too.
* Provide template for link to vignette on CRAN.
* Fixed link to vignette on gitlab.
* Prevent R/PKGNAME-package.R from being overwritten by default. 
* Use new cleanr::check\_package() instead of cleanr::check\_directory()
* Got rid of github\_document in README.

# packager 0.21.0

* Fixed link to vignette on gitlab.

# packager 0.20.0

* Caught possibly failing get\_gitlab\_log as to make provide\_cran\_comments() 
  more stable.
* Fix devel.R template.

# packager 0.19.0

* Create a devel.R from the template on infection.
* Not using deprecated packager::use\_dev\_version() in Makefile.
* Using path with pkgbuild::build() instead of pkg from devtools.

# packager 0.18.2

* Fixed use\_dev\_version.

# packager 0.18.1

* Did not fix use\_dev\_version.

# packager 0.18.0

* Adapted to devtools 2.0.0 (using pkgbuild, pkgload, remotes, rcmdcheck, 
  usethis where appropriate).
* Added personal options on load.
* Enhanced template for devel.R

# packager 0.17.0

* Provided minimal example in vignette.
* Hardened eval\_from\_log() against logging output like XXX = <environmend> and
  `\r`.
* Fixed bug in print\_lints().
* Do not call add\_github\_url\_to\_desc() any more.
* Fixed default for argument details to internal function use\_intro().
* Use devtools::upload\_cran() from version 1.13.6.

# packager 0.16.0

* provide\_cran\_comments() now reads info from logs on gitlab.com, given that
  .gitlab-ci.yml from this package (via packager:::use_gitlab_ci()) is used.

# packager 0.15.3

* Roxygenized the examples from 0.15.2.

# packager 0.15.2

* Hardboiled examples for invect() against missing git default config.


# packager 0.15.1

* Fixed broken pipeline by hardening provide\_gitlab\_url() against missing git
  default config.

# packager 0.15.0

* Added new functions set\_desc\_url(), which, using provide\_gitlab\_url(),
  sets the DESCRIPTION's URL to a hopefully reasonable URL when running 
  infect().
* Added new functions is\_r\_package() and provide\_gitlab\_url().

# packager 0.14.0

* Fixed for using gitlab instead of github.

# packager 0.13.0

* Added fixed for git2r: Stefan Widgren switched from S4 back to S3.
* Added codetools to DESCRIPTION.

# packager 0.12.0

* Added function check\_usage() as a wrapper to codetools::checkUsagePackage().

# packager 0.11.2

* Added auto git commit of changed devel NEWS file.

# packager 0.11.1

* Fix Makefile

# packager 0.11.0

* Added functions use\_dev\_version() and use\_dev\_news().

# packager 0.10.1

* Using names for old tags in tagging.

# packager 0.10.0

* Added update\_deps() for updating package dependencies like internals from
  package remotes does.
* provide\_cran\_comments() now optionally reads a travis log from file.
* Fixed git commit in release().

# packager 0.9.0
* Added function print\_lints() to print lints sorted by patterns
  matching source file names.


# packager 0.8.0

* Setting initial package version to '0.1.0'.
* Using the minor R version in DESCRIPTION (not the patched one).
* Fixed setting a package's title if no description is given.
* Set the argument author\_at\_r for function set\_package\_info to default to 
  option packager/whoami.
* Add a failsafe version of git2r::commit called git\_commit().
* Added exception handling if reading the git config throws an error by
  conditionally setting a local git config.
* Added option `verbose` to create().
* Sanitized the return value of git\_sync\_status().
* Linted the codes heavily.
* Added an inclusion pattern to check\_codetags() and set reasonable defaults
  for patterns.

# packager 0.7.0

* Added function release() which skips the usual interactive questions done
  by devtools::release().
* Added function provide\_make\_list() which is an extension to 
  fakemake::provide\_make\_list().
* Enhanced docs for provide\_cran\_comments().

# packager 0.6.0

Wrapped the travis-cli interface into tryCatch to be able to use my Makefile as
template on systems where travis-cli will fail.

# packager 0.5.0

- Fixed testing.
- Added internal function strip\_off\_attributes(), mainly to get rid of object 
names.
- Fixed querying the package's maintainer's name.
- Updated package's info.


# packager 0.4.1

Fixed adding github url to DESCRIPTION.

# packager 0.4.0

- Hotfixed git\_tag()
- Added internal function git\_add\_commit() to mimic `git commit -am"MESSAGE"`.

# packager 0.3.1

provide\_cran\_comments(name = ) now defaults to NA, using the DESCRIPTION's
maintainer's given name.

# packager 0.3.0

Resetting RUnit tests.

# packager 0.2.2

Vignette defaults are now set from details and description passed to 
set\_package\_info.

# packager 0.2.1

Added bugfix for get\_news().

# packager 0.2.0

Added function to add github url to DESCRIPTION.

# packager 0.1.0

* Added a `NEWS.md` file to track changes to the package.
