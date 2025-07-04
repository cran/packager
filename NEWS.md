# packager 1.15.3

* Adapted `lintr::indentation_linter()` in `lint_package()` to my vim-settings.
* Fixed CRAN notes on lost braces.

# packager 1.15.2

* Fixed CRAN notes on Escaped LaTeX specials.
* Fix `provide_news_rd()` to deal with escaped underscores.

# packager 1.15.1

* Fix tests failing for R-devel on several platforms.

# packager 1.15.0

* Package `fritools` back on CRAN.
* Updated rhub logs.
* Read win\_builder message from ~/Mail/CRAN for cran\_comments.md.
* Adapted to lintr 3.0.0.
* Fixed retrival of gitlab logs for camelCase packages.
* Fixed formerly extraneous argument `character.only` to call to
  `requireNamespase()` in `build_manual()`, which always led to falsely never
  roxygenizing.
* `use_devel()` now provides an exenteded `devel.R` and adds 
  `devel/devel_test.R`, which I found very helpful.

# packager 1.14.0

* Added function `build_manual` as a substitute for devtools' version.
* `submit()` now has an argument `consider_tracked` to control exception
  handling if git status is not clean.
* tinytest now starts with a reasonable test file testing for
  `throw()` to throw an error. 

# packager 1.13.0

* Added output from `log/tinytest.log` to cran-comments.md.
* Local test result get written to file `cran-comments.md` only if the results
  read from the file are not empty. 
  This fixes changing output from testthat causing empty results as well as
  empty log files (for example when tinytest.R is not present target tinytest is
  made anyway).
* Now adding `tinytest` to infected packages.


# packager 1.12.0

* Adapted for exception handling from fritools.

# packager 1.11.1

* Fixed missing codes in vignette.

# packager 1.11.0

* Gitlab logs for cran-comments.md are now checked for the appropriate version.
* Added more templates for man-roxygen.
* Devel versions for `check_news()` only match first order sections (lines
  starting with `# `). So we can keep documentation of changes in devel versions
  in the subsections.
* Substituted imports from `usethis` and `withr`.

# packager 1.10.0

* Updated the `makelist` returned  by `get_package_makelist()` to 
    - install,
    - dev\_install before running RUnit tests,
    - knit README.md from README.Rmd,
    - rename the target from `log/cran_comments.Rout` (which is now the sink) to
     `cran-comments.md`, as the later _is_ the target file.
    - make build depend on file LICENSE,
    - build with vignettes instead of without vignettes.
  So now 
  ```
  fakemake::make("cran-comments.md", get_package_makelist())
  ```
  and
  ```
  system(paste("make -f", system.file("templates", "nomakefile", package = "packager"), "cran-comments.md"))
  ```
  
  will run identical make chains.
  Added internal function `compare_make()` to ensure for this.
* Switched vignette and unit tests from `git2r` to `gert`, so now `git2r` is not
  suggested anymore.
* Added git wrapper `git_diff()`.
* Renamed makelist target from `codetags.Rout` to `check_codetags.Rout`
  according to the function's name.
* `extract_vignette_codes()` now extracts to `inst/vignette_codes/`.

# packager 1.9.0

* Now importing package `fritools`.
* Call package `whoami` only if system dependency `whoami` is available or the
  system running is windows.
* Switched from `git2r` to `gert`.
* Updated the `makelist` returned  by `get_package_makelist()` to 
    - install the current and packager's dependencies and suggested packages. 
    - report RUnit testing correctly.
* Added function `provide_news_rd()` which will derive file `inst/NEWS.rd` from
  file `NEWS.md`. The former will be shown in the package's help index, so it's
  more prominent to people using that index than the latter which will only be
  shown by `utils::news()` if the latter is not available.
  The new function is incorporated in the package's Makelist and it's template
  for a Makfile.

# packager 1.8.0
* Added `rhub` checks.  
  New internal functions
    - `check_rhub()` to trigger checks on rhub for solaris and windows --
       given that you've configured `rhub`.
    - `get_rhub_latest()` to read summaries from rhub
    - `get_local_rhub()` which is called by `provide_cran_comments()` to read 
       output from `get_rhub_latest()` that was written to log/rhub.(log|Rout).  
    So the workflow is
    1. trigger rhub checks
    1. wait a while
    1. read summaries from rhub and save them to log/rhub.(log|Rout)
    1. run `provide_cran_comments()` which will then incorporate the rhub check log.
* Fixed number of `cleanr` issues reported by `provide_cran_comments()`.
* Updated vignette to `rasciidoc`, which provides a fancy floating table of contents.

# packager 1.7.0

* Added argument `stop_on_devel = TRUE` to `submit()` that enables a check on 
  whether the package has a developement version (that is, 
  following the [semantic versioning](https://semver.org/) definition of a 
  pre-release version, the version has a fourth part) and, if so, stops the 
  submission. 
* Function `use_git_check_version_not_tagged()` is now exported.
* New Function `use_git_pre_commit_script` to add git pre-commit scripts and
  infrastructure.

# packager 1.6.0

* `infect()` now (via `use_git_check_version_not_tagged()`) adds a git hook to
  prevent from commiting to a package version that is already tagged in git.
* Added function `lint_package` as a wrapper to `lintr::lint_package`.
  Why? lintr added the `cyclocomp_linter` to it's default linters. Which sucks,
  because we run cyclocomp independently. And lintr should lint, not check for
  cyclomatic complexity. As this might reoccur in future and we don't want to
  adapt all our calls to `lint_package` excluding crappy linters, this is a
  hardcoded wrapper.
* Now passing `build_args` to `rcmdcheck::rcmdcheck()`.
* Changed default value for `build_args` from `character()` a derivation from
  the default value for `args` for function `rcmdcheck_and_log()`.
  So now all `args` except "--as-cran" will be used as `build_args` by default.
  Comes in handy, because usually we want to exclude actions (via "--no-manual" 
  or the like) from both build and check. Now don't need to pass them to two
  arguments explicitly.


# packager 1.5.0

* Added argument `args` to `rcmdcheck_and_log()` that is passed to
  `rcmdcheck::rcmdcheck()`.
* Fix help links to package `callr` following 
  Deepayan Sarkar (see https://deepayan.github.io/tmp/topichelp/funs.html).
* Fixed `get_package_makelist()` to use `packager::check_archive()` instead of 
  `check_archive()`.

# packager 1.4.0

* Returned to Rmd vignettes since rasciidoc vignettes fail on CRAN.

# packager 1.3.0

* `infect()` now creates man-roxygen/return_invisibly_null.R as a template for
  roxygen.

# packager 1.2.1

* Internal function `convert_package_vignettes()` now works for multiple Rmd
  files.
* Fixed package creation broken by missing package `rasciidoc` in field Suggests
  of file DESCRIPTION.
* `submit()` now tells you to use a developement version after submission.

# packager 1.2.0

* Now using rasciidoc vignettes.
  Pass `use_rasciidoc_vignette = FALSE` to `create()` or `infect()` to stick
  with the original rmarkdown vignette.
* Added an function internal function `extract_vignette_codes()` to extract R
  code from different vignettes.

# packager 1.1.0

* provide\_cran\_comments() now reports changes read from NEWS.md that may
  contain sections.
* provide\_cran\_comments() now reports the number of checks run by RUnit.

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
* Hardened `eval_from_log()` against logging output like `XXX = <environment>`
  and `\r`.
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

* Wrapped the travis-cli interface into tryCatch to be able to use my Makefile as
  template on systems where travis-cli will fail.

# packager 0.5.0

- Fixed testing.
- Added internal function strip\_off\_attributes(), mainly to get rid of object 
names.
- Fixed querying the package's maintainer's name.
- Updated package's info.


# packager 0.4.1

* Fixed adding github url to DESCRIPTION.

# packager 0.4.0

- Hotfixed git\_tag()
- Added internal function git\_add\_commit() to mimic `git commit -am"MESSAGE"`.

# packager 0.3.1

* provide\_cran\_comments(name = ) now defaults to NA, using the DESCRIPTION's
 maintainer's given name.

# packager 0.3.0

* Resetting RUnit tests.

# packager 0.2.2

* Vignette defaults are now set from details and description passed to 
 set\_package\_info.

# packager 0.2.1

* Added bugfix for get\_news().

# packager 0.2.0

* Added function to add github url to DESCRIPTION.

# packager 0.1.0

* Added a `NEWS.md` file to track changes to the package.

