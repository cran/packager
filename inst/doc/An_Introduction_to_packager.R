## ----results = "hide", message = FALSE----------------------------------------
path <- file.path(tempdir(), "myFirstPackage")
packager::create(path, fakemake = "check")


## -----------------------------------------------------------------------------
list.files(path, recursive = FALSE)
gert::git_status(repo = path)
gert::git_log(repo = path)


## -----------------------------------------------------------------------------
cat(readLines(file.path(path, "log", "spell.Rout")), sep = "\n")
tail(readLines(file.path(path, "log", "check.Rout")), sep = "\n")


## -----------------------------------------------------------------------------
cat(readLines(file.path(path, "TODO.md")), sep = "\n")


## -----------------------------------------------------------------------------
cat(readLines(file.path(path, "DESCRIPTION")), sep = "\n")


## -----------------------------------------------------------------------------
unlink(path, recursive = TRUE)
if ("myFirstPackage" %in% .packages()) detach("package:myFirstPackage", 
                                              unload = TRUE)


## ----results = "hide", message = FALSE----------------------------------------
package_title <- "myOtherPackage"
path <- file.path(tempdir(), package_title)
a  <- utils::person(given = "Your", family = "Name", email = "some@whe.re", 
                    role = c("aut", "cre"))
packager::create(path, author_at_r = a, title = package_title,
                 description = "This is very important.",
                 details = "At least to me.", fakemake = "roxygen2")

## -----------------------------------------------------------------------------
cat(readLines(file.path(path, "DESCRIPTION")), sep = "\n")


## ----eval = FALSE-------------------------------------------------------------
# pkgload::load_all(path)
# help(paste0(package_title, "-package"))

## ----echo = FALSE-------------------------------------------------------------
pkgload::load_all(path)
# insert developement page
help_file <-  system.file("man", paste0(package_title, "-package.Rd"), 
                          package = devtools::as.package(path)$package)
captured <- gsub('_\b', '',  capture.output(tools:::Rd2txt(help_file) ))
cat(captured, sep = "\n")


## ----eval = FALSE-------------------------------------------------------------
# adc <- utils::person(given = "Andreas Dominik",
#                       family = "Cullmann",
#                       email = "fvafrcu@mailbox.org",
#                       role = c("aut", "cre"))
# pop <- as.list(getOption("packager"))
# pop[["whoami"]] <- adc
# options(packager = pop)
# 


## -----------------------------------------------------------------------------
gert::git_status(repo = path)
gert::git_log(repo = path)


## -----------------------------------------------------------------------------
list.files(file.path(path, "log"))


## -----------------------------------------------------------------------------
ml <- packager::get_package_makelist(is_cran = TRUE)
cbind(lapply(ml, function(x) x[["target"]]),
      lapply(ml, function(x) x[["alias"]]))


## -----------------------------------------------------------------------------
suppressMessages(withr::with_dir(path, 
                                  print(fakemake::make("build", ml, 
                                                       verbose = FALSE))))


## -----------------------------------------------------------------------------
gert::git_status(repo = path)
packager::git_diff(x = ".Rbuildignore", path = path)


## -----------------------------------------------------------------------------
withr::with_dir(path, packager::git_add_commit(path = ".", untracked = TRUE,
                                               message = "make build"))
gert::git_status(repo = path)


## -----------------------------------------------------------------------------
suppressMessages(withr::with_dir(path, 
                                 print(fakemake::make("check", ml, 
                                                      verbose = FALSE))))


## -----------------------------------------------------------------------------
gert::git_status(repo = path)


## -----------------------------------------------------------------------------
cat(tail(readLines(file.path(path, "log", "check.Rout")), n = 7), sep = "\n")
check_log <- file.path(path, "log", "check.Rout")
status <- packager::get_check_status(check_log)
RUnit::checkEqualsNumeric(status[["status"]][["errors"]], 0)


## -----------------------------------------------------------------------------
withr::with_dir(path, packager::git_add_commit(path = ".", untracked = TRUE,
                                               message = "make check"))


## -----------------------------------------------------------------------------
system.time(withr::with_dir(path, print(fakemake::make("check", ml, verbose = FALSE))))


## -----------------------------------------------------------------------------
try(packager::submit(path))


## -----------------------------------------------------------------------------
packager::git_tag(path = path, message = "A Tag")
packager::use_dev_version(path = path)
desc::desc_get("Version", file = path)
cat(readLines(file.path(path, "NEWS.md")), sep = "\n")



## ----echo = FALSE-------------------------------------------------------------
# remove the package
unlink(path, recursive = TRUE)

