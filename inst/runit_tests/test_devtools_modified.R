if (interactive()) pkgload::load_all(".")

test_union_write <- function() {
    path <- file.path(tempfile())
    on.exit(unlink(path))
    lines <- c("foo", "bar")
    expectation <- lines
    packager:::union_write(path, lines)
    result <- readLines(path)
    RUnit::checkIdentical(result, expectation)
    line <- "foo bar"
    expectation <- c(lines, line)
    packager:::union_write(path, line)
    result <- readLines(path)
    RUnit::checkIdentical(result, expectation)
}

test_use_git_ignore <- function() {
    expectation <- TRUE
    path <- file.path(tempdir(), "prutp")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    gert::git_init(path)
    ignores <- c("foo", "bar")
    result <- packager:::use_git_ignore(path = path, ignores = ignores)
    RUnit::checkIdentical(result, expectation)

    expectation <- ignores
    result  <- readLines(file.path(path, ".gitignore"))
    RUnit::checkIdentical(result, expectation)

}

test_use_git <- function() {
    path <- file.path(tempdir(), "prutp")
    expectation <- path
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    result <- packager:::use_git(path = path)
    RUnit::checkIdentical(result, expectation)

    # rerun on intialised git repo
    expectation <- NULL
    result <- packager:::use_git(path = path)
    RUnit::checkIdentical(result, expectation)
}
