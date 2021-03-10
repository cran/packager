if (interactive()) pkgload::load_all(".")

# see R/git.R: git_tag_create()
run_test <- fritools::is_running_on_fvafrcu_machines()

test_repo <- function() {
    d <- tempfile()
    dir.create(d)
    RUnit::checkTrue(!packager:::is_git_repository(d))
    RUnit::checkTrue(!packager:::is_git_clone(d))
    RUnit::checkTrue(!packager:::uses_git(d))
    gert::git_init(d)
    RUnit::checkTrue(packager:::is_git_repository(d))
    RUnit::checkTrue(packager:::is_git_clone(d))
    RUnit::checkTrue(packager:::uses_git(d))

    RUnit::checkTrue(!packager:::is_git_uncommitted(d))
    fritools::touch(file.path(d, "foobar.txt"))
    result <- packager:::git_add(path = d, files = "foobar.txt")
    RUnit::checkTrue(is(result, "tbl"))
    result <- packager:::git_commit(d, message = "ini")
    RUnit::checkTrue(is(result, "character"))
    fritools::touch(file.path(d, "foobaz.txt"))
    result <- git_add_commit(d, message = "sec", untracked = TRUE)
    RUnit::checkTrue(is(result, "character"))

}
if (interactive()) test_repo()

test_git_tag_create <- function() {

    if (run_test) {
        path <- file.path(tempdir(), "prutp")
        on.exit(unlink(path, recursive = TRUE))
        packager:::package_skeleton(path)

        # no repo
        RUnit::checkException(packager:::git_tag_create(path = path,
                                                        version = "0.0.0",
                                                        message = "Foo"))

        # initial repo
        packager:::use_git(path)
        result <- packager:::git_tag_create(path = path,
                                            version = "0.0.0.9000",
                                            message = "Initial Commit")

        RUnit::checkIdentical("0.0.0.9000", getElement(result, "name"))
        desc::desc_bump_version("minor", file = path)

        # uncommitted changes
        RUnit::checkException(packager::git_tag(path = path))
    }
}
if (interactive()) test_git_tag_create()

test_git_tag <- function() {
    if (run_test) {
        path <- file.path(tempdir(), "prutp")
        on.exit(unlink(path, recursive = TRUE))
        packager:::package_skeleton(path)

        # no repo
        RUnit::checkException(packager::git_tag(path = path))

        # initial repo
        packager:::use_git(path)
        result <- packager::git_tag(path = path)
        RUnit::checkIdentical("1.0", getElement(result, "name"))
        desc::desc_bump_version("minor", file = path)

        # uncommitted changes
        RUnit::checkException(packager::git_tag(path = path))

        # commited changes
        git_add_commit(path = path)
        result <- packager::git_tag(path = path)
        RUnit::checkIdentical("1.1", getElement(result, "name"))

        # version number lower than in tags
        desc::desc_set(Version = "0.3", file = path)
        git_add_commit(path = path)
        RUnit::checkException(packager::git_tag(path = path))
    }
}

if (interactive()) test_git_tag()
