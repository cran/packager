if (interactive()) pkgload::load_all(".")

provide_fake_package <- function() {
    path <- file.path(tempdir(), "prutp")
    tryCatch(suppressMessages(usethis::create_package(path, open = FALSE)),
                              error = identity
    )
    return(path)
}

test_is_null_or_true <- function() {
    result <- packager:::is_null_or_true(NULL)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true(TRUE)
    RUnit::checkTrue(result)
    result <- packager:::is_null_or_true("foobar")
    RUnit::checkTrue(! result)
}

test_is_force <- function() {
    result <- packager:::is_force()
    RUnit::checkTrue(result)
}

test_get_news <- function() {
    path <- provide_fake_package() # Exclude Linting  
    on.exit(unlink(path, recursive = TRUE))
    withr::with_dir(path, usethis::use_news_md(open = FALSE))
    result <- packager:::get_news(path)
    expectation <-
        "\n* Added a `NEWS.md` file to track changes to the package."
    RUnit::checkIdentical(result, expectation)
}

test_git <- function() {
    path <- provide_fake_package() # Exclude Linting  
    on.exit(unlink(path, recursive = TRUE))
    RUnit::checkTrue(! packager:::is_git_clone(path),
                          msg = "Not a git repo.")
    RUnit::checkException(packager:::is_git_uncommitted(path),
                          msg = "Not a git repo, no commits.")
    packager:::use_git(path = path)
    RUnit::checkTrue(packager:::is_git_clone(path))
    RUnit::checkTrue(! packager:::is_git_uncommitted(path),
                     msg = "All should be commited.")
    cat("foo", file = file.path(path, "DESCRIPTION"), append = TRUE)
    RUnit::checkTrue(packager:::is_git_uncommitted(path),
                     msg = "Uncommited changes.")
}

test_warn_and_stop <- function()
    RUnit::checkException(packager:::warn_and_stop("foo"))


test_url <- function() {
    #% get_remote_url
    path <- file.path(tempdir(), "prutp")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    repo <- git2r::init(path)
    git2r::remote_add(repo, "github", "https://github.com/fvafrCU/prutp")
    git2r::remote_add(repo, "local", "./some_path/git/prutp")
    ##% package's url
    expectation <- "https://github.com/fvafrCU/prutp"
    result <- grep(value = TRUE, "github", packager:::get_remote_url(path))
    RUnit::checkIdentical(result, expectation)

    #% get_git_url
    ##% no such url
    expectation <- NULL
    url <- packager:::get_remote_url(path = tempdir())
    result <- packager:::get_git_url(url)
    RUnit::checkIdentical(result, expectation)

    ##% no github url
    expectation <- NULL
    url <- packager:::get_remote_url(path)
    x <- grep(value = TRUE, "github", url, invert = TRUE)
    result <- packager:::get_git_url(x, type = "github")
    RUnit::checkIdentical(result, expectation)

    ##% package's url
    expectation <- "https://github.com/fvafrCU/prutp"
    url <- packager:::get_remote_url(path)
    result <- packager:::get_git_url(url, type = "github")
    RUnit::checkIdentical(result, expectation)

    ##% multiple url
    git2r::remote_add(repo, "github1", "https://github.com/fvafrCU/fakepackage")

    ###% return all url
    expectation <- c("https://github.com/fvafrCU/prutp",
                     "https://github.com/fvafrCU/fakepackage")
    url <- packager:::get_remote_url(path)
    result <- packager:::get_git_url(url, type = "github")
    RUnit::checkIdentical(result, expectation)

    ###% return first url
    expectation <- c("https://github.com/fvafrCU/prutp")
    url <- packager:::get_remote_url(path)
    result <- packager:::get_git_url(url, return_only_one = TRUE,
                                     type = "github")
    RUnit::checkIdentical(result, expectation)

    ###% throw on multiple
    RUnit::checkException(packager:::get_git_url(url, return_only_one = TRUE,
                                                 force = FALSE,
                                                 type = "github"))
}


test_travis <- function() {
    path <- file.path(tempdir(), "prutp")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    repo <- git2r::init(path)
    git2r::remote_add(repo, "github", "https://github.com/fvafrCU/packager")
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        RUnit::checkException(packager:::travis_cli(path))
    }
}
