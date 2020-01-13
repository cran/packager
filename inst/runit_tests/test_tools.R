if (interactive()) pkgload::load_all(".")

test_add_commit <- function() {
    path <- tempfile()
    dir.create(path)
    r <- git2r::init(path)
    writeLines("hello, world!", file.path(path, "f"))
    expectation <- structure(list(staged = structure(list(),
                                                     .Names = character(0)),
                                  unstaged = structure(list(),
                                                       .Names = character(0)),
                                  untracked = structure(list(untracked = "f"),
                                                        .Names = "untracked")),
                             .Names = c("staged", "unstaged", "untracked"),
                             class = "git_status")
    result <- git2r::status(r)
    RUnit::checkIdentical(expectation, result)
    RUnit::checkException(git_add_commit(path = path))
    git_add_commit(path = path, untracked = TRUE)
    expectation <- structure(list(staged = structure(list(),
                                                    .Names = character(0)),
                                 unstaged = structure(list(),
                                                      .Names = character(0)),
                                 untracked = structure(list(),
                                                       .Names = character(0))),
                            .Names = c("staged", "unstaged", "untracked"),
                            class = "git_status")
    result <- git2r::status(r)
    RUnit::checkIdentical(expectation, result)
}

provide_fake_package <- function() {
    path <- file.path(tempdir(), "prutp")
    tryCatch(suppressMessages(usethis::create_package(path, quiet = TRUE)),
                              error = identity
    )
    return(path)
}


notest_cyclocomp <- function() {
    # somehow fails on install...
    path <- file.path(tempdir(), "prutp")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path)
    result <- packager::check_cyclomatic_complexity(path)
    # news too new
    news_file <- file.path(path, "NEWS.md")
    writeLines(c("# prutp 0.1.1", new), news_file)
    RUnit::checkTrue(packager::check_news(path))


    RUnit::checkTrue(result)
    pccc <- packager::check_cyclomatic_complexity
    RUnit::checkException(pccc(path, max_complexity = 0))
}

test_news <- function() {
    path <- file.path(tempdir(), "prutp")
    on.exit(unlink(path, recursive = TRUE))
    packager::create(path, fakemake = FALSE)
    result <- packager::check_news(path)
    RUnit::checkTrue(result)

    # usethis auto-updates NEWS
    packager::use_dev_version(path)
    RUnit::checkTrue(packager::check_news(path))

    # Bump version
    desc::desc_bump_version(path, which = "minor")


    # add machting version, keep devel version
    news_file <- file.path(path, "NEWS.md")
    old <- readLines(news_file)
    new <- "# prutp 0.2.0"
    writeLines(c(new, old), news_file)
    RUnit::checkException(packager::check_news(path))

    # remove devel version
    writeLines(new, news_file)
    RUnit::checkTrue(packager::check_news(path))

    # actual version not covered in NEWS.md
    writeLines(c("# prutp 0.2.1"), news_file)
    RUnit::checkException(packager::check_news(path))
}

test_git_tag <- function() {
    path <- file.path(tempdir(), "prutp")
    on.exit(unlink(path, recursive = TRUE))
    usethis::create_package(path)

    # no repo
    RUnit::checkException(packager::git_tag(path = path))

    # initial repo
    packager:::use_git(path)
    result <- packager::git_tag(path = path)
    RUnit::checkIdentical("0.0.0.9000", getElement(result, "name"))
    RUnit::checkIdentical("CRAN release", getElement(result, "message"))
    desc::desc_bump_version("minor", file = path)

    # uncommitted changes
    RUnit::checkException(packager::git_tag(path = path))

    # commited changes
    git_add_commit(path = path)
    result <- packager::git_tag(path = path)
    RUnit::checkIdentical("0.1.0", getElement(result, "name"))
    RUnit::checkIdentical("CRAN release", getElement(result, "message"))

    # version number lower than in tags
    desc::desc_set(Version = "0.0.3", file = path)
    git_add_commit(path = path)
    RUnit::checkException(packager::git_tag(path = path))

}

test_githuburl <- function() {
    path <- file.path(tempdir(), "prutp")
    on.exit(unlink(path, recursive = TRUE))
    usethis::create_package(path)
    user <- "foobar"


    #% gh_username defaults NULL, curl not working
    if (Sys.info()[["nodename"]] ==  "fvafrdebianCU") {
        # curl not working
        expectation <- FALSE
        result <- add_github_url_to_desc(path = path)
        RUnit::checkIdentical(expectation, result)
    } else {
        # FIXME: on travis??
    }

    #% No git remote set
    expectation <- FALSE
    result <- add_github_url_to_desc(path = path, default_gh_user = NA)
    RUnit::checkIdentical(expectation, result)

    #% remote set
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        # whoami is better than remote url
        url <- "https://github.com/fvafrCU/prutp"
    } else {
        url <- paste("https://github.com", user, "prutp", sep = "/")
    }

    unlink(path, recursive = TRUE)
    usethis::create_package(path)
    repo <- git2r::init(path)
    git2r::remote_add(repo, "github", url)
    result <- add_github_url_to_desc(path = path, default_gh_user = NA)
    RUnit::checkTrue(result)
    RUnit::checkIdentical(url, desc::desc_get_urls(path))

    #% user given
    if (Sys.info()[["nodename"]] %in% c("h5", "h6")) {
        # doubled entry from above
        expectation <- c("https://github.com/fvafrCU/prutp",
                         paste("https://github.com", user, "prutp", sep = "/"))
    } else {
        expectation <- paste("https://github.com", user, "prutp", sep = "/")
    }

    add_github_url_to_desc(path = path, default_gh_user = user)
    result <- desc::desc_get_urls(file = path)
    RUnit::checkIdentical(expectation, result)

}
