is_check <- function(x) {
    is_check_stage <- identical(getElement(x, "stage"), "check")
    is_check_job <- identical(getElement(x, "name"), "packager")
    is_check <- is_check_stage && is_check_job
    return(is_check)
}

is_my_name <- function(x, name) return(tolower(getElement(x, "name")) == name)

#' Read a \verb{gitlab} Check Log
#'
#' For a given user's project, the last log for jobs for name and stage "check"
#' will be read. This is assumed to be the output of \command{R CMD check},
#' \code{\link[rcmdcheck:rcmdcheck]{rcmdcheck::rcmdcheck}}
#' \code{\link[devtools:check]{devtools::check}}, or the like.
#' @param user The user's name on \verb{gitlab}.
#' @param project The project's name on \verb{gitlab}.
#' @param private_token The user's private token on \verb{gitlab}.
#' @param ... Arguments passed to \code{\link[httr:GET]{httr::GET}}.
#' @return A character vector containing the lines of the \verb{gitlab} log.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' gitlab_token <- readLines(file.path("~", ".gitlab_private_token.txt"))
#' if (Sys.info()[["nodename"]] == "fvafrdebianCU") {
#'     j <- get_gitlab_log(user = "fvafrcu", project = "packager",
#'                         private_token = gitlab_token,
#'                         httr::use_proxy("10.127.255.17", 8080))
#' } else {
#'     j <- get_gitlab_log(user = "fvafrcu", project = "packager",
#'                         private_token = gitlab_token)
#' }
#'
#' cat(j, sep = "\n")
#' }
get_gitlab_log <- function(user, project, private_token, ...) {
    if (is.null(private_token)) {
        job <- NULL
    } else {
        url <- paste0("https://gitlab.com/api/v4/users/", user, "/projects", 
                      "?per_page=100")
        names(private_token) <- "PRIVATE-TOKEN"
        r <- httr::GET(url, httr::add_headers(.headers = private_token), ...)
        projects <- httr::content(r)
        project_index <- sapply(projects, is_my_name, project)
        if (!any(project_index)) throw(paste0("Could not find `", project,
                                             "` on ", url, "."))
        my_project <- projects[project_index][[1]]
        url <- paste("https://gitlab.com/api/v4/projects",
                      my_project[["id"]], "jobs/", sep = "/")
        r <- httr::GET(url, httr::add_headers(.headers = private_token), ...)
        jobs <- httr::content(r)
        check_jobs <-  jobs[sapply(jobs, is_check)]
        last_check_jobs_url <- check_jobs[[1]][["web_url"]]
        r <- httr::GET(paste(last_check_jobs_url, "raw", sep = "/"), ...)
        job <- httr::content(r)
        if (!is.null(job))
            job <- unlist(strsplit(job, split = "\n"))
    }
    return(job)
}
