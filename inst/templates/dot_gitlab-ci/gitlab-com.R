# check if this is gitlab.com
if (!require("fritools", character.only = TRUE))
    install.packages("fritools", repos = "https://cloud.r-project.org/")
r <- fritools::is_running_on_gitlab_com(verbose = TRUE)
warning(attr(r, "message"))
print(r)
if (!isTRUE(r)) {
    stop("fritools: Do not recognize gitlab.com") 
} else {
    message("Node is ", Sys.info()[["nodename"]],
            ", I guess this is gitlab.com.")
}
