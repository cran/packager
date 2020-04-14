# from devtools 1.13.3, added cran_submission_url as argument
upload_cran <- function(pkg, built_path, cran_submission_url = NULL) {
    pkg <- as.package(pkg)
    maint <- maintainer(pkg)
    comments <- cran_comments(pkg)
    message("Uploading package & comments")
    body <- list(pkg_id = "", name = maint$name, email = maint$email,
        uploaded_file = httr::upload_file(built_path, "application/x-gzip"),
        comment = comments, upload = "Upload package")
    r <- httr::POST(cran_submission_url, body = body)
    httr::stop_for_status(r)
    new_url <- httr::parse_url(r$url)
    new_url$query$strErr # Exclude Linting
    message("Confirming submission")
    body <- list(pkg_id = new_url$query$pkg_id, name = maint$name,
        email = maint$email, policy_check = "1/", submit = "Submit package")
    r <- httr::POST(cran_submission_url, body = body)
    httr::stop_for_status(r)
    new_url <- httr::parse_url(r$url)
    if (new_url$query$submit == "1") {
        message("Package submission successful.\n",
                "Check your email for confirmation link.")
    }
    else {
        warning(gsub("\\+", " ", new_url$query$strErr))
        stop("Package failed to upload.", call. = FALSE)
    }
    invisible(TRUE)
}

