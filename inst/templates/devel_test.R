#!/usr/bin/env Rscript
## Open an R Code File and a Corresponding Unit Test
arguments <- commandArgs(trailingOnly = TRUE)
file_arguments <- c("-f", "--file")
if (all(file_arguments %in% arguments)) stop(paste("got both",
                                                   paste(file_arguments,
                                                         collapse = " and ")))
for (file_argument in file_arguments) {
    if (file_argument %in% arguments) {
        file_name <- arguments[which(file_argument == arguments) + 1]
    }
}
missing_file_name <- ! exists("file_name")
missing_file_options <- ! any(file_arguments %in% arguments)
if (missing_file_name && missing_file_options) {
    # get file_name as last argument not starting with "-"
    left_over <- grep("^-", arguments, invert = TRUE, value = TRUE)
    num_left_over <- length(left_over)
    if (num_left_over > 0) {
        file_name <- left_over[num_left_over]
    }
}
if (! exists("file_name")) stop("don't know which file to use")
if (! file.exists(file_name)) stop(paste("there is no file", file_name))

fritools::develop_test(file_name,  force_runit = TRUE, force_tiny = FALSE)
