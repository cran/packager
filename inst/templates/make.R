#!/usr/bin/env Rscript
arguments <- commandArgs(trailingOnly = TRUE)

target_arguments <- c("-t", "--target")
if (all(target_arguments %in% arguments)) stop(paste("got both",
                                                   paste(target_arguments,
                                                         collapse = " and ")))
for (target_argument in target_arguments) {
    if (target_argument %in% arguments) {
        target_name <- arguments[which(target_argument == arguments) + 1]
    }
}
missing_target_name <- ! exists("target_name")
missing_target_options <- ! any(target_arguments %in% arguments)
if (missing_target_name && missing_target_options) {
    # get target_name as last argument not starting with "-"
    left_over <- grep("^-", arguments, invert = TRUE, value = TRUE)
    num_left_over <- length(left_over)
    if (num_left_over > 0) {
        target_name <- left_over[num_left_over]
    }
}
if (! exists("target_name")) {
    target_name <- "cran_comments"
    warning("Setting missing target to `", target_name,"`!")
}
if (devtools::as.package(".")[["package"]] == "packager") pkgload::load_all()
makelist <- packager::get_package_makelist()
print(fakemake::make(target_name, makelist, verbose = TRUE))
