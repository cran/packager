## convert vignettes from Rmd to rasciidoc
rmd2rasciidoc <- function(in_file,
                             out_file = sub("\\.Rmd$", ".Rasciidoc", in_file)) {
    if (file.exists(in_file)) {
        lines <- readLines(in_file)
        header_bounds <- grep("^--- *$", lines)
        header <- lines[(header_bounds[1] + 1):(header_bounds[2] -1)]
        body <- lines[(header_bounds[2] + 1):length(lines)]
        body_safe <- body
        # convert code blocks first to get faciliate getting their start/stop
        # indices:
        body <- convert_code_blocks(body)

        code_block_starts <- grep("^//begin\\.rcode.*$", body)
        code_block_stops <- grep("^//end\\.rcode.*$", body)
        rcode_block_index <- unlist(apply(cbind(code_block_starts, code_block_stops), 1, function(x) seq(x[1], x[2])))

        body <- convert_links(body)
        body <- convert_code_inline(body)
        body <- convert_sections(body)
        body[rcode_block_index] <- body_safe[rcode_block_index]
        body <- convert_code_blocks(body)


        new_header <- convert_title(header)
        new_header <- c(new_header, convert_author(header))
        new_header <- c(new_header, convert_date(header))
        new_header <- c(new_header, ":toc2:")
        new_header <- c(new_header, convert_vignette_directions(header))
        lines <- c(new_header, body)
        writeLines(lines, out_file)
    } else {
        warning(in_file, " does not exist!")
    }
    return(invisible(out_file))
}
convert_vignette <- function(in_file,
                             out_file = sub("\\.Rmd$", ".Rasciidoc", in_file)) {
    res <- FALSE
    if (file.exists(in_file)) {
        res <- rmd2rasciidoc(in_file = in_file, out_file = out_file)
        is_vignette <- any(grepl("%\\\\Vignette", readLines(in_file)))
        is_same_path <- out_file == sub("\\.Rmd$", ".Rasciidoc", in_file)
        if (is_vignette && is_same_path) file.remove(in_file)
    } else {
        warning(in_file, " does not exist!")
    }
    return(invisible(res))
}

convert_package_vignettes <- function(path) {
    path <- as.package(path)[["path"]]
    vignette_builder <- desc::desc_get_field("VignetteBuilder", file = path)
    if(! "rasciidoc" %in% unlist(strsplit(vignette_builder, split = ","))) {
        vignette_builder <- paste("rasciidoc,", vignette_builder)
    }
    add_desc_package(path, "Suggests", "rasciidoc")
    desc::desc_set("VignetteBuilder" = vignette_builder, file = path)
    markdown_vignettes <- dir(file.path(path, "vignettes"),
                              full.names = TRUE,
                              pattern = ".*\\.Rmd$")
    for (markdown_vignette in markdown_vignettes)
        convert_vignette(markdown_vignette)
}
# helper functions
convert_links <- function(lines) gsub("\\[(.*)\\]\\((.*)\\)", "\\2[\\1]", lines)

convert_code_inline <- function(lines) gsub("`+", "+", lines)

convert_sections <- function(lines) {
    l <- sub("^#", "=#", lines)
    for (i in 1:7) l <- gsub("=#", "==", l)
    return(l)
}

convert_code_blocks <- function(lines)
    sub("^```$", "//end.rcode",
        sub("^``` *\\{r(.*)\\}", "//begin.rcode\\1", lines))
convert_title <- function(lines) {
    line <- grep("^title:", lines, value = TRUE)
    line <- paste("=", sub("^\"", "",
                           sub("\"$", "",
                               sub("^title: *(.*)", "\\1", line))))
    return(line)
}
convert_author <- function(lines) {
    line <- grep("^author:", lines, value = TRUE)
    if (identical(line, "author: ")) {
        line <- NULL
    } else {
        line <- sub("^a", ":A", line)
    }
    return(line)
}
convert_date <- function(lines) {
    line <- grep("^date:", lines, value = TRUE)
    line <- sub("^d", ":D", line)
    return(line)
}
convert_vignette_directions <- function(lines) {
    l <- grep("^ *%\\\\Vignette", lines, value = TRUE)
    l <- sub(".*(\\%\\\\Vignette.*)", "    \\1", l)
    l <- sub("%\\\\VignetteEngine\\{knitr::rmarkdown\\}", "%\\\\VignetteEngine\\{rasciidoc::rasciidoc\\}", l)
    l <- c("", "////", l, "////")
    return(l)
}
