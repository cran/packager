## convert vignettes from Rmd to rasciidoc
convert_vignette <- function(in_file, 
                             out_file = sub("\\.Rmd$", ".Rasciidoc", in_file)) {
    if (file.exists(in_file)) {
        lines <- readLines(in_file)
        lines <- convert_links(lines)
        lines <- convert_code_blocks(lines)
        lines <- convert_code_inline(lines)
        lines <- convert_sections(lines)
        lines <- convert_title(lines)
        lines <- convert_author(lines)
        lines <- remove_header(lines)
        lines <- convert_vignette_header(lines)
        writeLines(lines, out_file)
        file.remove(in_file)
    } else {
        warning(in_file, " does not exist!")
    }
    return(invisible(NULL))
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
    sub("^```$", "//end.rcode", sub("^```\\{r(.*)\\}", "//begin.rcode\\1", lines))
convert_title <- function(lines) sub("^title:(.*)", "= \\1", lines)
convert_author <- function(lines) {
    
    author_line <- grep("^author:", lines, value = TRUE)
    if (author_line == "author: ") {
        lines[!grepl("^author: ", lines)]
    } else {
        sub("^author: (.*)", "\\1", lines)
    }
}
remove_header <- function(lines) lines[-c(1, grep("^(author|date|output):", lines))]
convert_vignette_header <- function(lines) {
    l <- sub("^vignette: >$", ":toc2:\n////", lines)
    l <- sub("^---$", "\n////", l)
    l <- sub(".*(\\%\\\\Vignette.*)", "    \\1", l)
    l <- sub("%\\\\VignetteEngine\\{knitr::rmarkdown\\}", "%\\\\VignetteEngine\\{rasciidoc::rasciidoc\\}", l)
    return(l)
}
convert_package_vignette <- function(path) {
    path <- as.package(path)[["path"]]
    vignette_builder <- desc::desc_get_field("VignetteBuilder", file = path)
    if(! "rasciidoc" %in% unlist(strsplit(vignette_builder, split = ","))) {
        vignette_builder <- paste("rasciidoc,", vignette_builder)
    }
    desc::desc_set("VignetteBuilder" = vignette_builder, file = path)
    convert_vignette(dir(file.path(path, "vignettes"), 
                                             full.names = TRUE,
                                             pattern = ".*\\.Rmd$"))

}
