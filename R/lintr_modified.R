# modified copies from lintr 1.0.2
fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}

highlight_string <- function(message, column_number = NULL, ranges = NULL) {
  maximum <- max(column_number, unlist(ranges))
  line <- fill_with(" ", maximum)
  lapply(ranges, function(range) {
    substr(line, range[1], range[2]) <<- fill_with("~", range[2] -
      range[1] + 1L)
  })
  substr(line, column_number, column_number + 1L) <- "^"
  line
}

print_lint <- function(x, ...) {
  color <- switch(x$type, warning = crayon::magenta, error = crayon::red,
    style = crayon::blue, crayon::bold
  )
  cat(
    sep = "", crayon::bold(x$mark, " ", x$filename, ":",
                           as.character(x$line_number),
      ":", as.character(x$column_number), ": ",
      sep = ""
    ),
    color(x$type, ": ", sep = ""), crayon::bold(x$message),
    "\n", x$line, "\n", highlight_string(
      x$message, x$column_number,
      x$ranges
    ), "\n"
  )
  invisible(x)
}
