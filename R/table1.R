#' Generate an HTML table of descriptive statistics
#'
#' @param file optional file location to write table to
#' @param css cascading style sheet to embed
#' @inheritParams table1::table1
#'
#' @return a washu_table1 object
#' @export
#'
#' @examples
#' \dontrun{
#' table1(~., mtcars, "Table 1. Descriptives.html")
#' }
table1 <- function(x,
                   data,
                   file,
                   css = system.file("table1_defaults_1.0",
                     "table1_defaults.css",
                     package = "table1"
                   ),
                   ...) {
  table1::table1(x, data, ...) -> table_1

  html_lines <- c(
    "<html>",
    "<style>",
    readLines(css),
    "</style>",
    table_1,
    "</html>"
  )

  if (!missing(file)) {
    writeLines(html_lines, file)
  }

  structure(html_lines,
    class = c("washu_table1", class(html_lines))
  )
}

#' Print washu_table1 objects
#'
#' @param x an object of type washu_table1
#'
#' @return the argument x, invisibly
#'
#' @export
print.washu_table1 <- function(x) {
  tf <- tempfile(fileext = ".html")
  cat(x, file = tf)
  rstudioapi::viewer(tf)
  invisible(x)
}

#' Custom S3 Print for washu_table1 objects
#' @param x A washu_table1 object to print
#' @param ... additional arguments
#' @importFrom knitr knit_print
#' @export
knit_print.washu_table1 <- function(x, ...) {
  knitr::asis_output(x)
}
