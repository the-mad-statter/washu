#' Excel column number
#'
#' @param column_reference excel column reference (e.g., "A" or "AC")
#'
#' @return column number index
#' @export
#'
#' @examples
#' xl_column_number("A")
xl_column_number <- function(column_reference) {
  column_reference <- toupper(column_reference)
  letters <- strsplit(column_reference, "")
  numbers <- lapply(letters, function(x) match(x, LETTERS))
  vapply(numbers, function(n) sum(n * 26^(rev(seq_along(n)) - 1)), numeric(1))
}
