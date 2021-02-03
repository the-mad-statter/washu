#' Create a dicotomized x column based on first rpart split
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param x name of predictor to dicotomize
#' @param y name of response to base dicotomization on
#'
#' @return .data with a new factor representing the optimal split
#' @export
#'
#' @examples
#' mutate_dicotomize(iris, Sepal.Length, Sepal.Width)
mutate_dicotomize <- function(.data, x, y) {
  m <- rpart::rpart(substitute(y ~ x), .data)
  k <- m$splits[1, "index"]
  n <- sprintf("%s_ge_%s", deparse(substitute(x)), k)
  v <- .data[deparse(substitute(x))] >= k
  .data[n] <- factor(v, labels = paste(c("<", ">="), k))
  .data
}
