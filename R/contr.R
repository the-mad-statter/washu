#' Backward difference contrast matrix
#'
#' @description Return a backward difference contrast matrix
#'
#' @param fct factor variable to apply contrast to
#'
#' @return matrix with n rows and n-1 columns where n is the number of factor
#' levels
#' @export
#'
#' @references
#' [Backward difference coding](https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/#backward) # nolint
#'
#' @seealso \link[stats]{contrasts}
#'
#' @examples
#' contrasts(iris$Species) <- contr.backward_difference(iris$Species)
contr.backward_difference <- function(fct) {
  levels <- levels(fct)
  n <- length(levels)
  mx <- matrix(numeric(n * (n - 1)), nrow = n, ncol = n - 1)
  for (r in 1:n) {
    for (c in 1:(n - 1)) {
      mx[r, c] <- ifelse(r <= c, c - n, c) / n
    }
  }
  colnames(mx) <- paste0(
    "[",
    levels[2:n],
    "-",
    levels[1:(n - 1)],
    "]",
    sep = ""
  )
  mx
}
