#' WebPower Bind WebPower Objects
#'
#' @param x first WebPower object
#' @param y second WebPower object
#'
#' @return a combined WebPower object
#' @export
#'
#' @examples
#' WebPower::wp.rmanova(
#'   n = 40,
#'   ng = 2,
#'   nm = 2,
#'   f = NULL,
#'   nscor = 1,
#'   alpha = 0.05,
#'   power = 0.80,
#'   type = 2
#' ) -> x
#'
#' WebPower::wp.rmanova(
#'   n = 200,
#'   ng = 2,
#'   nm = 2,
#'   f = NULL,
#'   nscor = 1,
#'   alpha = 0.05,
#'   power = 0.80,
#'   type = 2
#' ) -> y
#'
#' WebPower_bind_webpower(x, y)
WebPower_bind_webpower <- function(x, y) {
  for(n in names(x)) {
    if(washu::`%notin%`(n, c("note", "method", "url")))
      x[[n]] <- c(x[[n]], y[[n]])
  }
  return(x)
}
