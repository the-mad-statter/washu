#' Replace missing values
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param x name of variable to impute on
#'
#' @return an object of the same type as .data with missing values in the desired column imputed by random sample from observed values
#' @export
#'
#' @seealso \link[tidyimpute]{impute}
#'
#' @examples
#' impute_bootstrap(airquality, Ozone)
impute_bootstrap <- function(.data, x) {
  orig <- .data %>% pull({{ x }})
  boot <- sample(na.omit(orig), length(orig), TRUE)
  impt <- if_else(is.na(orig), boot, orig)
  .data %>% mutate({{ x }} := impt)
}
