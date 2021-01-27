#' Subset columns by variable completion rate
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param rate completion rate
#' @param greater if TRUE, rate is the minimally acceptable completion rate
#'
#' @return an object of the same type as .data
#' @export
#' 
#' @examples
#' ## select columns with at least a 95% completion rate
#' select_via_complete_rate(airquality, 0.95)
select_via_complete_rate <- function(.data, rate, greater = TRUE) {
  cf <- function(x, y) ifelse(greater, x > y, x < y)
  dplyr::select(.data, tidyselect:::where(~ cf(sum(!is.na(.)) / length(.), rate)))
}

#' Subset columns by variable missing rate
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param rate missing rate
#' @param greater if TRUE, rate is the maximally acceptable missing rate
#'
#' @return an object of the same type as .data
#' @export
#' 
#' @examples
#' ## select columns with a maximum missing rate of 5%
#' select_via_missing_rate(airquality, 0.05)
select_via_missing_rate <- function(.data, rate, greater = TRUE) {
  cf <- function(x, y) ifelse(greater, x > y, x < y)
  dplyr::select(.data, tidyselect:::where(~ cf(sum(is.na(.)) / length(.), rate)))
}

#' Select columns by class
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param class variable class to select on
#' @param keep if TRUE, keep variables of type class, otherwise drop
#'
#' @return an object of the same type as .data
#' @export
#'
#' @examples
#' ## select factor columns
#' select_via_class(iris, "factor")
#' 
#' ## select everything but factor columns
#' select_via_class(iris, "factor", FALSE)
#' 
#' ## select numeric
#' select_via_class(iris, "numeric")
select_via_class <- function(.data, class, keep = TRUE) {
  if_not_keep_negate <- function(x) ifelse(keep, x, !x)
  dplyr::select(.data, tidyselect:::where(~ if_not_keep_negate(any(class(.) %in% class))))
}
