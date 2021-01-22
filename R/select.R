#' Subset columns by variable completion rate
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param complete_rate minimally acceptable completion rate 
#'
#' @return an object of the same type as .data
#' @export
#' 
#' @examples
#' ## select columns containing less than 5% missing
#' select_via_complete_rate(airquality, 0.95)
select_via_complete_rate <- function(.data, complete_rate) {
  .data %>% 
    dplyr::summarize(dplyr::across(dplyr::everything(), 
                                   ~ sum(!is.na(.)) / dplyr::n())) %>% 
    tidyr::pivot_longer(dplyr::everything()) %>% 
    dplyr::filter(value >= complete_rate) %>% 
    dplyr::pull(name) -> keepers
  
  .data %>% 
    dplyr::select(dplyr::all_of(keepers))
}

#' Select columns by class
#'
#' @param .data a data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param class the class of variable to select based on (can be negated with `-`)
#'
#' @return an object of the same type as .data
#' @export
#'
#' @examples
#' ## select factor columns
#' select_via_class(iris, factor)
#' 
#' ## select everything but factor columns
#' select_via_class(iris, -factor)
#' 
#' ## select numeric
#' select_via_class(iris, numeric)
select_via_class <- function(.data, class) {
  class <- deparse(substitute(class))
  negate <- grepl("^-", class)
  if(negate)
    class <- sub("^-", "", class)
  
  classes <- lapply(.data, base::class)
  keep <- lapply(classes, function(x) as.logical(!negate + class %in% x - 1))
  .data[, unlist(keep), drop = FALSE]
}
