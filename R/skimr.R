#' Set or add the summary functions for a particular type of data
#'
#' @description Build a custom skim function that adds percents to factor summaries
#'
#' @param fct_n_levels number of factor levels to print sorted in descending frequency
#' @param fct_percents flag to print percents per level
#' @param fct_digits number of significant digits to print for factor summaries
#' @param fct_delimiter delimiter to use between factor levels
#' 
#' @note NAs are ignored when computing percents.
#'
#' @return A new skim() function. This is callable.
#' @export
#'
#' @examples
#' wu_skim <- wu_skim_with()
#' wu_skim(factor(mtcars$cyl))
wu_skim_with <- function(fct_n_levels = NULL, fct_percents = TRUE, fct_digits = 1, fct_delimiter = c(", ", "<br>")) {
  fct_delimiter <- match.arg(fct_delimiter)
  
  skimr::skim_with(
    factor = skimr::sfl(
      "ordered" = NULL, 
      "top_counts" = NULL,
      "counts" = function(x) {
        conts <- rev(sort(table(x)))
        pairs <- sprintf("%s: %s", names(conts), conts)
        
        if(fct_percents) {
          props <- prop.table(conts)
          percs <- format(round(100 * props, fct_digits), nsmall = fct_digits)
          pairs <- sprintf("%s: %s (%s%%)", names(conts), conts, percs)
        }
        
        if(!is.null(fct_n_levels))
          pairs <- pairs[1:fct_n_levels]
        
        paste(pairs, collapse = fct_delimiter)
      }
    ),
    numeric = skimr::sfl("hist" = NULL)
  )
}
