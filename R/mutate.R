#' Create a dicotomized x column based on first rpart split
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr)
#' @param x name of predictor to dicotomize
#' @param y name of response to base dicotomization on
#'
#' @return .data with a new factor representing the optimal split
#' @export
#'
#' @examples
#' mutate_dicotomize(iris, Sepal.Length, Sepal.Width)
#'
#' @seealso \link[rpart]{rpart}
mutate_dicotomize <- function(.data, x, y) {
  m <- rpart::rpart(substitute(y ~ x), .data)
  k <- m$splits[1, "index"]
  n <- sprintf("%s_ge_%s", deparse(substitute(x)), k)
  v <- .data[deparse(substitute(x))] >= k
  .data[n] <- factor(v, labels = paste(c("<", ">="), k))
  .data
}

#' Auto coalesce after a join
#'
#' When joining data, there are often column conflicts. Coalescing is the most
#' common solution. This function attempts to identify the conflicting column
#' names after a join, coalesces them into the base name, and drops the
#' suffixed versions.
#'
#' @param data the data resulting from a join operation
#' @param suffix the suffixes used to disambiguate non-joined duplicate
#' variables during the join
#'
#' @return a dataframe with duplicate variables coalesced and the original
#' variants dropped
#' @export
#'
#' @examples
#' x <- data.frame(id = 1:3, v = paste0("x", 1:3))
#' y <- data.frame(id = 2:4, v = paste0("y", 2:4))
#' d <- dplyr::left_join(x, y, by = "id")
#' mutate_auto_coalesce(d)
mutate_auto_coalesce <- function(data, suffix = c(".x", ".y")) {
  message(
    "Check out powerjoin package: ",
    "https://github.com/moodymudskipper/powerjoin"
  )

  assertthat::are_equal(length(suffix), 2)

  data_names <- names(data)
  suffix_1_names <- data_names[grepl(sprintf("^.+%s$", suffix[1]), data_names)]
  suffix_2_names <- data_names[grepl(sprintf("^.+%s$", suffix[2]), data_names)]

  assertthat::are_equal(length(suffix_1_names), length(suffix_2_names))

  coalesce_names <- sub(suffix[1], "", suffix_1_names)
  for (i in seq_along(coalesce_names)) {
    data[[coalesce_names[i]]] <- dplyr::coalesce(
      data[[suffix_1_names[i]]],
      data[[suffix_2_names[i]]]
    )
    data[[suffix_1_names[i]]] <- NULL
    data[[suffix_2_names[i]]] <- NULL
  }

  return(data)
}
