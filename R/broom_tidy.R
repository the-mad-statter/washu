#' Add significance flag
#'
#' @param data data frame containing a column of p-values
#' @param p_val_col name of the p-value column
#'
#' @return a data frame with an additional character column named p_flag
#' @export
add_significance_flag <- function(data, p_val_col) {
  data %>%
    dplyr::mutate(p_flag = dplyr::case_when({{ p_val_col }} < .001 ~ "***",
                                            {{ p_val_col }} < .010 ~ "**",
                                            {{ p_val_col }} < .050 ~ "*",
                                            {{ p_val_col }} < .100 ~ ".",
                                            TRUE ~ ""))
}

#' Pairwise t tests
#'
#' @description Performs independent sample t-tests for each pair of groups from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param group grouping variable
#' @param ... additional parameters passed onto \code{\link[stats:t.test]{t.test()}}
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_pairwise_t_test_two_sample <- function(data, response, group, ...) {
  response <- rlang::ensym(response)
  group <- rlang::ensym(group)

  data %>%
    dplyr::pull(!!group) %>%
    factor() %>%
    levels() -> conditions

  combn(1:length(conditions), 2) %>%
    split(rep(1:ncol(.), each = nrow(.))) %>%
    lapply(function(p) {
      # make subset
      data %>%
        dplyr::filter(!!group %in% conditions[p]) -> ss

      # model
      tt <- rlang::eval_tidy(rlang::quo(t.test(!!response ~ !!group, data = ss, ...)))

      # tidy
      tt %>%
        broom::tidy() %>%
        dplyr::mutate(
          "response" = rlang::as_string(response),
          "group1" = !!paste0(group, "==", conditions[p][1]),
          "group2" = !!paste0(group, "==", conditions[p][2])
        ) %>%
        dplyr::select(response, group1, group2, dplyr::everything())
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p.value)
}

#' Pairwise t tests
#'
#' @description Performs paired t-tests for each pair of groups where observations are paired by unit from tidy data.
#'
#' @param data tidy data containing the response, group, and unit variables
#' @param response response variable
#' @param group grouping variable
#' @param unit sampling unit variable (e.g., persons)
#' @param ... additional parameters passed onto \code{\link[stats:t.test]{t.test()}}
#'
#' @note Tests are performed on pairwise complete data.
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_pairwise_t_test_dep_sample <- function(data, response, group, unit, ...) {
  response <- rlang::ensym(response)
  group <- rlang::ensym(group)
  unit <- rlang::ensym(unit)

  data %>%
    dplyr::pull(!!group) %>%
    factor() %>%
    levels() -> conditions

  combn(1:length(conditions), 2) %>%
    split(rep(1:ncol(.), each = nrow(.))) %>%
    lapply(function(p) {
      # make pairwise complete subset
      data %>%
        dplyr::filter(!is.na(!!response), !!group %in% conditions[p]) %>%
        dplyr::group_by(!!unit) %>%
        dplyr::filter(n() == 2) %>%
        dplyr::ungroup() -> data_pairwise_complete

      # split into (x,y)
      data_pairwise_complete %>%
        dplyr::filter(!!group == conditions[p][1]) %>%
        dplyr::pull(!!response) -> x

      data_pairwise_complete %>%
        dplyr::filter(!!group == conditions[p][2]) %>%
        dplyr::pull(!!response) -> y

      # model
      tt <- t.test(x, y, paired = TRUE, ...)

      # tidy
      tt %>%
        broom::tidy() %>%
        dplyr::mutate(
          "response" = rlang::as_string(response),
          "group1" = !!paste0(group, "==", conditions[p][1]),
          "group2" = !!paste0(group, "==", conditions[p][2])
        ) %>%
        dplyr::select(response, group1, group2, dplyr::everything())
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p.value)
}

#' Pairwise t tests
#'
#' @description Performs one-sample t-tests for each group from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param group grouping variable
#' @param mu a number indicating the true value of the mean
#' @param ... additional parameters passed onto \code{\link[stats:t.test]{t.test()}}
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_pairwise_t_test_one_sample <- function(data, response, group, mu = 0, ...) {
  response <- rlang::ensym(response)
  group <- rlang::ensym(group)

  data %>%
    dplyr::pull(!!group) %>%
    factor() %>%
    levels() -> conditions

  conditions %>%
    lapply(function(p) {
      # make subset
      data %>%
        dplyr::filter(!!group == p) %>%
        dplyr::pull(!!response) -> x

      # model
      tt <- t.test(x, mu = mu, ...)

      # tidy
      tt %>%
        broom::tidy() %>%
        dplyr::mutate(
          "response" = rlang::as_string(response),
          "group" = !!paste0(group, "==", p),
        ) %>%
        dplyr::select(response, group, dplyr::everything())
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p.value)
}

#' Pairwise cor tests
#'
#' @description Test for association between paired samples, using one of Pearson's product moment correlation coefficient, Kendall's tau or Spearman's rho for each pair of variables.
#'
#' @param data data frame containing the variables
#' @param ... further arguments passed to \code{\link[stats:cor.test]{cor.test()}}
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_cor_test <- function(data, ...) {
  vars <- names(data)
  combn(1:length(vars), 2) %>%
    split(rep(1:ncol(.), each = nrow(.))) %>%
    lapply(function(p) {
      # split into (x, y)
      x <- data[[vars[p[1]]]]
      y <- data[[vars[p[2]]]]

      # model
      ct <- cor.test(x, y, ...)

      # tidy
      ct %>%
        broom::tidy() %>%
        dplyr::mutate(
          v1 = vars[p][1],
          v2 = vars[p][2]
        ) %>%
        dplyr::select(v1, v2, dplyr::everything())
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p.value)
}

#' Subset columns using a correlation p-value
#'
#' @description Select variables in a data frame based on the p-value of the correlation with one of the variables.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param x name of the common variable with which to compute correlations
#' @param p.value the maximum desired p-value
#' @param ... additional arguments passed to \link[stats]{cor.test}
#'
#' @return An object of the same type as .data
#' @export
#'
#' @examples
#' select_via_cor_sig(mtcars, mpg, 0.001)
select_via_cor_sig <- function(.data, x, p.value, ...) {
  x <- rlang::ensym(x)

  .data %>%
    dplyr::select(-dplyr::all_of(x)) %>%
    names() %>%
    lapply(function(candidate) {
      c(rlang::as_string(x), candidate)
    }) -> ls_pairs
  
  ls_pairs %>% 
    lapply(function(vec_pair) {
      x <- .data[[vec_pair[1]]]
      y <- .data[[vec_pair[2]]]

      cor.test(x, y, ...) %>%
        broom::tidy() %>%
        dplyr::mutate(v1 = vec_pair[1], v2 = vec_pair[2]) %>%
        dplyr::select(v1, v2, dplyr::everything())
    }) %>%
    dplyr::bind_rows() -> tbl_tidy_cor_test
  
  tbl_tidy_cor_test %>% 
    dplyr::filter(p.value < {{p.value}}) %>%
    dplyr::pull(v2) %>% 
    c(rlang::as_string(x), .) -> keepers
  
  .data %>% 
    dplyr::select(dplyr::all_of(keepers))
}
