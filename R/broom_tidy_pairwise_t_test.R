#' Pairwise t tests
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param group grouping variable
#' @param ... additional parameters passed onto t.test()
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
    dplyr::bind_rows()
}

#' Pairwise t tests
#'
#' @param data tidy data containing the response, group, and unit variables
#' @param response response variable
#' @param group grouping variable
#' @param unit sampling unit variable (e.g., persons)
#' @param ... additional parameters passed onto t.test()
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
    dplyr::bind_rows()
}

#' Pairwise t tests
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param group grouping variable
#' @param mu a number indicating the true value of the mean
#' @param ... additional parameters passed onto t.test()
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
    dplyr::bind_rows()
}
