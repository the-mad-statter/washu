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
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters passed onto \code{\link[stats:t.test]{t.test()}}
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_pairwise_t_test_two_sample <- function(data,
                                                  response,
                                                  group,
                                                  descriptives = TRUE,
                                                  ...) {
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
          "group_1" = !!paste0(group, "==", conditions[p][1]),
          "group_2" = !!paste0(group, "==", conditions[p][2])
        ) %>%
        dplyr::rename(
          mean_diff_est = estimate,
          mean_diff_lwr = conf.low,
          mean_diff_upr = conf.high,
          t_value = statistic,
          df = parameter,
          p_value = p.value
        ) %>%
        dplyr::select(
          -dplyr::starts_with("estimate")
        ) -> tdy_infr

      ss %>%
        dplyr::group_by(!!group) %>%
        dplyr::summarize(
          n = sum(!is.na(!!response)),
          m = mean(!!response, na.rm = TRUE),
          s = sd(!!response, na.rm = TRUE),
        ) %>%
        tidyr::pivot_wider(
          names_from = !!group,
          values_from = -!!group
        ) -> tdy_desc
      names(tdy_desc) <- c("n_1", "n_2", "m_1", "m_2", "s_1", "s_2")

      if (descriptives) {
        dplyr::bind_cols(tdy_desc, tdy_infr)
      } else {
        tdy_infr
      }
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p_value) %>%
    dplyr::select(
      response,
      group_1,
      group_2,
      dplyr::everything()
    )
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

#' Augment data with information from an object
#'
#' @param x Model object of class lmerModLmerTest with information to append to observations.
#' @param data observations to be augmented
#' @param ... Addition arguments to augment method.
#'
#' @return A \link[tibble]{tibble} with information about data points.
#' @export
broom_augment.lmerModLmerTest <- function(x, data, ...) {
  data %>%
    tibble::rownames_to_column(".rowname") %>%
    dplyr::left_join(
      tibble::tibble(.rowname = names(fitted), .fitted = predict(mdl_thigh_pain)),
      by = ".rowname"
    ) %>%
    select(-.rowname)
}

#' Pairwise Wilcoxon (Mann Whitney U) Rank Sum Tests
#'
#' @description Performs independent sample Wilcoxon tests for each pair of groups from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param group grouping variable
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters passed onto \code{\link[stats:wilcox.test]{wilcox.test()}}
#'
#' @return a tibble with information about the model components; one model per row
#' @export
broom_tidy_pairwise_wilcox_test_two_sample <- function(
  data,
  response,
  group,
  descriptives = TRUE,
  ...
) {
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
      wrst <- rlang::eval_tidy(rlang::quo(wilcox.test(!!response ~ !!group, data = ss, ...)))

      # tidy
      wrst %>%
        broom::tidy() %>%
        dplyr::mutate(
          "response" = rlang::as_string(response),
          "group_1" = !!paste0(group, "==", conditions[p][1]),
          "group_2" = !!paste0(group, "==", conditions[p][2])
        ) %>%
        dplyr::rename(
          w_value = statistic,
          p_value = p.value
        ) -> tdy_infr

      ss %>%
        dplyr::group_by(!!group) %>%
        dplyr::summarize(
          n = sum(!is.na(!!response)),
          md = median(!!response, na.rm = TRUE),
          q25 = quantile(!!response, 0.25, na.rm = TRUE, names = FALSE),
          q75 = quantile(!!response, 0.75, na.rm = TRUE, names = FALSE),
          iqr = q75 - q25
        ) %>%
        tidyr::pivot_wider(
          names_from = !!group,
          values_from = -!!group
        ) -> tdy_desc
      names(tdy_desc) <- c("n_1", "n_2", "md_1", "md_2", "q25_1", "q25_2", "q75_1", "q75_2", "iqr_1", "iqr_2")

      if (descriptives) {
        dplyr::bind_cols(tdy_desc, tdy_infr)
      } else {
        tdy_infr
      }
    }) %>%
    dplyr::bind_rows() %>%
    add_significance_flag(p_value) %>%
    dplyr::select(
      response,
      group_1,
      group_2,
      dplyr::everything()
    )
}

#' One-sided Confidence Intervals
#'
#' @param x An lm object created by stats::lm().
#' @param alternatives Logical indicating whether or not to include a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence interval if conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the the coefficient estimates. This is typical for logistic and multinomial regressions, but a bad idea if there is no log or logit link. Defaults to FALSE.
#'
#' @export
broom_tidy_sided_ci <- function(
  x,
  alternatives = 'two.sided',
  conf.level = 0.95,
  exponentiate = FALSE
) {
  warning('This function is experimental. Use with caution.')

  x %>%
    broom::tidy(conf.level = conf.level) %>%
    dplyr::mutate(
      alternative = alternatives
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      p.value = dplyr::if_else(alternative %in% c('less', 'greater'), p.value / 2, p.value),
      conf.low = dplyr::case_when(
        alternative == 'two.sided' ~ confint(x, level = conf.level)[[term, 1]],
        alternative == 'less'      ~ -Inf,
        alternative == 'greater'   ~ -confint(x, level = 1 - 2 * (1 - conf.level))[[term, 2]],
        TRUE ~ NA_real_
      ),
      conf.high = dplyr::case_when(
        alternative == 'two.sided' ~ confint(x, level = conf.level)[[term, 2]],
        alternative == 'less'      ~ -confint(x, level = 1 - 2 * (1 - conf.level))[[term, 1]],
        alternative == 'greater'   ~ Inf,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::ungroup() -> r

  if(exponentiate)
    dplyr::mutate(r, across(c(estimate, conf.low, conf.high), ~ exp(.))) -> r

  return(r)
}
