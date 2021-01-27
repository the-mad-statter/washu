#' Power plot for Cohen's f2
#' @param data tibble of effect size data
#' @param N total sample sizes for which to compute power
#' @param u1 degrees of freedom for effect generator
#' @param u2 degrees of freedom for other predictors (1 + u1 + u2 + v = N)
#' @param sig.level significance level (Type I error probability)
#' @param min_pwr minimum desirable power to label (NULL for no label)
#' @param effect variable in data representing the effect sizes
#' @param labels effect labels to be used for legend
#' @return object of class "ggplot"
#' @export
#' @examples
#' ## Exercise 9.1 P. 424 from Cohen (1988)
#' dplyr::tibble(f2 = 0.1/(1-0.1),
#'               y = "sales",
#'               x = "demographics",
#'               source = "Cohen (1988)") %>%
#'   pp_f2(85:105,
#'         u1 = 5,
#'         labels = sprintf("%s %s", format(round(f2, 2), nsmall = 2), source))
#' @note The effect variable is converted to factor in the given order and labels applied respectively.
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[pwr]{pwr.f2.test}}
pp_f2 <- function(data, N, u1 = 1, u2 = 0, sig.level = 0.05, min_pwr = NULL, effect = .data$f2, labels = .data$f2) {
  # guide_label <- latex2exp::TeX("$\u0192^2$")
  guide_label <- "\u0192\u00b2"

  grid <- expand.grid(i = 1:nrow(data), N = N)
  grid <- cbind(data[grid$i, ], grid)

  grid %>%
    dplyr::select(-.data$i) %>%
    dplyr::mutate(
      u1 = u1,
      u2 = u2,
      v = .data$N - 1 - .data$u1 - .data$u2,
      sig.level = sig.level,
      power = pwr::pwr.f2.test(.data$u1, .data$v, {{ effect }}, .data$sig.level)$power,
      f2_fct = factor({{ effect }},
                      unique({{ effect }}),
                      labels = unique({{ labels }}))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(.data$N, .data$power, color = .data$f2_fct, linetype = .data$f2_fct)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Total Sample Size", y = "Power",
      color = guide_label, linetype = guide_label
    ) -> pp

  if (!is.null(min_pwr)) {
    pp <- pp + ggplot2::geom_hline(
      yintercept = min_pwr,
      color = "red",
      linetype = "dashed"
    )
  }

  return(pp)
}

#' Map between various effect sizes
#' @param size effect size to convert (see details)
#' @param type the effect type to convert
#' @details Ported from Microsoft Excel Spreadsheet created by Jamie DeCoster on 2012-06-19 \url{http://www.stat-help.com} with the addition of R^2, f^2, and omega^2.
#'
#' Expected values for type include: \cr
#' \tabular{ll}{
#'   r \tab \href{https://en.wikipedia.org/wiki/Effect_size#Pearson_r_or_correlation_coefficient}{Pearson's r} \cr
#'   R^2 \tab \href{https://en.wikipedia.org/wiki/Effect_size#Coefficient_of_determination}{Coefficient of Determination} \cr
#'   d \tab \href{https://en.wikipedia.org/wiki/Effect_size#Cohen's_d}{Cohen's d} \cr
#'   or \tab \href{https://en.wikipedia.org/wiki/Effect_size#Odds_ratio}{Odds ratio} \cr
#'   f \tab \href{https://en.wikipedia.org/wiki/Effect_size#Cohen's_f2}{Cohen's f} \cr
#'   f^2 \tab \href{https://en.wikipedia.org/wiki/Effect_size#Cohen's_f2}{Cohen's f^2} \cr
#'   eta^2 \tab \href{https://en.wikipedia.org/wiki/Effect_size#Eta-squared_(e2)}{Eta-squared} \cr
#'   omega^2 \tab \href{https://en.wikipedia.org/wiki/Effect_size#Omega-squared_(w2)}{Omega-squared} \cr
#'   cl \tab \href{https://en.wikipedia.org/wiki/Effect_size#Common_language_effect_size}{Common Language Effect Size}
#' }
#' @return list containing converted effects
#' @references
#' Formulas for converting between f, f-squared, r-squared, eta-squared, and d were taken from: \cr
#' Cohen J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.), Hillsdale, NJ: Erlbaum. pp. 281, 284, 285
#'
#' Formulas for converting between r and d were taken from: \cr
#' Rosenthal, R. (1994). Parametric measures of effect size. In H. Cooper & L. V. Hedges (Eds.), The Handbook of Research Synthesis. New York, NY: Sage. pp. 239.
#'
#' Formulas for converting between the odds ratio and d were taken from: \cr
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Introduction to Meta-Analysis. Chichester, West Sussex, UK: Wiley.
#'
#' Formulas for converting between the cl (also called the "Common Language Effect Size") and d were taken from: \cr
#' Ruscio, J. (2008). A probability-based measure of effect size: Robustness to base rates and other factors. Psychological Methods, 13, 19-30.
#'
#' Formulas for converting between f-squared and omega-squared were taken from: \cr
#' Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9(2), 164-182.
#' @export
#' @examples # small Cohen's d
#' es_map(0.20, "d")
#'
#' # medium Cohen's d
#' es_map(0.50, "d")
#'
#' # large Cohen's d
#' es_map(0.80, "d")
#'
#' # testing code
#' sapply(unlist(as.list(formals(es_map)$type[2:10])), function(x) es_map(0.10, x))
es_map <- function(size, type = c("r", "R^2", "d", "or", "f", "f^2", "eta^2", "omega^2", "cl")) {
  type <- match.arg(type)

  if (type == "r"){
    r         <- size
    `R^2`     <- r^2
    d         <- r * 2 / sqrt(1 - r^2)
    or        <- exp(d * pi / sqrt(3))
    f         <- d / 2
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if (type == "R^2") {
    `R^2`     <- size
    r         <- sqrt(`R^2`)
    d         <- r * 2 / sqrt(1 - r^2)
    or        <- exp(d * pi / sqrt(3))
    f         <- d / 2
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d/sqrt(2))
  } else if (type == "d"){
    d         <- size
    r         <- sqrt(d^2 / (d^2 + 4))
    `R^2`     <- r^2
    or        <- exp(d * pi / sqrt(3))
    f         <- d/2
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if (type == "or") {
    or        <- size
    d         <- log(or) * (sqrt(3) / pi)
    r         <- sqrt(d^2 / (d^2 + 4))
    `R^2`     <- r^2
    f         <- d/2
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if (type == "f") {
    f         <- size
    d         <- f * 2
    r         <- sqrt(d^2 / (d^2 + 4))
    `R^2`     <- r^2
    or        <- exp(d * pi / sqrt(3))
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if(type == "f^2") {
    `f^2`     <- size
    `R^2`     <- `f^2` / (1 + `f^2`)
    r         <- sqrt(`R^2`)
    d         <- r * 2 / sqrt(1 - r^2)
    or        <- exp(d * pi / sqrt(3))
    f         <- d / 2
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if(type == "eta^2") {
    `eta^2`   <- size
    f         <- sqrt(`eta^2` / (1 - `eta^2`))
    d         <- f * 2
    r         <- sqrt(d^2 / (d^2 + 4))
    `R^2`     <- r^2
    `f^2`     <- `R^2` / (1 - `R^2`)
    or        <- exp(d * pi / sqrt(3))
    `omega^2` <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if (type == "omega^2") {
    `omega^2` <- size
    `f^2`     <- `omega^2` / (1 - `omega^2`)
    `R^2`     <- `f^2` / (1 + `f^2`)
    r         <- sqrt(`R^2`)
    d         <- r * 2 / sqrt(1 - r^2)
    or        <- exp(d * pi / sqrt(3))
    f         <- d / 2
    `eta^2`   <- `f^2` / (1 + `f^2`)
    cl        <- stats::pnorm(d / sqrt(2))
  } else if(type == "cl"){
    cl        <- size
    d         <- sqrt(2) * stats::qnorm(cl)
    r         <- sqrt(d^2 / (d^2 + 4))
    `R^2`     <- r^2
    or        <- exp(d * pi / sqrt(3))
    f         <- d / 2
    `f^2`     <- `R^2` / (1 - `R^2`)
    `eta^2`   <- `f^2` / (1 + `f^2`)
    `omega^2` <- `f^2` / (1 + `f^2`)
  }

  return(list("r"       = r,
              "R^2"     = `R^2`,
              "d"       = d,
              "or"      = or,
              "f"       = f,
              "f^2"     = `f^2`,
              "eta^2"   = `eta^2`,
              "omega^2" = `omega^2`,
              "cl"      = cl))
}

#' Cohen's d
#'
#' @param m_0 mean of group 0
#' @param m_1 mean of group 1
#' @param s_0 standard deviation of group 0
#' @param s_1 standard deviation of group 1
#'
#' @return Cohen's d
#' @export
#' 
#' @examples
#' mtcars %>% 
#'   group_by(vs) %>% 
#'   summarize(m = mean(mpg), s = sd(mpg)) %>% 
#'   tidyr::pivot_wider(names_from = vs, values_from = c(m, s)) %>% 
#'   rowwise() %>% 
#'   mutate(cohens_d = es_cohens_d(m_0, m_1, s_0, s_1))
#' 
#' @references 
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences. Routledge.
es_cohens_d <- function(m_0, m_1, s_0, s_1) {
  (m_1 - m_0) / sqrt((s_0^2 + s_1^2) / 2)
}

#' Glass' Delta
#'
#' @param m_0 mean of group 0
#' @param m_1 mean of group 1
#' @param s standard deviation of control group
#'
#' @return Glass' delta
#' @export
#' 
#' @examples
#' mtcars %>% 
#'   group_by(vs) %>% 
#'   summarize(m = mean(mpg), s = sd(mpg)) %>% 
#'   tidyr::pivot_wider(names_from = vs, values_from = c(m, s)) %>% 
#'   rowwise() %>% 
#'   mutate(glass_delta = es_glass_delta(m_0, m_1, s_0))
#' 
#' @references 
#' Hedges, L. V. & Olkin, I. (1985). Statistical methods for meta-analysis. Academic Press.
es_glass_delta <- function(m_0, m_1, s) {
  (m_1 - m_0) / s
}

#' Hedges g
#'
#' @param n_0 number of observations in group 0
#' @param n_1 number of observations in group 1
#' @param m_0 mean of group 0
#' @param m_1 mean of group 1
#' @param s_0 standard deviation of group 0
#' @param s_1 standard deviation of group 1
#'
#' @examples 
#' mtcars %>% 
#'   group_by(vs) %>% 
#'   summarize(n = n(), m = mean(mpg), s = sd(mpg)) %>% 
#'   tidyr::pivot_wider(names_from = vs, values_from = c(n, m, s)) %>% 
#'   rowwise() %>% 
#'   mutate(hedges_g = es_hedges_g(n_0, n_1, m_0, m_1, s_0, s_1))
#' 
#' @return
#' @export
#' 
#' @references 
#' Hedges, L. V. (1981). Distribution theory for Glass' estimator of effect size and related estimators. Journal of Educational Statistics, 6(2), 107-128. https://doi.org/10.3102/10769986006002107
es_hedges_g <- function(n_0, n_1, m_0, m_1, s_0, s_1) {
  (m_1 - m_0) / sqrt(((n_0 - 1) * s_0^2 + (n_1 - 1) * s_1^2) / (n_0 + n_1 - 2))
}
