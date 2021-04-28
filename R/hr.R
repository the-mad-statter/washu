#' Convert hazard ratio to beta coefficient
#'
#' @param hr hazard ratio
#' @param se standard error of the hazard ratio
#'
#' @return named list containing estimates for the beta coefficient and associated standard error
#' @export
#' 
#' @examples
#' coef_convert_hr_to_beta(1.3, .01)
coef_convert_hr_to_beta <- function(hr, se) {
  pivot_value <- qnorm(0.025, lower.tail = FALSE)
  moe <- pivot_value * se
  lwr <- hr - moe
  upr <- hr + moe
  # convert hr ci to beta scale; then back compute beta standard error
  list(
    beta = log(hr), 
    se = (log(upr) - log(lwr)) / 2 * pivot_value
  )
}

# test difference of two beta coefficients
# based on remotes::install_bioc("survcomp")


#' Hazard ratio test
#'
#' @param beta1 beta estimate for the first hazard ratio
#' @param se1 standard error of beta estimate for the first hazard ratio
#' @param beta2 standard error of beta estimate for the second hazard ratio
#' @param se2 standard error of beta estimate for the second hazard ratio
#' @param r correlation between the first and second risk scores (i.e., predictors)
#' @param n number of samples from which the hazard ratios were estimated
#'
#' @return A list with class "htest" containing the following components:
#' \tabular{rr}{
#' statistic \tab the value of the t-statistic. \cr
#' parameter \tab the degrees of freedom for the t-statistic. \cr
#' p.value \tab the p-value for the test. \cr
#' conf.int \tab a confidence interval for the mean appropriate to the specified alternative hypothesis. \cr
#' estimate \tab the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test. \cr
#' null.value \tab the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test. \cr
#' stderr \tab the standard error of the mean (difference), used as denominator in the t-statistic formula. \cr
#' alternative \tab a character string describing the alternative hypothesis. \cr
#' method \tab a character string indicating what type of t-test was performed. \cr
#' data.name \tab a character string giving the name(s) of the data.
#' }
#' 
#' @export
#' 
#' @note Currently only performs a directional test
#' @seealso \code{\link[survcomp]{hr.comp2}}
#'
#' @examples
#' set.seed(12345)
#' age <- as.numeric(rnorm(100, 50, 10) >= 50)
#' size <- as.numeric(rexp(100,1) > 1)
#' stime <- rexp(100)
#' cens <- runif(100,.5,2)
#' sevent <- as.numeric(stime <= cens)
#' stime <- pmin(stime, cens)
#' coxm1 <- survival::coxph(survival::Surv(stime, sevent) ~ age)
#' coxm2 <- survival::coxph(survival::Surv(stime, sevent) ~ size)
#' hr.test(coxm1$coefficients, 
#'         drop(sqrt(coxm1$var)), 
#'         coxm2$coefficients, 
#'         drop(sqrt(coxm2$var)), 
#'         cor(age, size), 
#'         100
#' )
hr.test <- function (beta1, se1, beta2, se2, r = 0, n = 10000) {
  d <- beta1 - beta2
  se <- sqrt(se1^2 + se2^2 - 2 * r * se1 * se2)
  t <- d / se
  df <- n - 1
  p <- 1 - pt(t, df)
  
  ci <- c(d - qt(0.05, df, lower.tail = FALSE) * se, Inf)
  attr(ci, "conf.level") <- 0.95
  
  htest <- list(
    statistic = c(t = t),
    parameter = c(df = df),
    p.value = p,
    conf.int = ci,
    estimate = c("difference of the betas" = d),
    null.value = c("difference in betas" = 0),
    stderr = se,
    alternative = "greater",
    method = "Paired t-test of beta coefficients",
    data.name = sprintf("\n beta1 = %s\n se1   = %s\n beta2 = %s\n se2   = %s\n r     = %s", 
                        beta1, se1, beta2, se2, r)
  )
  
  attr(htest, "class") <- "htest"
  
  htest
}
