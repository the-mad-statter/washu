#' Logistic Regression
#' @description Fit a logistic regression model.
#' @inheritParams stats::glm
#' @export
lr <- function(formula, data, ...) {
  glm(formula, binomial, data, ...)
}

#' Deviance Test
#' @details Perform a deviance test comparing the given model to the null model.
#' @param x an object of class "glm", usually, a result of a call to \code{\link[glm]{stats::glm()}}
#' @return object of class "dtest"
#' @export
deviance_test <- function(x) {
  robj <- dplyr::tibble(df = x$df.null - x$df.residual,
                        chisq = x$null.deviance - x$deviance,
                        p.value = 1 - pchisq(chisq, df)
  )
  attr(robj, "class") <- append(attr(robj, "class"), "dtest", 0)
  return(robj)
}

#' Print Objects of Type dtest
#' @param x an object of class "dtype", a result of a call to \code{\link[deviance.test]{washu::deviance.test()}}
#' @param format format of the output
#' @param digits if format is text, default decimal digits to print
#' @param digits.df if format is text, df decimal digits to print
#' @param digits.stat if format is text, test statistic decimal digits to print
#' @param digits.p if format is text, p-value decimal digits to print
#' @param ... additional parameters passed to other functions
#' @export
print.dtest <- function(x, 
                        format = c("tibble", "text"), 
                        digits = 4, 
                        digits.df = 0, 
                        digits.stat = digits, 
                        digits.p = digits, 
                        ...) {
  format <- match.arg(format)
  switch (
    format,
    "tibble" = {
      attr(x, "class") <- attr(x, "class")[-1]
      print(x, ...)
    },
    "text" = {
      print(
        sprintf(
          "\u03c7\u00b2(%.*f) = %.*f, p = %s", 
          digits.df, x$df, 
          digits.stat, x$chisq, 
          sub("0.", ".", sprintf("%.*f", digits.p, x$p.value))
        ),
        ...
      )
    }
  )
}

#' Print Objects of Type HLtest
#' @param x an object of class "HLtest", a result of a call to \code{\link[HosmerLemeshow]{vcdExtra::HosmerLemeshow()}}
#' @param format format of the output
#' @param digits if format is text, default decimal digits to print
#' @param digits.df if format is text, df decimal digits to print
#' @param digits.stat if format is text, test statistic decimal digits to print
#' @param digits.p if format is text, p-value decimal digits to print
#' @param ... additional parameters passed to other functions
#' @export
print.HLtest <- function(x, 
                         format = c("default", "tibble", "text"), 
                         digits = 4, 
                         digits.df = 0, 
                         digits.stat = digits, 
                         digits.p = digits, 
                         ...) {
  format <- match.arg(format)
  switch (
    format,
    "default" = {
      vcdExtra:::print.HLtest(x, ...)
    },
    "tibble" = {
      tibble:::print.tbl_df(dplyr::tibble(df = x$df, chisq = x$chisq, p.value = x$p.value), ...)
    },
    "text" = {
      print(
        sprintf(
          "\u03c7\u00b2(%.*f) = %.*f, p = %s", 
          digits.df, x$df, 
          digits.stat, x$chisq, 
          sub("0.", ".", sprintf("%.*f", digits.p, x$p.value))
        ),
        ...
      )
    }
  )
}

#' Compute the model's R2
#' @inheritParams performance::r2
#' @note This is a wrapper around \code{\link[r2]{performance::r2()}} for a consistent class for printing
#' @export
r2 <- function(model, ...) {
  r_obj <- performance::r2(model, ...)
  attr(r_obj, "class") <- append(attr(r_obj, "class"), "r_sqr", 0)
  return(r_obj)
}

#' Print Objects of Type r_sqr
#' @param x an object of class "r_sqr", a result of a call to \code{\link[r2]{washu::r2()}}
#' @param format format of the output
#' @param digits.r2 if format is text, default decimal digits to print
#' @param ... additional parameters passed to other functions
#' @export
print.r_sqr <- function(x, 
                        format = c("default", "tibble", "text"), 
                        digits.r2 = 4, 
                        ...) {
  format <- match.arg(format)
  switch (
    format,
    "default" = {
      attr(x, "class") <- attr(x, "class")[-1]
      print(x, digits = digits.r2, ...)
    },
    "tibble" = {
      attr(x, "class") <- "list"
      print(dplyr::as_tibble(x), ...)
    },
    "text" = {
      labs <- gsub("([\\w])([\\w]+)_([\\w])([\\w]+)", 
                   "\\U\\3\\L\\4 \\U\\1\\L\\2", 
                   names(x), 
                   perl = TRUE
              )
      labs <- sub("R2", "R\u00b2", labs)
      vals <- sub("0.", ".", sprintf("%.*f", digits.r2, x))
      print(paste(labs, vals, sep = " = "), ...)
    }
  )
}

#' Print Objects of Type glm
#' @param x an object of class "glm", a result of a call to \code{\link[glm]{stats::glm()}}
#' @param format format of the output
#' @param sig.level significance level
#' @param ... additional parameters passed to other functions
#' @export
print.glm <- function(x, 
                      format = c("default", "text"),
                      sig.level = 0.05, 
                      ...) {
  format <- match.arg(format)
  switch (
    format,
    "default" = {
      stats:::print.glm(x, ...)
    },
    "text" = {
      if(x$family$family == "binomial") {
        sink(tempfile())
        r2_obj <- r2(x)
        r2_str <- print(r2_obj, "text", ...)
        dt_obj <- deviance_test(x)
        dt_str <- print(dt_obj, "text", ...)
        g <- ifelse(is.null(list(...)$g), 10, list(...)$g)
        hl_obj <- vcdExtra::HosmerLemeshow(x, g)
        hl_str <- print(hl_obj, "text", ...)
        sink()
        sprintf("The model %s significantly better than the null model, %s, %s, and a Hosmer-Lemeshow test suggested %s fit, %s.",
                ifelse(dt_obj$p.value < sig.level, "was", "was not"),
                r2_str,
                dt_str,
                ifelse(hl_obj$p.value < sig.level, "lack of", "adequate"),
                hl_str
        )
      } else {
        sprintf("Do not yet know how to report models of family %s.", x$family$family)
      }
    }
  )
}
