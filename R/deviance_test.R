#' Deviance Test
#' @details Perform a deviance test comparing the given model to the null model.
#' @param mdl an object of class "glm", usually, a result of a call to \code{\link[glm]{stats::glm()}}
#' @return object of class "dtest"
#' @export
deviance_test <- function(mdl) {
  robj <- tibble(df = mdl$df.null - mdl$df.residual,
                 chisq = mdl$null.deviance - mdl$deviance,
                 p.value = 1 - pchisq(chisq, df)
  )
  attr(robj, "class") <- append(attr(robj, "class"), "dtest", 0)
  return(robj)
}

#' Print Objects of Type dtest
#' @param x an object of class "dtype", a result of a call to \code{\link[deviance.test]{washu::deviance.test()}}
#' @param format format of the output
#' @param digits number of decimal digits to display
#' @param ... additional parameters passed to other functions
#' @return the argument x, invisibly
#' @export
print.dtest <- function(x, format = c("tibble", "text"), digits = 4, ...) {
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
          "\u03c7\u00b2(%s) = %s, p = %s", 
          x$df, 
          format(round(x$chisq, digits), nsmall = digits), 
          format(round(x$p.value, digits), nsmall = digits)
        ), 
        ...
      )
    }
  )
}
