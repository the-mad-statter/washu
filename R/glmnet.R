#' Extract coefficients from a glmnet object
#'
#' @param object Fitted "glmnet" model object or a "relaxed" model
#' (which inherits from class "glmnet").
#' @param s Value(s) of the penalty parameter lambda at which predictions are
#' required. Default is the entire sequence used to create the model.
#' @param exact This argument is relevant only when predictions are made at
#' values of s (lambda) different from those used in the fitting of the
#' original model. Not available for "relaxed" objects. If exact=FALSE
#' (default), then the predict function uses linear interpolation to make
#' predictions for values of s (lambda) that do not coincide with those used
#' in the fitting algorithm. While this is often a good approximation, it can
#' sometimes be a bit coarse. With exact=TRUE, these different values of s are
#' merged (and sorted) with object$lambda, and the model is refit before
#' predictions are made. In this case, it is required to supply the original
#' data x= and y= as additional named arguments to predict() or coef(). The
#' workhorse predict.glmnet() needs to update the model, and so needs the data
#' used to create it. The same is true of weights, offset, penalty.factor,
#' lower.limits, upper.limits if these were used in the original call. Failure
#' to do so will result in an error.
#' @param ... additional arguments (ignored)
#'
#' @seealso \link[glmnet]{predict.glmnet}, \link[glmnet]{glmnet}
#'
#' @return a data.frame object
#' @export
coef.glmnet <- function(object, s = NULL, exact = FALSE, ...) {
  glmnet::coef.glmnet(object, s, exact, ...) %>%
    as.matrix() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
}
