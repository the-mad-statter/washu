#' Tidy risk ratio estimation and confidence intervals
#'
#' @param .data A data frame or data frame extension (e.g. a tibble).
#' @param outcome Name of variable containing dichotomous outcome status.
#' @param exposure Name of exposure variable containing exposure status.
#' @param na.rm A logical value indicating whether to remove counted NA values.
#' @param ... Arguments passed to \code{\link[epitools]{riskratio}}.
#'
#' @note If outcome and exposure are not of type factor, the function will
#' convert them to factors with default levels.
#'
#' @return A tidy tibble of the output from \code{\link[epitools]{riskratio}}.
#' @export
#'
#' @examples
#' ##Selvin 1998, p. 289
#' sel <- matrix(c(178, 79, 1411, 1486), 2, 2)
#' dimnames(sel) <- list("Behavior type" = c("Type A", "Type B"),
#'                       "Outcome" = c("CHD", "No CHD")
#' )
#' epitools::riskratio.boot(sel, rev = "b")
#'
#' bind_rows(
#'   tibble(behavior = "Type B", chd =  "no", n = 1486),
#'   tibble(behavior = "Type A", chd =  "no", n = 1411),
#'   tibble(behavior = "Type B", chd = "yes", n =   79),
#'   tibble(behavior = "Type A", chd = "yes", n =  178)
#' ) %>%
#'   tidyr::uncount() %>%
#'   mutate(
#'     behavior = factor(behavior, levels = c("Type B", "Type A")),
#'     chd = factor(chd, levels = c("no", "yes"))
#'   ) %>%
#'   epitools_riskratio(chd, behavior)
epitools_riskratio <- function(.data, outcome, exposure, na.rm = FALSE, ...) {
  outcome <- rlang::ensym(outcome)
  exposure <- rlang::ensym(exposure)

  if(!is.factor(dplyr::select(.data, !!outcome)))
    .data <- mutate(.data, !!outcome := factor(!!outcome))

  if(!is.factor(dplyr::select(.data, !!exposure)))
    .data <- mutate(.data, !!exposure := factor(!!exposure))

  dplyr::count(.data, !!outcome, !!exposure) -> m

  if(na.rm)
    tidyr::drop_na(m) -> m
  else {
    if(!all(complete.cases(m)))
      stop(
        sprintf(
          "Counting by %s and %s resulted in NAs.",
          rlang::as_string(outcome),
          rlang::as_string(exposure)
        ),
        " ",
        "Check the data or set `na.rm = TRUE`."
      )
  }

  dplyr::pull(m, n) %>% matrix(ncol = 2) -> m

  dn <- list(levels(pull(.data, !!exposure)), levels(pull(.data, !!outcome)))
  names(dn) <- c(rlang::as_string(exposure), rlang::as_string(outcome))
  dimnames(m) <- dn

  epitools::riskratio(m, ...) -> rr

  dplyr::bind_cols(
    tibble::tibble(
      exposure = rownames(rr$measure),
      outcome = rlang::as_string(outcome)
    ),
    tibble::as_tibble(rr$data)[-dim(rr$data)[1], - dim(rr$data)[2]],
    tibble::as_tibble(rr$measure),
    tibble::as_tibble(rr$p.value),
    tibble::tibble(
      correction = rr$correction,
      method = attr(rr, "method")
    )
  )
}
