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
#' @references
#' Steve Selvin (1998), Modern Applied Biostatistical Methods Using S-Plus, 1st Edition, Oxford University Press
#'
#' @examples
#' ## Selvin 1998, p. 289
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

#' Tidy odds ratio estimation and confidence intervals
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param outcome Name of variable containing dichotomous outcome status.
#' @param exposure Name of exposure variable containing exposure status.
#' @param na.rm A logical value indicating whether to remove counted NA values.
#' @param ... Arguments passed to \code{\link[epitools]{oddsratio}}.
#'
#' @note If outcome and exposure are not of type factor, the function will
#' convert them to factors with default levels.
#'
#' @return A tidy tibble of the output from \code{\link[epitools]{oddsratio}}.
#' @export
#'
#' @examples
#' ## Case-control study assessing whether exposure to tap water
#' ## is associated with cryptosporidiosis among AIDS patients
#' tapw <- c("Lowest", "Intermediate", "Highest")
#' outc <- c("Case", "Control")
#' dat <- matrix(c(2, 29, 35, 64, 12, 6), 3, 2, byrow = TRUE)
#' dimnames(dat) <- list("Tap water exposure" = tapw, "Outcome" = outc)
#' epitools::oddsratio(dat, rev = "c")
#'
#' tribble(
#'   ~tapw, ~outc, ~n,
#'   'lowest', 'case', 2,
#'   'lowest', 'control', 29,
#'   'intermediate', 'case', 35,
#'   'intermediate', 'control', 64,
#'   'highest', 'case', 12,
#'   'highest', 'control', 6
#' ) %>%
#'   tidyr::uncount() %>%
#'   mutate(
#'     tapw = factor(tapw, levels = c('lowest', 'intermediate', 'highest')),
#'     outc = factor(outc, levels = c('control', 'case'))
#'   ) %>%
#'   epitools_oddsratio(outc, tapw)
epitools_oddsratio <- function(data, outcome, exposure, na.rm = FALSE, ...) {
  outcome <- rlang::ensym(outcome)
  exposure <- rlang::ensym(exposure)

  if(!is.factor(dplyr::select(data, !!outcome)))
    data <- dplyr::mutate(data, !!outcome := factor(!!outcome))

  if(!is.factor(dplyr::select(data, !!exposure)))
    data <- dplyr::mutate(data, !!exposure := factor(!!exposure))

  dplyr::count(data, !!outcome, !!exposure) -> m

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

  dn <- list(levels(dplyr::pull(data, !!exposure)), levels(dplyr::pull(data, !!outcome)))
  names(dn) <- c(rlang::as_string(exposure), rlang::as_string(outcome))
  dimnames(m) <- dn

  or <- epitools::oddsratio(m, ...)

  dplyr::bind_cols(
    tibble::tibble(
      exposure = rownames(or$measure),
      outcome = rlang::as_string(outcome)
    ),
    tibble::as_tibble(or$data)[-dim(or$data)[1], - dim(or$data)[2]],
    tibble::as_tibble(or$measure),
    tibble::as_tibble(or$p.value),
    tibble::tibble(
      correction = or$correction,
      method = attr(or, "method")
    )
  )
}
