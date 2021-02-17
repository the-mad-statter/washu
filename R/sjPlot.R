#' @inherit sjPlot::tab_model
#'
#' @param ... arguments passed on to \link[sjPlot]{tab_model}; notably one or more regression models to summarize
#' @param show.omnibus.f Logical, if TRUE, the omnibus F-test is computed and printed
#' @param show.auc Logical, if TRUE, the AUC is calculated and printed
#' @param show.hosmer_lemeshow Logical, if TRUE, a Hosmer-Lemeshow goodness of fit test is calculated and printed
#' @param show.deviance_test Logical, if TRUE, a deviance-test comparing to the null model is calculated and printed
#' @param print Logical, if TRUE in non-interactive mode, the html table code is inserted into the document
#'
#' @export
#'
#' @references
#' Hosmer, David W.; Lemeshow, Stanley (2013). Applied Logistic Regression. New York: Wiley.
sjPlot_tab_model <- function(...,
                             show.omnibus.f = FALSE,
                             show.auc = FALSE,
                             show.hosmer_lemeshow = FALSE,
                             show.deviance_test = FALSE,
                             digits = 2,
                             digits.p = 3,
                             file = NULL,
                             use.viewer = TRUE,
                             print = TRUE) {
  all_args <- append(list(...), list(ignore_me = 1))
  unnamed_args <- all_args[names(all_args) == ""]

  sjTable <- sjPlot::tab_model(
    ...,
    digits = digits,
    digits.p = digits.p
  )

  new_html <- character(0)

  if(show.omnibus.f) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Omnibus F</td>")
    summary_test_obj <- lapply(unnamed_args, summary)
    summary_test_htm <- vapply(summary_test_obj, function(x) {
      numdf <- x$fstatistic[["numdf"]]
      dendf <- x$fstatistic[["dendf"]]
      fstat <- x$fstatistic[["value"]]
      pvalu <- pf(fstat, numdf, dendf, lower.tail = FALSE)
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"3\">F(%s, %s) = %s, p = %s</td>",
              numdf,
              dendf,
              format(round(fstat, digits), nsmall = digits),
              format(round(pvalu, digits.p), nsmall = digits.p)
      )
    },
    character(1)
    )
    new_html <- c(new_html, summary_test_htm)
    new_html <- c(new_html, "  </tr>")
  }

  if(show.auc) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">AUC</td>")
    auc_num <- vapply(unnamed_args, function(mdl) modEvA::AUC(mdl, plot = FALSE)$AUC, numeric(1))
    auc_htm <- sprintf("    <td class=\"tdata summary summarydata\" colspan=\"3\">%s</td>",
                       format(round(auc_num, digits), nsmall = digits))
    new_html <- c(new_html, auc_htm)
    new_html <- c(new_html, "  </tr>")
  }

  if(show.hosmer_lemeshow) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Hosmer Lemeshow</td>")
    hosmer_lemeshow_test_obj <- lapply(unnamed_args, function(mdl) vcdExtra::HosmerLemeshow(mdl))
    hosmer_lemeshow_test_htm <- vapply(hosmer_lemeshow_test_obj, function(x) {
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"3\">&#120536;&sup2;(%s) = %s, p = %s</td>",
              x$df,
              format(round(x$chisq, digits), nsmall = digits),
              format(round(x$p.value, digits.p), nsmall = digits.p)
      )
    },
    character(1)
    )
    new_html <- c(new_html, hosmer_lemeshow_test_htm)
    new_html <- c(new_html, "  </tr>")
  }

  if(show.deviance_test) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Deviance</td>")
    deviance_test_obj <- lapply(unnamed_args, function(mdl) washu::deviance_test(mdl))
    deviance_test_htm <- vapply(deviance_test_obj, function(x) {
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"3\">&#120536;&sup2;(%s) = %s, p = %s</td>",
              x$df,
              format(round(x$chisq, digits), nsmall = digits),
              format(round(x$p.value, digits.p), nsmall = digits.p)
      )
    },
    character(1)
    )
    new_html <- c(new_html, deviance_test_htm)
    new_html <- c(new_html, "  </tr>")
  }
  new_html <- c(new_html, "</table>")

  page_complete <- sub("</table>", paste(new_html, collapse = "\n"), sjTable$page.complete)

  if(is.null(file))
    file <- tempfile(fileext = ".html")
  conn <- file(file)
  writeLines(page_complete, conn)
  close(conn)

  if(interactive()) {
    if(use.viewer) {
      # need to ensure viewer file is temp file for viewer to show
      # also needs .html extension to be rendered as html
      file <- tempfile(fileext = ".html")
      conn <- file(file)
      writeLines(page_complete, conn)
      close(conn)
      rstudioapi::viewer(file)
    } else {
      # a file was written somewhere so we can just use it for browser
      browseURL(file)
    }
  } else {
    if(print)
      htmltools::includeHTML(file)
  }
}

#' Print a fitted model as HTML ANOVA table
#'
#' @param mdl a model returned by a model fitting function (e.g., lm or glm)
#' @param title character, will be used as table caption.
#' @param pred.labels character vector with labels of predictor varaibles
#' @param full_width logical controlling whether the html table should have 100\% width
#' @param digits amount of decimals for numbers
#' @param scientific logical specifying whether to encode numbers in scientific format
#'
#' @return a character vector of the table source code
#' @export
#'
#' @examples
#' m <- lm(mpg ~ cyl, mtcars)
#' tab_anova(m)
#' 
#' @seealso \link[sjPlot]{tab_model}, \link[stats]{anova}, \link[base]{format}, \link[kableExtra]{kbl}, \link[kableExtra]{kable_classic}
tab_anova <- function(mdl, 
                      title = NULL, 
                      pred.labels = NULL, 
                      full_width = NULL, # kable_styling
                      digits = getOption("digits"), 
                      scientific = FALSE,
                      ...) {
  mdl %>% 
    anova() %>% 
    dplyr::as_tibble() %>% 
    janitor::clean_names() %>% 
    washu::add_significance_flag(pr_f) %>% 
    dplyr::mutate(
      dplyr::across(
        c(tidyselect:::where(is.numeric), -df),
        ~ format(round(., digits), nsmall = digits, scientific = scientific)
      ), 
      dplyr::across(
        .fns = trimws
      ), 
      dplyr::across(
        .fns = ~ dplyr::if_else(. == "NA", "", .)
      ),
      predictor = rownames(anova(mdl)), 
      p = paste0(pr_f, p_flag)
    ) %>% 
    dplyr::select(predictor, sum_sq, dplyr::everything(), -c(pr_f, p_flag)) -> tbl
  
  if(!is.null(pred.labels)) {
    tbl %>% 
      mutate(predictor = dplyr::all_of(pred.labels)) -> tbl
  }
  
  tbl %>% 
    kableExtra::kbl(col.names = c("Predictor", "SS", "df", "MS", "F", "p"), caption = title, ...) %>% 
    kableExtra::kable_classic(full_width = full_width, ...)
}
