#' @inherit sjPlot::tab_model
#'
#' @param ... arguments passed on to \link[sjPlot]{tab_model}; notably one or more regression models to summarize
#' @param show.omnibus.f Logical, if TRUE, the omnibus F-test is computed and printed
#' @param show.auc Logical, if TRUE, the AUC is calculated and printed
#' @param show.hosmer_lemeshow Logical, if TRUE, a Hosmer-Lemeshow goodness of fit test is calculated and printed
#' @param show.deviance_test Logical, if TRUE, a deviance-test comparing to the null model is calculated and printed
#' @param footnote optional footnote for table
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
                             footnote = NULL,
                             digits = 2,
                             digits.p = 3,
                             file = NULL,
                             use.viewer = TRUE,
                             print = TRUE) {
  # because models can be passed as separate objects in ... or as a single list, 
  # capture them in a models object, and if we accidentally listed a list, unbox
  # this makes it easier to count and manipulate the models in later code
  models <- list(...)
  if(length(class(models[[1]])) == 1 && class(models[[1]]) == "list") 
    models <- lapply(models[[1]], function(x) x)

  # build base table
  ## invisible(capture.output()) to suppress cat'd profiling message
  invisible(capture.output(
    sjTable <- sjPlot::tab_model(
      ...,
      digits = digits,
      digits.p = digits.p
    )
  ))

  # count columns for later use
  sjTable$page.complete %>%
    xml2::read_html() %>%
    rvest::html_nodes("td.depvarhead") %>%
    length() -> ncols
  ncols_per_model <- (ncols - 1) / length(models)

  # construct 0 to 5 additional rows to append
  new_html <- character(0)

  # 1. row for if omnibus f
  if(show.omnibus.f) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Omnibus F</td>")
    summary_test_obj <- lapply(models, summary)
    summary_test_htm <- vapply(summary_test_obj, function(x) {
      numdf <- x$fstatistic[["numdf"]]
      dendf <- x$fstatistic[["dendf"]]
      fstat <- x$fstatistic[["value"]]
      pvalu <- pf(fstat, numdf, dendf, lower.tail = FALSE)
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"%s\">F(%s, %s) = %s, p = %s</td>",
              ncols_per_model,
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

  # 2. row for if auc
  if(show.auc) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">AUC</td>")
    auc_num <- vapply(models, function(mdl) modEvA::AUC(mdl, plot = FALSE)$AUC, numeric(1))
    auc_htm <- sprintf("    <td class=\"tdata summary summarydata\" colspan=\"%s\">%s</td>",
                       ncols_per_model,
                       format(round(auc_num, digits), nsmall = digits))
    new_html <- c(new_html, auc_htm)
    new_html <- c(new_html, "  </tr>")
  }

  # 3. row for if hosmer lemeshow
  if(show.hosmer_lemeshow) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Hosmer Lemeshow</td>")
    hosmer_lemeshow_test_obj <- lapply(models, function(mdl) vcdExtra::HosmerLemeshow(mdl))
    hosmer_lemeshow_test_htm <- vapply(hosmer_lemeshow_test_obj, function(x) {
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"%s\">&#120536;&sup2;(%s) = %s, p = %s</td>",
              ncols_per_model,
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

  # 4. row for is deviance test
  if(show.deviance_test) {
    new_html <- c(new_html, "  <tr>")
    new_html <- c(new_html, "    <td class=\"tdata leftalign summary\">Deviance</td>")
    deviance_test_obj <- lapply(models, function(mdl) washu::deviance_test(mdl))
    deviance_test_htm <- vapply(deviance_test_obj, function(x) {
      sprintf("    <td class=\"tdata summary summarydata\" colspan=\"%s\">&#120536;&sup2;(%s) = %s, p = %s</td>",
              ncols_per_model,
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

  # 5. footnote
  if(!is.null(footnote)) {
    new_html <- c(
      new_html,
      c(
        "<tfoot>",
        "  <tr>",
        sprintf(
          "    <td colspan=\"%s\" style=\"word-wrap: break-word;\">%s</td>",
          ncols,
          footnote
        ),
        "  </tr>",
        "</tfoot>"
      )
    )
  }

  # append the 0 to 5 additional rows
  page_complete <- sub("</table>", paste(c(new_html, "</table>"), collapse = "\n"), sjTable$page.complete)

  # write the table out if and where specified
  if(is.null(file))
    file <- tempfile(fileext = ".html")
  conn <- file(file)
  writeLines(page_complete, conn)
  close(conn)

  # view output
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

#' Increment regression model table counter
#'
#' @details If using the title argument for sjPlot::tab_model(), the tables are automatically numbered through the use of a css counter. This function can be used in inline R code to insert html code to increment this counter in case a particular table number is desired to be skipped over.
#'
#' @export
sjPlot_tab_model_increment_counter <- function() {
  knitr::asis_output("<div style=\"counter-increment: table;\" />")
}
