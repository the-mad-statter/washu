#' WebPlotDigitizer
#' Open the WebPlotDigitizer web app in the default browser.
#' @details It is often necessary to reverse engineer images of data visualizations to extract the underlying numerical data. WebPlotDigitizer is a semi-automated tool that makes this process extremely easy:
#' @export
webplotdigitizer <- function() {
  utils::browseURL("https://automeris.io/WebPlotDigitizer")
}

#' Copy user information from ldap to clipboard
#' Look up a user in ldap by email and copy data to clipboard to paste into the consult database.
#' @param email mail of the user
consult_db_copy_user <- function(email) {
  q <- washu::wu_ldap_query("mail", email)
  v <- c("cn",
         "sn",
         "title",
         "physicalDeliveryOfficeName",
         "telephoneNumber",
         "givenName",
         "displayName",
         "department",
         "streetAddress",
         "personalTitle",
         "name",
         "sAMAccountName",
         "userPrincipalName",
         "mail",
         "eduPersonNickname",
         "eduPersonPrimaryAffiliation")
  for(m in setdiff(v, names(q))) {
    q[m] <- NA
  }
  q <- q[v]
  q$app_role <- "user"
  clipr::write_clip(q)
}

#' Push slashes
#' @param x text to modify
#' @return x with backslashes replaced with forward slashes
push_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

#' Tidy Sub
#' Utility function to make sub() work better with pipes
#' @param x a character vector where matches are sought
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param replacement a replacement for matched pattern in sub and gsub.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is.
#' @param ... additional parameters passed to sub
tidy_sub <- function(x, pattern, replacement, fixed = TRUE, ...) {
  sub(pattern, replacement, x, fixed = fixed, ...)
}

#' Backup value for missing arguments
#' @param x argument to check for missing
#' @param y value to use in case of missing
`%missing%` <- function(x, y) {
  if (rlang::quo_is_missing(x)) y else x
}

#' Protect TeX input
#' @param x TeX to escape
#' @param ... additional arguments
protect_tex_input <- function(x, ...) {
  if (is.character(x) || is.factor(x)) {
    x <- gsub("'([^ ']*)'", "`\\1'", x, useBytes = TRUE)
    x <- gsub("\"([^\"]*)\"", "``\\1''", x, useBytes = TRUE)
    x <- gsub("\\", "\\textbackslash ", x,
              fixed = TRUE,
              useBytes = TRUE
    )
    x <- gsub("([{}&$#_^%])", "\\\\\\1", x, useBytes = TRUE)
    x
  }
  else {
    x
  }
}

#' Embed external pdf in knitr documents
#' @param path pdf path
#' @export
include_pdf <- function(path) {
  paste0(
    "<div class=\"iframe-pdf-container\">",
    "<iframe class=\"iframe-pdf\" src=\"data:application/pdf;base64,%s\" />",
    "</div>"
  ) %>%
    sprintf(base64enc::base64encode(path)) %>%
    knitr::asis_output()
}

#' Purrr-like map
#' @param .x A list or atomic vector
#' @param .f A function to apply
#' @param ... additional arguments
#' @note implemented to avoid dependence on purrr
map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
