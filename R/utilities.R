#' WebPlotDigitizer
#' @description Open the WebPlotDigitizer web app in the default browser.
#' @details It is often necessary to reverse engineer images of data
#' visualizations to extract the underlying numerical data. WebPlotDigitizer is
#' a semi-automated tool that makes this process extremely easy:
#' @export
webplotdigitizer <- function() {
  utils::browseURL("https://automeris.io/WebPlotDigitizer")
}

#' Copy user information from ldap to clipboard
#' @description Look up a user in ldap by email and copy data to clipboard to
#' paste into the consult database.
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
#' @description Utility function to make sub() work better with pipes
#' @param x a character vector where matches are sought
#' @param pattern character string containing a regular expression (or character
#'  string for fixed = TRUE) to be matched in the given character vector.
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

#' Centered sequence generation
#' @description Generate regular sequences based on a central value
#' @param center the middle value of the sequence
#' @param by number: decrement/increment of the sequence
#' @param length.out desired length of the sequence
#' @details Generates a sequence of numbers above and below a central value.
#'
#' The length.out argument should be an odd integer.
#' If the value given is fractional, it will be rounded up.
#' If the value given is even, it will be made odd by adding one.
#' @return vector of type integer or double
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S
#' Language. Wadsworth & Brooks/Cole.
#' @seealso \code{\link[base]{seq}},
#' @export
#' @examples
#' # integer center, integer by, odd length.out
#' cseq(center = 10, by = 2, length.out = 5)
#'
#' # integer center, integer by, even length.out
#' # same as previous because even length.out is made odd by adding one
#' cseq(center = 10, by = 2, length.out = 4)
#'
#' # double center, double by, odd length.out
#' # doubles also possible
#' cseq(center = 10.5, by = 2.25, length.out = 7)
cseq <- function(center, by, length.out = 3) {
  n.steps <- floor(ceiling(length.out) / 2)
  from <- center - n.steps * by
  to <- center + n.steps * by
  seq(from, to, by)
}

#' Not In
#' @description Return a logical vector indicating if there is not a match or
#' is for its left operand
#' @param x vector or NULL: the values to be matched
#' @param table vector or NULL: the values to be matched against
#' @return A logical vector, indicating if a match was not located for each
#' element of x: thus the values are TRUE or FALSE and never NA
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S
#' Language. Wadsworth & Brooks/Cole.
#' @seealso \code{\link[base]{match}}
#' @export
"%notin%" <- function(x, table) {
  match(x, table, nomatch = 0) == 0
}

#' Default in the event of NULL arguments
#' @param x argument to check if null
#' @param default value to supply if argument is null
#' @return original argument if not null; default otherwise
#' @export
match_arg <- function(x, default) {
  ifelse(is.null(x), default, x)
}

#' Send an email message via the Mailgun API
#' @param to email address of recipient
#' @param from name of sender (but will come from mailgun@domain_name)
#' @param subject subject of message
#' @param html html body of message
#' @param text text body of message
#' @param domain_name Mailgun domain name (e.g., sandbox<sandbox>.mailgun.org)
#' @param api_key Mailgun API password
#' @param ssl_verifypeer whether to verify peer
#' @return TRUE if successful; error otherwise
#' @examples
#' \dontrun{
#' ## Retrieve Mailgun domain name and API key from .Renviron file
#' send_mailgun(to = "robin@wustl.edu",
#'              from = "Robin",
#'              subject = "Robin Says Hi!",
#'              html = "<p>Hi Robin,</p><p>Robin</p>")
#' }
#' @export
#' @seealso \code{\link[httr]{POST}}, \code{\link[httr]{set_config}}
#' @references \href{https://documentation.mailgun.com/en/latest/}{Mailgun API Documentation}
send_mailgun <- function(to = "robin@wustl.edu",
                         from = "Excited User",
                         subject = "Mailgun Test",
                         html = "<div>Test</div>",
                         text = html,
                         domain_name = Sys.getenv("MAILGUN_DOMAIN_NAME"),
                         api_key = Sys.getenv("MAILGUN_API_KEY"),
                         ssl_verifypeer = TRUE) {
  httr::POST(
    url = sprintf("https://api.mailgun.net/v3/%s/messages", domain_name),
    config = c(
      httr::authenticate("api", api_key),
      httr::config(ssl_verifypeer = ssl_verifypeer)
    ),
    body = list(
      from = sprintf("%s <mailgun@%s>", from, domain_name),
      to = to,
      subject = subject,
      html = html,
      text = text
    ),
    encode = "multipart") %>%
    httr::stop_for_status()

  return(TRUE)
}

#' Build and check a package, cleaning up automatically on success.
#' @inheritParams devtools::check
#' @param ... additional parameters passed to \code{\link[devtools]{check}}
#' @note Adds environment flag to skip time check as per
#' \href{https://stat.ethz.ch/pipermail/r-package-devel/2019q1/003577.html}{r-package-devel}.
#' @export
check <- function(document = FALSE,
                  env_vars = c("_R_CHECK_SYSTEM_CLOCK_" = FALSE),
                  ...) {
    devtools::check(document = document, env_vars = env_vars, ...)
}
