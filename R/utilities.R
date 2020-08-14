#' WebPlotDigitizer
#' Open the WebPlotDigitizer web app in the default browser.
#' @details It is often necessary to reverse engineer images of data visualizations to extract the underlying numerical data. WebPlotDigitizer is a semi-automated tool that makes this process extremely easy:
#' @export
webplotdigitizer <- function() {
  browseURL("https://automeris.io/WebPlotDigitizer")
}

#' Copy user information from ldap to clipboard
#' Look up a user in ldap by email and copy data to clipboard to paste into the consult database.
#' @param email mail of the user
consult_db_copy_user <- function(email) {
  q <- wu_ldap_query("mail", email)
  v <- c("cn", "sn", "title", "physicalDeliveryOfficeName", "telephoneNumber", "givenName", "displayName", "department", "streetAddress", "personalTitle", "name", "sAMAccountName", "userPrincipalName", "mail", "eduPersonNickname", "eduPersonPrimaryAffiliation")
  clipr::write_clip(q[v])
}
