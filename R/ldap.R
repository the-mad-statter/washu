#' Remove raw nul
#'
#' Removes nul values from curl response content to allow conversion to a character representation
#'
#' @param data content element from a curl response
#'
#' @return raw content with the nul values deleted
#'
#' @seealso \code{\link{ldap_content}}, \code{\link[base:rawConversion]{rawToChar}}, \code{\link[curl]{curl_fetch}}
rm_raw_nul <- function(data) {
  data[!data=='00']
}

#' LDAP content
#'
#' Coerces ldap curl response content to character vector by splitting on "\\n\\n\\t"
#'
#' @param response curl response object
#' @param delim delimiter
#'
#' @return character vector representation of content
#'
#' @seealso \code{\link{rm_raw_nul}}, \code{\link[base:rawConversion]{rawToChar}}, \code{\link[curl]{curl_fetch}}
ldap_content <- function(response, delim = "\n\n\t") {
  response %>%
    `[[`("content") %>%
    rm_raw_nul() %>%
    rawToChar() %>%
    strsplit(delim) %>%
    `[[`(1)
}

#' Coerce LDAP content to data frame
#'
#' Coerces a character vector of LDAP content containing key-value, colon-delimited pairs into a wide format tibble
#'
#' @param ldap_content content to be coerced
#'
#' @return tibble representing the content
#'
#' @seealso \code{\link{ldap_content}}
ldap_as_tibble <- function(ldap_content) {
  ldap_content %>%
    tibble::as_tibble_col(column_name = "key_value_pairs") %>%
    tidyr::separate(.data$key_value_pairs, c("key", "value"), ": ", extra = "merge") %>%
    tidyr::pivot_wider(names_from = .data$key)
}

#' LDAP clean tibble
#'
#' Normalize LDAP tibble data by re-storing data in tab-delimited variables
#'
#' @param ldap_tibble tibble to clean
#'
#' @return cleaned tibble
#'
#' @details Some LDAP queries tested so far (i.e., sAMAccountName, mail, userPrincipalName) have objectClass
#' data stored in the DN variable, and memberOf data is stored as "\\n\\tmemberOf: "-delimited variable strings.
#' This function extracts and stores this data in tab-delimited variable strings.
#'
#' @seealso \code{\link{ldap_as_tibble}}
ldap_clean_tibble <- function(ldap_tibble) {
  ldap_tibble %>%
    dplyr::mutate(
      objectClass = stringr::str_extract(.data$DN,
                                         stringr::regex("(?<=objectClass: ).+$", dotall = TRUE)),
      objectClass = stringr::str_replace_all(.data$objectClass,
                                             stringr::regex("\n\tobjectClass: ", dotall = TRUE),
                                             "\t"),
      DN = stringr::str_remove(.data$DN, stringr::regex("\n\t.*$", dotall = TRUE)),
      memberOf = stringr::str_replace_all(.data$memberOf,
                                          stringr::regex("\n\tmemberOf: ", dotall = TRUE), "\t"))
}

#' Generic LDAP query
#'
#' Perform generic LDAP queries
#'
#' @param hostname name (or ip address) of the LDAP server
#' @param base_dn distinguished name (DN) of an entry in the directory. This DN identifies the entry that is the starting point of the search. If no base DN is specified, the search starts at the root of the directory tree.
#' @param attributes The attributes to be returned. To specify more than one attribute, use commas to separate the attributes (for example, "cn,mail,telephoneNumber"). If no attributes are specified in the URL, all attributes are returned.
#' @param scope The scope of the search, which can be one of these values:
#'   \itemize{
#'     \item{base}{retrieves information about the distinguished name (base_dn) specified in the URL only.}
#'     \item{one}{retrieves information about entries one level below the distinguished name (base_dn) specified in the URL. The base entry is not included in this scope.}
#'     \item{sub}{retrieves information about entries at all levels below the distinguished name (base_dn) specified in the URL. The base entry is included in this scope.}
#'   }
#' @param filter Search filter to apply to entries within the specified scope of the search.
#' @param user username
#' @param pass password
#' @param protocol connection protocol to use
#' @param port override the default port number (LDAP = 389, LDAPS = 636)
#'
#' @return curl response object
#'
#' @examples
#' \dontrun{
#' ## anonymous default query
#' ldap_query("ldap.example.com")
#'
#' ## look up all attributes for user using authenticated request
#' wustl_key_to_find <- "pooh"
#' user <- "piglet@wustl.edu"
#' pass <- "*****"
#' ldap_query(hostname = "accounts.ad.wustl.edu",
#'            base_dn = "OU=Current,OU=People,DC=accounts,DC=ad,DC=wustl,DC=edu",
#'            scope = "sub",
#'            filter = sprintf("(sAMAccountName=%s)", wustl_key_to_find),
#'            user = user,
#'            pass = pass,
#'            protocol = "ldap")
#' }
ldap_query <- function(hostname,
                       base_dn = "",
                       attributes = "",
                       scope = c("base", "one", "sub"),
                       filter = "(objectClass=*)",
                       user,
                       pass,
                       protocol = c("ldaps", "ldap"),
                       port = c(636, 389)) {
  protocol <- match.arg(protocol)
  port <- ifelse(!missing(port), sprintf(":%i", port), "")
  attributes <- paste(attributes, collapse = ",")
  scope <- match.arg(scope)

  url = sprintf("%s://%s%s/%s?%s?%s?%s",
                 protocol,
                 hostname,
                 port,
                 base_dn,
                 attributes,
                 scope,
                 filter)

  h <- curl::new_handle()
  if(!missing(user) & !missing(pass))
    h <- curl::handle_setopt(h, userpwd = sprintf("%s:%s", user, pass))

  curl::curl_fetch_memory(url, h)
}

#' WU LDAP query
#'
#' Default values for WU LDAP queries
#'
#' @param hostname name (or ip address) of the LDAP server
#' @param base_dn distinguished name (DN) of an entry in the directory. This DN identifies the entry that is the starting point of the search. If no base DN is specified, the search starts at the root of the directory tree.
#' @param attributes The attributes to be returned. To specify more than one attribute, use commas to separate the attributes (for example, "cn,mail,telephoneNumber"). If no attributes are specified in the URL, all attributes are returned.
#' @param scope The scope of the search, which can be one of these values:
#'   \itemize{
#'     \item{base}{retrieves information about the distinguished name (base_dn) specified in the URL only.}
#'     \item{one}{retrieves information about entries one level below the distinguished name (base_dn) specified in the URL. The base entry is not included in this scope.}
#'     \item{sub}{retrieves information about entries at all levels below the distinguished name (base_dn) specified in the URL. The base entry is included in this scope.}
#'   }
#' @param filter Search filter to apply to entries within the specified scope of the search.
#' @param user WUSTL Key username
#' @param pass WUSTL Key pass
#'
#' @return curl response object
#'
#' @seealso \code{\link{ldap_query}}
wu_ldap_query_default <- function(hostname = "accounts.ad.wustl.edu",
                                  base_dn = "OU=Current,OU=People,DC=accounts,DC=ad,DC=wustl,DC=edu",
                                  attributes = "",
                                  scope = "sub",
                                  filter = "(objectClass=*)",
                                  user = sprintf("%s@wustl.edu", Sys.getenv("WUSTL_KEY_USER")),
                                  pass = Sys.getenv("WUSTL_KEY_PASS")) {
  ldap_query(hostname,
             base_dn,
             attributes,
             scope,
             filter,
             user,
             pass,
             protocol = "ldap")
}

#' WU LDAP query for sAMAccountName
#'
#' Search WU LDAP for a given sAMAccountName
#'
#' @param sAMAccountName sAMAccountName to search for
#' @param user searcher username
#' @param pass searcher password
#'
#' @return tibble of all attributes for given sAMAccountName
wu_ldap_query_sAMAccountName <- function(sAMAccountName, user, pass) {
  wu_ldap_query_default(
    filter = sprintf("(sAMAccountName=%s)", sAMAccountName),
    user = user,
    pass = pass) %>%
    ldap_content() %>%
    ldap_as_tibble() %>%
    ldap_clean_tibble()
}

#' WU LDAP query for mail
#'
#' Search WU LDAP for a given mail
#'
#' @param mail mail to search for
#' @param user searcher username
#' @param pass searcher password
#'
#' @return tibble of all attributes for given mail
wu_ldap_query_mail <- function(mail, user, pass) {
  wu_ldap_query_default(
    filter = sprintf("(mail=%s)", mail),
    user = user,
    pass = pass) %>%
    ldap_content() %>%
    ldap_as_tibble() %>%
    ldap_clean_tibble()
}

#' WU LDAP query for userPrincipalName
#'
#' Search WU LDAP for a given userPrincipalName
#'
#' @param userPrincipalName userPrincipalName to search for
#' @param user searcher username
#' @param pass searcher password
#'
#' @return tibble of all attributes for given userPrincipalName
wu_ldap_query_userPrincipalName <- function(userPrincipalName, user, pass) {
  wu_ldap_query_default(
    filter = sprintf("(userPrincipalName=%s)", userPrincipalName),
    user = user,
    pass = pass) %>%
    ldap_content() %>%
    ldap_as_tibble() %>%
    ldap_clean_tibble()
}

#' WU LDAP query
#'
#' Perform WU LDAP queries
#'
#' @param type attribute to query
#' @param value value to match
#' @param user searcher username
#' @param pass searcher password
#' @param ... arguments to override defaults of \code{\link{wu_ldap_query_default}}
#'
#' @return tibble of all attributes for given attribute-value pair
#'
#' @export
#'
#' @details type = "custom" allows for custom searches, but requires post-processing.
#'
#' @examples
#' \dontrun{
#' ## piglet searches for pooh
#' wu_ldap_query("mail", "pooh@wustl.edu", "piglet@wustl.edu", "*****")
#'
#' ## custom search for all Research Statisticians
#' wu_ldap_query(type = "custom",
#'               user = "piglet@wustl.edu",
#'               pass = "*****",
#'               attributes = "mail",
#'               filter = sprintf("(title=%s)", "Research Statistician")) %>%
#'   ldap_content("\n\n") %>%
#'   gsub("^.+\n\tmail: ", "", .)
#' }
wu_ldap_query <- function(type = c("sAMAccountName",
                                   "userPrincipalName",
                                   "mail",
                                   "custom"),
                          value,
                          user = sprintf("%s@wustl.edu", Sys.getenv("WUSTL_KEY_USER")),
                          pass = Sys.getenv("WUSTL_KEY_PASS"),
                          ...) {
  type <- match.arg(type)
  switch(type,
         "sAMAccountName"    = wu_ldap_query_sAMAccountName(value, user, pass),
         "userPrincipalName" = wu_ldap_query_userPrincipalName(value, user, pass),
         "mail"              = wu_ldap_query_mail(value, user, pass),
         "custom"            = wu_ldap_query_default(user = user, pass = pass, ...))
}
