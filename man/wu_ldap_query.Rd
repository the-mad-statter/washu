% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldap.R
\name{wu_ldap_query}
\alias{wu_ldap_query}
\title{WU LDAP query}
\usage{
wu_ldap_query(
  type = c("sAMAccountName", "userPrincipalName", "mail", "custom"),
  value,
  user = sprintf("\%s@wustl.edu", Sys.getenv("WUSTL_KEY_USER")),
  pass = Sys.getenv("WUSTL_KEY_PASS"),
  ...
)
}
\arguments{
\item{type}{attribute to query}

\item{value}{value to match}

\item{user}{searcher username}

\item{pass}{searcher password}

\item{...}{arguments to override defaults of \code{\link{wu_ldap_query_default}}}
}
\value{
tibble of all attributes for given attribute-value pair
}
\description{
Perform WU LDAP queries
}
\details{
type = "custom" allows for custom searches, but requires post-processing.
}
\examples{
\dontrun{
## piglet searches for pooh
wu_ldap_query("mail", "pooh@wustl.edu", "piglet@wustl.edu", "*****")

## custom search for all Research Statisticians
wu_ldap_query(type = "custom",
              user = "piglet@wustl.edu",
              pass = "*****",
              attributes = "mail",
              filter = sprintf("(title=\%s)", "Research Statistician")) \%>\%
  ldap_content("\n\n") \%>\%
  gsub("^.+\n\tmail: ", "", .)
}
}
