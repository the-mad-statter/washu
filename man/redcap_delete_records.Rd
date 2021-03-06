% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redcapr.R
\name{redcap_delete_records}
\alias{redcap_delete_records}
\title{Delete REDCap records}
\usage{
redcap_delete_records(redcap_uri, token, records)
}
\arguments{
\item{redcap_uri}{The URI (uniform resource identifier) of the REDCap project. Required.}

\item{token}{The user-specific string that serves as the password for a project. Required.}

\item{records}{Vector of record ids to delete. Required.}
}
\value{
A \link[httr]{response} object
}
\description{
Delete REDCap records
}
\examples{
\dontrun{
creds <- retrieve_credential_local(1234)
redcap_delete_records(creds$redcap_uri, creds$token, c(1, 5, 7))
}
}
