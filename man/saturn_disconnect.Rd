% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saturn.R
\name{saturn_disconnect}
\alias{saturn_disconnect}
\title{Saturn Disconnect}
\usage{
saturn_disconnect(session)
}
\arguments{
\item{session}{ssh connection created with \code{\link{saturn_connect}}}
}
\description{
Saturn Disconnect
}
\examples{
\dontrun{
session <- saturn_connect()
saturn_disconnect(session)
}

}
\seealso{
\code{\link[ssh]{ssh}}
}
