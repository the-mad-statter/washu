% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{append_timestamp}
\alias{append_timestamp}
\title{Append a Timestamp to the End of a File Name}
\usage{
append_timestamp(x, format = "\%y\%m\%d\%H\%M\%S", delim = "_")
}
\arguments{
\item{x}{file name to which a time stamp string should be appended.}

\item{format}{character string representing how the timestamp should appear}

\item{delim}{delimiter between original file name of x and the timestamp}
}
\value{
file name with time stamp string appended
}
\description{
Append a Timestamp to the End of a File Name
}
\examples{
append_timestamp("my_file.txt")
}
