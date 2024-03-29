% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown.R
\name{estimate_items}
\alias{estimate_items}
\title{Estimate items}
\usage{
estimate_items(data, service, hours, rate, .protect = TRUE)
}
\arguments{
\item{data}{A data frame, data frame extention (e.g. a tibble), or a lazy
data frame (e.g., from dbplyr or dtplyr).}

\item{service}{description of line item}

\item{hours}{total hours for the line item}

\item{rate}{rate per hour for the line item}

\item{.protect}{flag whether to escape for TeX output}
}
\description{
Estimate items
}
