% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DTedit.R
\name{dteditmodUI}
\alias{dteditmodUI}
\title{Create a DataTable with Add, Edit and Delete buttons.}
\usage{
dteditmodUI(id)
}
\arguments{
\item{id}{the namespace of the module}
}
\description{
dteditmodUI - user-interface function for module use
}
\details{
Use in conjunction with \code{callModule} and \code{dtedit} to create
editable datatables. \code{dteditUI} is used in the 'user interface'
component of the shiny app.
}
\seealso{
\code{\link{dteditmod}} : the companion server function.\cr

\itemize{
\item \code{example("dteditmodUI")} a simple example
with a reactive dataframe
\item \code{dteditmod_demo()} a more complex example.
Includes database interaction and interactions between
the data of multiple datatables.
}

Other Datatable Edit functions: 
\code{\link{dtedit}()}
}
\concept{Datatable Edit functions}
