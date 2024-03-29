% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown.R
\name{wu_new_letter_of_support}
\alias{wu_new_letter_of_support}
\title{New letter of support}
\usage{
wu_new_letter_of_support(
  from_name,
  from_title,
  from_department_type,
  from_department_name,
  from_department_url,
  from_campus_box,
  from_phone,
  from_email,
  to_name,
  to_address,
  date,
  salutation,
  closing,
  body,
  signature = "",
  output = "LOS.Rmd"
)
}
\arguments{
\item{from_name}{sender name}

\item{from_title}{sender title}

\item{from_department_type}{sender department type}

\item{from_department_name}{sender department name}

\item{from_department_url}{sender department url}

\item{from_campus_box}{sender campus box}

\item{from_phone}{sender phone}

\item{from_email}{sender email}

\item{to_name}{recipient name}

\item{to_address}{recipient address}

\item{date}{letter date}

\item{salutation}{letter greeting}

\item{closing}{letter closing}

\item{body}{body of letter}

\item{signature}{path to signature file (empty string for no signature)}

\item{output}{RMarkdown output}
}
\description{
New letter of support
}
\examples{
\dontrun{
## some yaml may require quotes, e.g.:
## - salutation because of the colon
## - signature if spaces in path
wu_render_letter_of_support(
  "Matthew J. Schuelke, PhD",
  "Research Statistician",
  "Division",
  "Biostatistics",
  "https://biostatistics.wustl.edu",
  "8067",
  "+1 314 362-0000",
  "schuelke@wustl.edu",
  "Christopher Robin",
  c("11 Mallord Street", "LONDON", "SW3 6DT", "UK"),
  Sys.Date(),
  "\"To Whom It May Concern:\"",
  "Sincerely,",
  letter_of_support_body("The Woozle Effect"),
  paste0("\"", Sys.getenv("WU_SIGNATURE"), "\"")
)
}
}
