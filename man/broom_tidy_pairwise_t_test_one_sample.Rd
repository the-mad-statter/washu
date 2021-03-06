% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/broom.R
\name{broom_tidy_pairwise_t_test_one_sample}
\alias{broom_tidy_pairwise_t_test_one_sample}
\title{Pairwise t tests}
\usage{
broom_tidy_pairwise_t_test_one_sample(data, response, group, mu = 0, ...)
}
\arguments{
\item{data}{tidy data containing the response and group variables}

\item{response}{response variable}

\item{group}{grouping variable}

\item{mu}{a number indicating the true value of the mean}

\item{...}{additional parameters passed onto \code{\link[stats:t.test]{t.test()}}}
}
\value{
a tibble with information about the model components; one model per row
}
\description{
Performs one-sample t-tests for each group from tidy data.
}
