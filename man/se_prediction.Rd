% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem.R
\name{se_prediction}
\alias{se_prediction}
\title{Standard Error of Prediction}
\usage{
se_prediction(sd.1, sd.2, r)
}
\arguments{
\item{sd.1}{Standard deviation of the first variable.}

\item{sd.2}{Standard deviation of the second variable.}

\item{r}{Reliability coefficient (reliability of the test).}
}
\value{
The standard error of prediction.
}
\description{
This function calculates the standard error of prediction based on the
provided standard deviations and the reliability coefficient.
}
\examples{
# Single values
se_prediction(3, 4, 0.8)

# Vectors
se_prediction(c(3, 2, 4), c(4, 3, 5), 0.8)

}
