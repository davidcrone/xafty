% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as.numeric_xafty}
\alias{as.numeric_xafty}
\title{Check if Passed Values can be Parsed as Numeric}
\usage{
as.numeric_xafty(numbers, force_type = TRUE)
}
\arguments{
\item{numbers}{Character vector of Numbers to be converted}

\item{force_type}{Boolean. Whether to force the type conversion even if it introduces NAs during type conversion. If TRUE
the function keeps the column as is.}
}
\value{
An equally length numeric vector if the numerics could be successfully converted to NA.
}
\description{
Check if Passed Values can be Parsed as Numeric
}
