% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_column_input.R
\name{filter_column_exactinput}
\alias{filter_column_exactinput}
\title{Filter Values in Column That Don't Break Exact Input Rule}
\usage{
filter_column_exactinput(check_table, validity_table, filter_column)
}
\arguments{
\item{check_table}{Data Frame. The table that will be checked against the specified rules in the validity table.}

\item{validity_table}{Data Frame. A table that stores the rules by which the check table is compared to.}

\item{filter_column}{Character. The column which will be checked. The column must be present in both the check table
as well as the validity table}
}
\description{
Filter Values in Column That Don't Break Exact Input Rule
}
