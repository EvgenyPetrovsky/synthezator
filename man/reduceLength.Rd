% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generator.R
\name{reduceLength}
\alias{reduceLength}
\title{Reduce length of value according to specification}
\usage{
reduceLength(value, type = c("Number", "Date", "Varchar"), len = NULL,
  num_dec = NULL)
}
\arguments{
\item{value}{value to be processed}

\item{type}{data type of value}

\item{len}{desired length of value; must be specified for Number type}

\item{num_dec}{number of decimals; must be specified for Number type}
}
\description{
Reduce length of value according to specification
}
