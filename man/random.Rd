% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generator.R
\name{random}
\alias{random}
\title{Random number generator}
\usage{
random(count, rand_dist_name = c("Normal", "Poisson"), rand_dist_mean,
  rand_dist_sd)
}
\arguments{
\item{count}{number of values to generate}

\item{rand_dist_name}{random distribution name; Normal and Poisson are
supported}

\item{rand_dist_mean}{random distribution mean}

\item{rand_dist_sd}{random distribution standard deviation}
}
\description{
Generate number or date
}
