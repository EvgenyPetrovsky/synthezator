% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generator.R
\name{validateSign}
\alias{validateSign}
\title{Validate and adjust sign of a number}
\usage{
validateSign(number, sign_type = c("Any", "Not Negative", "Not Positive",
  "Flip Negative", "Flip Positive"))
}
\arguments{
\item{number}{is a number to be validated and adjusted}

\item{sign_type}{sign type / limitation that should be applied}
}
\description{
Function adjust sign of a number based on a given limitation
  (sign_type). Sign type can be of different values: "Any" (no adjustment
  applied), "Not negative" / "Not Positive" (if number is negative / positive
  then it is replaced with 0), "Flip Negative" / "Flip Positive" (if number
  is negative / positive then it is multiplied by -1)
}
