% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orca_2.0_functions.R
\name{input_reason_for_ignores}
\alias{input_reason_for_ignores}
\title{Input Reasons for Screener Ignores}
\usage{
input_reason_for_ignores(data)
}
\arguments{
\item{data}{the data frame to act on}
}
\value{
A data frame with the reason columns added, only if there were 0s present in the first place
}
\description{
asks for input for each import that has a 0 marked for contact (1-4 codes) and a description for any 'other' reasons
}
