% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orca_2.0_functions.R
\name{calculate_rurality}
\alias{calculate_rurality}
\title{Calculate Rurality}
\usage{
calculate_rurality(zip_code, population_threshold = 50000)
}
\arguments{
\item{zip_code}{vector of zipcodes to act on (must be 5 digits)}

\item{population_threshold}{numeric value for population cut off for rural/non rural}
}
\value{
a vector of rural/non rural/na statuses
}
\description{
this function returns rural/non rural status for given zipcodes
}
