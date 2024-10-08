% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orca_2.0_functions.R
\name{get_stai5}
\alias{get_stai5}
\title{Process STAI 5 Data}
\usage{
get_stai5(token, timepoint = "orca_4month_arm_1", timestamp = T)
}
\arguments{
\item{token}{Unique REDCap token ID}

\item{timepoint}{redcap event name for the timepoint you wish to pull}

\item{timestamp}{whether to include survey timestamp. Default is true}
}
\value{
A data frame for the completed surveys
}
\description{
This function will download and return the total state and trait scores for the stai 5
}
