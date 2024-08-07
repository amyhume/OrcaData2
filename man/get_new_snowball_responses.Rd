% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orca_2.0_functions.R
\name{get_new_snowball_responses}
\alias{get_new_snowball_responses}
\title{Pulling New Snowball Responses}
\usage{
get_new_snowball_responses(token, min_date_time = "2022-01-01 00:00:00")
}
\arguments{
\item{token}{Unique REDCap API token}

\item{min_date_time}{The minimum timestamp to pull responses after}
}
\value{
A list with 'snowball_responses': a df with new responses and the id that referred them, and 'to_compensate': a df with list of ids who have referred people, the number of people referred by them
}
\description{
Imports new screener responses which have been referred by another person. Will match up the email with the existing participant and put them into another df to compensate. If no id can be found for the email, user will be required to input one
}
