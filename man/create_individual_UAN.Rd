% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_UAN.R
\name{create_individual_UAN}
\alias{create_individual_UAN}
\title{Create individual data table for data from University of Antwerp, Belgium.}
\usage{
create_individual_UAN(INDV_info, Capture_data, species_filter)
}
\arguments{
\item{INDV_info}{Data frame. Primary individual data from University of Antwerp.}

\item{Capture_data}{Output of \code{\link{create_capture_UAN}}.}

\item{species_filter}{6 letter species codes for filtering data.}
}
\value{
A data frame.
}
\description{
Create individual data table in standard format for data from University of
Antwerp, Belgium.
}
