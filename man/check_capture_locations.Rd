% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capture_check.R
\name{check_capture_locations}
\alias{check_capture_locations}
\title{Check that all capture locations in Capture_data appear in Location_data}
\usage{
check_capture_locations(
  Capture_data,
  Location_data,
  approved_list,
  output,
  skip
)
}
\arguments{
\item{Capture_data}{Data frame. Capture data output from pipeline.}

\item{Location_data}{Data frame. Location data output from pipeline.}

\item{approved_list}{List object. List of approved records from brood_approved_list.csv,
capture_approved_list.csv, individual_approved_list.csv, location_approved_list.csv.}

\item{output}{Character. Run checks on potential errors ("errors"), warnings ("warnings"), or both ("both"; default).}

\item{skip}{Character. Identifiers of the individual quality checks (CheckID) that should be skipped.}
}
\value{
A list of:
\item{CheckList}{A summary dataframe of check warnings and errors.}
\item{WarningRows}{A vector of rows with warnings.}
\item{ErrorRows}{A vector of rows with errors.}
\item{Warnings}{A list of row-by-row warnings.}
\item{Errors}{A list of row-by-row errors.}
}
\description{
Check that all capture locations recorded in Capture_data appear in Location_data. Missing locations will be flagged as a potential error.
}
\details{
Check ID: C6.
}
