% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_PFN.R
\name{make_IndIdentifier}
\alias{make_IndIdentifier}
\title{Make a summary table containing all ring numbers of re-ringed individual}
\usage{
make_IndIdentifier(raw_data)
}
\arguments{
\item{raw_data}{Data frame. Contains pairs of rings (one pair per
re-ringing event). Column 'RING' = first ring, column 'RING2' = second ring.}
}
\value{
A data frame with columns 'RingNr' (= original ring number) and
'Identifier' (= new unique integer identifier for the individual).
}
\description{
Assigns new unique identifiers to individuals that appear with two or more
different ring numbers in a PFN data set. The resulting table can then be used
to replace all alternative ring numbers of an individual with the new
identifier in all tables contained in the standard format.
}
\examples{
#Create fake dataset
dat <- data.frame(RING = c('A1', 'B2', 'C7', 'D7'), RING2 = c('B9', 'C7', 'E3', NA))
#Summarise re-ringing information by individual
make_IndIdentifier(raw_data = dat)
}
