% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_redcap_data.R
\name{import_redcap_data}
\alias{import_redcap_data}
\title{Import REDCap data into R}
\usage{
import_redcap_data(f, keep_checkbox_fields = FALSE, empty_as_na = FALSE)
}
\arguments{
\item{f}{A R script filename, or the R/CSV pairs as determined by \code{\link{return_filename_pair}}}

\item{keep_checkbox_fields}{(FALSE) A logical indicating if the checkbox fields should be
kept in the resulting data frame.}

\item{empty_as_na}{(FALSE) A logical indicating if the empty string "" should be
replaced by NA values.}
}
\value{
A \code{\link[tibble]{tibble}} of REDCap imported data.
}
\description{
REDCap exported data (exported to R) is imported with a few tweaks to the data
for usability.
}
\details{
REDCap allows a user to export all of the data in the project to R format. This results in
two files (an R file and a CSV file). Typically, one would simply source the R file and the
REDCap data would be available in the R session. However, there are a few small caveats with
this data, prompting the creation of this function.

Specifically, there are two issues:
\itemize{
\item Clearing the environment
\item No labels on factor variables
}

This function fixes those two issues and returns a \code{\link[tibble]{tibble}} for
further processing.
}
\note{
The input parameter to this function can be either the R script name, or the R/CSV pairs
as determined by \code{\link{return_filename_pair}}.
}
\examples{
\dontrun{
import_redcap_data("export_from_redcap.R")
}
}
