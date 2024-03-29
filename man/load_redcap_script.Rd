% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_redcap_data.R
\name{load_redcap_script}
\alias{load_redcap_script}
\title{Load REDCap data loading script}
\usage{
load_redcap_script(f)
}
\arguments{
\item{f}{The R script file to load}
}
\value{
A function that, when executed with no arguments, will load the REDCap data.
}
\description{
This is a function that will read in an R script which was generated
to load REDCap data into R. It will return a function that, when called, will
perform this work.
}
\details{
REDCap generates an R script that is used to load REDCap data (and labels, etc) into
R. The R script will load the corresponding csv file (that contains the data) and
set the various metadata based on REDCap information. This script is typically sourced
into R directly.

This function is a wrapper to load this R script in and make the data loading available
as a function call. That is,

\if{html}{\out{<div class="sourceCode">}}\preformatted{load_redcap_script(filename)()
}\if{html}{\out{</div>}}

will give the actual REDCap data, formatted as intended.

Why do this? There are several issues with the default REDCap R script. First off, they remove
everything in the environment. This is helpful to avoid name collisions, etc but not as helpful
as part of a larger system or library. Secondly, the files (csv and R) have to be in the working
directory (in the default script) which is a bit limiting. Therefore, this function takes care
of both of these issues which makes loading easier.
}
\note{
This function generally shouldn't be run (and is not exported) since there is a higher-level
function, \code{\link{import_redcap_data}} that manages a few other aspects of the data loading. This
function only manages loading the R script itself.
}
