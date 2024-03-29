% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_generic_fn_data1.R
\name{import.generic.fn.data1}
\alias{import.generic.fn.data1}
\title{import.generic.fn.data1}
\usage{
import.generic.fn.data1(generic_datazip)
}
\arguments{
\item{generic}{File path to FN DATA.ZIP folder}
}
\value{
a list of FN2 tables
}
\description{
A generic import function.  Project file path build is omitted so it 
must be provided as part of the argument.
}
\examples{
\dontrun{
fn <- import.generic.fn.data("~/FNData/NS1/IA02_NS1/DATA.ZIP")
fn$FN011
lapply(fn, head)
}
}
