% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tw061_fix_vartype.R
\name{tw061.vartypes}
\alias{tw061.vartypes}
\title{Assign tw061 variable types}
\usage{
tw061.vartypes(tw061table)
}
\arguments{
\item{tw061table}{is a list containing each FN table as an item in the list}
}
\value{
returns a list that contains each FN2 table as an item in the list
}
\description{
Changes the default 'chr' class of each variable to the desired class type (e.g. 'int') for GL1 data.  Generally this function will not be called directly as it is called from with 'import.fn.data'
}
