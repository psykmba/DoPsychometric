% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReliabilityTest.R
\name{GetReliabilityTest}
\alias{GetReliabilityTest}
\title{Getting the Reliability object based on a Psychometric object}
\usage{
GetReliabilityTest(object, what = "Alpha", check = T, imp = "NULL", ...)
}
\arguments{
\item{object}{A Psychometric object}

\item{what}{what type of analyses: Alpha, Omega or Parallel}

\item{check}{whether to check keys in alpha}

\item{imp}{how to impute variables in alpha:}

\item{...}{more arguments}
}
\value{
A Reliability object based on a Psychometric object that can be used for analyses
}
\description{
Makes it simple to do basic psychometrics
}
\details{
This function creates a new object that is an extension of the
Psychometric object. The functions implement reliability analyses using either
'alpha' or 'omega'. The 'Reliabilty' can be used to get detailed information
about items. In addition, using the function "GetCommand" you can get all the
necessary commands to do more extensive reliability analyses.
Another function is to estimate a parallel analyses using the psych::fa.parallel,
and to both commands and plots for this using the plot command.
}
