% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BestFunctions.R
\name{BestItemsForScale}
\alias{BestItemsForScale}
\title{BestItemsForScale}
\usage{
BestItemsForScale(object, targetObject = NULL, method = "mean")
}
\arguments{
\item{object}{Psychometric object}

\item{targetObject}{Psychometric object which should be used as facet targets}

\item{method}{which method to create the scales, mean default}
}
\value{
Psychometric object with items that correlate most strongly with their scale
}
\description{
BestItemsForScale
}
