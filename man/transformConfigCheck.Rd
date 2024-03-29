% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformConfig.R
\name{transformConfigCheck}
\alias{transformConfigCheck}
\title{Checks if transformConfig data.table exists}
\usage{
transformConfigCheck(var, transformConfig)
}
\arguments{
\item{var}{name of the response variable that is to be untransformed
#' @param transformConfig dataframe that is either sent by the user or found in the global environment}

\item{orderList}{returns the orderList in the form that can be used to create a transformOrderClass}
}
\description{
Helper function to check if transformation rules are available in the environment
}
\examples{
transformConfigCheck("DR_logit_FD")
}
