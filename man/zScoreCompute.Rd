% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customTransform.R
\name{zScoreCompute}
\alias{zScoreCompute}
\title{Custom transform function}
\usage{
zScoreCompute(pds, rho, ...)
}
\arguments{
\item{pds}{orignal pd values that are to be transformed}

\item{rho}{Correlation value that is to be passed}

\item{zScoreCompute}{Function that is to be called to transform to zscores from PDs}
}
\value{
List of transformed vales, the avg zindex, mean pd and rho
}
\description{
Custom transform function
}
\examples{
model_1<- zScoreCompute(c(0.1,0.2),0.023)
}
