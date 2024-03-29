% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectedModelRegressionResults.R
\name{selectedModelRegressionResults}
\alias{selectedModelRegressionResults}
\title{Show pretty table with results of the regression for the selected Model}
\usage{
selectedModelRegressionResults(
  selectedModel,
  allModelEvaluated,
  direction_config = macrometa,
  pvalue_threshold = 0.05,
  report_type = "html"
)
}
\arguments{
\item{selectedModel}{model name as a string as generated by the function modelDeveloper. String can be stored in a vector and used as well}

\item{allModelEvaluated}{the results as generated by modelEvaluator function}
}
\value{
Returns a table, using formattable
}
\description{
Show pretty table with results of the regression for the selected Model
}
\examples{
selectedModelDiagnostics("DR ~ ECI_yoy_ch_3QMA_lag_4+avg_oil_pri_barrel_3QMA_lag_1",allModelEvaluated) # allModelEvaluated created by modelEvaluator()
selectedModel <- "DR ~ ECI_yoy_ch_3QMA_lag_4+avg_oil_pri_barrel_3QMA_lag_1"
selectedModelDiagnostics(selectedModel,allModelEvaluated) # allModelEvaluated created by modelEvaluator()
}
