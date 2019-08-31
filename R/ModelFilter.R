#' Filter models based on adjusted Rsquared before running diagnostic tests. This is done merely to reduce the time taken by modelDiagnostic function
#'
#' @param allModels allmodel objects as created by ModelDeveloper
#' @param adj.r.squaredThreshold Adjusted Rsquared threshold, models with less this threshold will be dropped from testing for other diagnostics
#' @return Returns a list of selected model objects. These are not just names of models.
#' @export
#' @examples
#' selectedModelObjs <- modelFilter(allModels,adj.r.squaredThreshold =0.5)

modelFilter <- function(allModels,adj.r.squaredThreshold =0.5){

  cat("Entered number of models :", length(names(allModels)))
  selectedModelObjs <-
    Filter(function(x)
      summary(x)$adj.r.squared > adj.r.squaredThreshold,
      allModels)
  cat("\n")
  cat("\n Filter criteria - Adjusted RSquared ",adj.r.squaredThreshold )
  cat("\n")
  cat("\n Selected number of models :", length(names(selectedModelObjs)))

  selectedModelObjs
  }
