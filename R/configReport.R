#' Generates report details for each model defined by LHS and RHS
#' @param model Model defined by means of LHS and RHS(to be passed as a list)
#' @param multiple Default value of FALSE, single model to be examined.
#' @return List containing several report details. This is a named list
#' @export
#' @examples
#' model_1<- list(LHS="DR", RHS=c("avg_oil_pri_barrel_lag_3","avg_oil_pri_barrel_lag_2"))
#'reporter(model_1)

reporter <- function(model, multiple= FALSE) {

  allModels <- modelDeveloper(model$LHS,model$RHS,no_of_vars = length(model$RHS),multiple = multiple)

  #debugonce(modelDiagnostics)
  allModelsDiagnostics<-modelDiagnostics(allModels)
  #vars <- model$RHS

  allModelEvaluated<- modelEvaluator(allModelsDiagnostics)
  #debugonce(modelEvaluator)

  selectedModel <-
    allModelEvaluated$model # have chosen a model that passes all tests

  selectedModelObject <- allModels[[selectedModel]]
  report_summary <- summary(selectedModelObject)
  report_selectedModelDiagnostics <- selectedModelDiagnostics(selectedModel, allModelEvaluated)
  report_predicted_df <-
    selectedModelForecaster(selectedModel, allModelEvaluated)

  report_pred_plot <-
    selectedModelCharter(selectedModel, allModelEvaluated)
  report_details <- list(report_summary,report_selectedModelDiagnostics,report_predicted_df,report_pred_plot)

  names(report_details) = c("report_summary", "report_selectedModelDiagnostics", "report_predicted_df", "report_pred_plot")

  report_details

}
