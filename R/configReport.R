#' Generates report details for each model defined by LHS and RHS
#' @param model Model defined by means of LHS and RHS(to be passed as a list)
#' @param multiple Default value of FALSE, single model to be examined.
#' @return List containing several report details. This is a named list
#' @export
#' @examples
#' model_1<- list(LHS="DR", RHS=c("avg_oil_pri_barrel_lag_3","avg_oil_pri_barrel_lag_2"))
#'reporter(model_1)

reporter <-
  function(model,
           report_type = 'html',
           multiple = FALSE,
           ...) {
    model_LHS <- trimws(unlist(strsplit(model, "[~]"))[[1]])
    RHS_combined <- trimws(unlist(strsplit(model, "[~]"))[[2]])
    model_RHS <- trimws(unlist(strsplit(RHS_combined, '[+]')))
    allModels <-
      modelDeveloper(model_LHS,
                     model_RHS,
                     no_of_vars = length(model_RHS),
                     multiple = multiple)

    #debugonce(modelDiagnostics)
    allModelsDiagnostics <- modelDiagnostics(allModels)
    #vars <- model$RHS

    allModelEvaluated <- modelEvaluator(allModelsDiagnostics)
    #debugonce(modelEvaluator)

    selectedModel <-
      allModelEvaluated$model

    selectedModelObject <- allModels[[selectedModel]]
    report_summary <-
      selectedModelRegressionResults(
        selectedModel,
        allModelEvaluated,
        direction_config = macrometa,
        pvalue_threshold = 0.05,
        report_type = report_type
      )

    report_selectedModelDiagnostics <-
      selectedModelDiagnostics(selectedModel, allModelEvaluated, report_type = report_type)

    report_predicted_df <-
      selectedModelForecaster(selectedModel, selectedModelObject, allModelEvaluated)

    report_pred_plot <-
      selectedModelCharter(selectedModel, selectedModelObject, allModelEvaluated)

    dynamic <- dynamicCheck(selectedModel)

    if (nrow(dynamic) == 1) {
      report_dynamic_predicted_df <-
        dynamicForecast(
          selectedModel,
          selectedModelObject,
          laggedRelation = paste0(dynamic$depVar, ":", dynamic$lagVar, ":", dynamic$noofLags),
          trainIndex = dynamic$trainIndex
        )
    } else {
      report_dynamic_predicted_df <- data.table()
    }

    report_pred_plot_dynamic <-
      selectedModelCharter(selectedModel,
                           selectedModelObject,
                           allModelEvaluated,
                           predicted_df = report_dynamic_predicted_df)
    
    residualsVsFittedPlot <-function() {plot(selectedModelObject,which=1,col=c("dark blue"))}
    normalQQPlot<- function() { plot(selectedModelObject,which=2,col=c("dark blue"))}
    scale_locationPlot<-function() { plot(selectedModelObject,which=3,col=c("dark blue"))}


    if (nrow(dynamic) == 1) {
      report_predicted_df <- report_dynamic_predicted_df
      report_pred_plot <- report_pred_plot_dynamic

    }

    if (exists("transformConfig_Df", envir = .GlobalEnv)) {
      transformRule <-
        transformConfig_Df[depVar == model_LHS,]$transformRule

      div100 = function(x) {
        return(x / 100)
      }

      if (length(transformRule) > 0) {
      rules <- trimws(unlist(strsplit(transformRule, "[;]")))
      # untransform logic for divided by 100 which is used in Riyad bank. THis should move into a separate utility function of its own


      report_predicted_df$predicted_values_transformed <- report_predicted_df$predicted_values

        for (i in 1:length(report_predicted_df$predicted_values)) {
          for (rule in rules) {
            report_predicted_df$predicted_values_transformed[i] <-
              do.call(rule, list(report_predicted_df$predicted_values_transformed[i]))
          }

        }
      }
    }

    report_details <-
      list(
        model,
        report_summary,
        report_selectedModelDiagnostics,
        report_predicted_df,
        report_pred_plot,
        report_dynamic_predicted_df,
        report_pred_plot_dynamic,
        residualsVsFittedPlot,
        normalQQPlot,
        scale_locationPlot
        
      )

    names(report_details) = c(
      "modelName",
      "report_summary",
      "report_selectedModelDiagnostics",
      "report_predicted_df",
      "report_pred_plot",
      "report_dynamic_predicted_df",
      "report_pred_plot_dynamic",
      "residualsVsFittedPlot",
      "normalQQPlot",
      "scale_locationPlot"
    )

    report_details

  }
