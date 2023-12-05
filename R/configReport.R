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
           unTransformFunction=NA,
           ...) {
    input_arg_list <- list(...)
    
    scenarios <- list()
    scenario_names <- list()
    scenario_colors <- list()
    scenario_levels <- list()
    sensitivity <- list()
    do_not_plot <- FALSE
    # initialise all plots as Empty
    report_pred_plot<-c()
    report_pred_plot_dynamic<-c()
    residualsVsFittedPlot <-c()
    normalQQPlot <-c()
    scale_locationPlot <-c()
    acfPlot <-c()
    selectedModelScenariosCharts <-c()
    scenarioMEVCharts <-c()
    asc_order_of_scenarios <- c()

    untransformCheck <- FALSE
    if ("do_not_plot" %in% names(input_arg_list)) {
      if (input_arg_list$do_not_plot) {
        do_not_plot <- TRUE
      } else {
        do_not_plot <- FALSE
      }
    }
    
    if ("scenarios" %in% names(input_arg_list)) {
      scenarios <- input_arg_list$scenarios
      if ("scenario_names" %in% names(input_arg_list)) {
        scenario_names <- input_arg_list$scenario_names
      } else{
        scenario_names <- paste0("scenario_", seq(length(scenarios)))
      }
      
    }
    
    if ("scenario_colors" %in% names(input_arg_list)) {
      scenario_colors <- input_arg_list$scenario_colors
    }
    
    if ("scenario_levels" %in% names(input_arg_list)) {
      scenario_levels <- input_arg_list$scenario_levels
    }
    
    if ("sensitivity" %in% names(input_arg_list)) {
      sensitivity <- input_arg_list$sensitivity
    }
    if ("untransform" %in% names(input_arg_list)) {
      untransformCheck <- input_arg_list$untransform
    }
    
    
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
    
    if (!do_not_plot) {
      report_pred_plot <-
        selectedModelCharter(selectedModel, selectedModelObject, allModelEvaluated)
      
    }

    
    # Untransform Logic ----------------
    transformedObj <- NA
    
    if (untransformCheck == TRUE) {
      if ("transformConfig" %in% names(input_arg_list)) {
        transformConfig <- input_arg_list$transformConfig
        stopifnot(is.data.frame(transformConfig))
        setDT(transformConfig)
      } else {
        stopifnot(
          exists("transformConfig", envir = .GlobalEnv) &
            is.data.frame(transformConfig)
        )
        transformConfig <- copy(transformConfig)
        setDT(transformConfig)
      }
      orderList <- transformConfigCheck(model_LHS, transformConfig)
      
      no_of_elements_to_be_removed_for_untransform <-
        orderList[type == 'difference', lag * differences] # to check if there is any differencing within the transofrm, and to use it only for differencing operations
      if (length(no_of_elements_to_be_removed_for_untransform) == 0) {
        # this is needed as if there is no dofferncing operation, it returns and empty array and not a 0
        no_of_elements_to_be_removed_for_untransform <- 0
      }
      baseVar <- unlist(orderList[, unique(baseVarName)])
      
      baseData <- as.vector(unlist(forecast_df[, ..baseVar]))
      transformedObj <-
        transformClass(
          baseData = baseData,
          no_of_elements_to_be_removed_for_untransform = no_of_elements_to_be_removed_for_untransform
        )
      
      transformedObj <- SettransformOrder(transformedObj, orderList)
      
      transformedObj <- transform(transformedObj)
      
      # check how many of the predicted variables need to be removed
      # This has to be done cos in the case of differencing, usually the first N values need to be removed
      #
      
      values_to_be_untransformed <-
        report_predicted_df$predicted_values[(no_of_elements_to_be_removed_for_untransform +
                                                1):length(report_predicted_df$predicted_values)]
      transformedObj <-
        untransform(transformedObj, values_to_be_untransformed)
      
      report_predicted_df$predicted_values_transformed <-
        transformedObj@inputData
      report_predicted_df[, (baseVar) := baseData]
      comment(report_predicted_df) <-
        baseVar ## USeful for plotting, this will
      #tell the plotting function what the untransformed response variable typically this is the observed Default rate
      
    }
    # start of scenario forecasts and charts--------------------------------
    selectedModelScenariosCharts <- list()
    scenario_list <- c()
    scenarioMEVCharts <- c()
    ModelSensitivities_df_list <- c()
    if (length(scenarios) > 0) {
      scenario_list = c()
      for (i in 1:length(scenarios)) {
        s <-
          new("scenarioClass",
              scenario_name = scenario_names[[i]],
              scenario_input = scenarios[[i]])
        scenario_result <-
          selectedModelForecaster(
            selectedModel,
            selectedModelObject,
            allModelEvaluated,
            scenario_input_df = s@scenario_input
          ) # this is a dataframe with cols such as  Date  DR_logit_FD, avg_oil_pri_barrel_lag_3 predicted_values
        if ((untransformCheck == TRUE) &&
            exists("transformConfig", envir = .GlobalEnv)) {
          if (exists("transformedObj")) {
            transformedObj <-
              untransform(transformedObj, scenario_result$predicted_values[(no_of_elements_to_be_removed_for_untransform +
                                                                              1):length(scenario_result$predicted_values)])
            scenario_result$predicted_values_transformed <-
              transformedObj@inputData
            scenario_result[, (baseVar) := baseData]
            comment(scenario_result) <- baseVar
          }
        }
        
        s@predictions = scenario_result
        scenario_list <- c(scenario_list, s)
      }
      s <- new("scenarioClass",
              scenario_name = 'Baseline',predictions=report_predicted_df) # add baseline to the list
      scenario_list <- append(scenario_list,s)
      ## get the order of scenarios
    
      means<-c()
      scenario_names<-c()
      for (model_scenario in scenario_list){
        scen_preds <- setDT(model_scenario@predictions)
        # if (!exists("means")){}
        # if (!exists("scenario_names")){}
        
        if (!exists("base_var")){
          base_var <- comment( model_scenario@predictions)
          # base_var <- comment( model_scenario$scenario_list[[1]]@predictions)
          
        }
        index_of_predictions <-max(which(!is.na(scen_preds[, ..base_var][[1]])))+1:nrow(scen_preds)
        # index_of_predictions <- (tsp(stats::na.contiguous(scen_preds[, ..base_var][[1]]))[2] +1):nrow(baseline_preds)
        # means=c(means,mean(scen_preds$predicted_values_transformed,na.rm=TRUE))
        means=c(means,mean(scen_preds$predicted_values_transformed[index_of_predictions],na.rm=TRUE))
        
        scenario_names<-c(scenario_names,model_scenario@scenario_name)
        names(means) <-scenario_names
      }
      asc_order_of_scenarios<-names(means[order(means)])
      if (!do_not_plot) {
        if (length(scenario_colors) > 0) {
          selectedModelScenariosCharts <-
            selectedModelScenariosCharter(scenario_list[-length(scenario_list)],
                                          copy(report_predicted_df),
                                          scenario_colors = scenario_colors)
          
          scenarioMEVCharts <-
            selectedModelScenariosMEVCharter(scenario_list[-length(scenario_list)],
                                             copy(report_predicted_df),
                                             model_RHS,
                                             scenario_colors = scenario_colors)
          
        } else {
          selectedModelScenariosCharts <-
            selectedModelScenariosCharter(scenario_list[-length(scenario_list)], copy(report_predicted_df))
          
          scenarioMEVCharts <-
            selectedModelScenariosMEVCharter(scenario_list[-length(scenario_list)], copy(report_predicted_df), model_RHS)
          
          
        }
      }
      
    }
    
    # end of scenario forecasts and charts--------------------------------
    
    #start of model sensitivity analyser------------------------------
    if (length(sensitivity) > 0) {
      ModelSensitivities_df_list <-
        ModelSensitiser(
          selectedModel = selectedModel,
          selectedModelObject = selectedModelObject,
          predicted_df = copy(report_predicted_df),
          forecast_df = forecast_df,
          sensitivity = sensitivity,
          rhs = model_RHS,
          transformedObj = transformedObj
        )
      
    }
    
    
    
    
    # end of model sensitivity analyser------------------------------
    
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
    
    if (!do_not_plot) {
    report_pred_plot_dynamic <-
      selectedModelCharter(selectedModel,
                           selectedModelObject,
                           allModelEvaluated,
                           predicted_df = report_dynamic_predicted_df)
    
    residualsVsFittedPlot <-
      function() {
        plot(selectedModelObject,
             which = 1,
             col = c("dark blue"))
      }
    normalQQPlot <-
      function() {
        plot(selectedModelObject,
             which = 2,
             col = c("dark blue"))
      }
    scale_locationPlot <-
      function() {
        plot(selectedModelObject,
             which = 3,
             col = c("dark blue"))
      }
    acfPlot <- function() {
      AutoCorrelation <- acf(selectedModelObject$residuals, plot = FALSE)
      plot(AutoCorrelation, main = "Residuals ACF Plot")
    }
    }
    
    
    if (nrow(dynamic) == 1) {
      report_predicted_df <- report_dynamic_predicted_df
      report_pred_plot <- report_pred_plot_dynamic
      
    }
    
    
    # Custom untransform logic ----------------
    if (!is.na(unTransformFunction)){
      if(!is.function(unTransformFunction)){
        stop(paste0("The customTransformFunction does not exist or is not a function.",unTransformFunction))  
      }
      transformedValues<- unTransformFunction(valuesToBeTransformed=report_predicted_df$predicted_values,...)
      print("transformed values")
      print(transformedValues)
      
    }
    
    # if (exists("transformConfig_Df", envir = .GlobalEnv)) {
    #   transformRule <-
    #     transformConfig_Df[depVar == model_LHS,]$transformRule
    #
    #   div100 = function(x) {
    #     return(x / 100)
    #   }
    #
    #   if (length(transformRule) > 0) {
    #     rules <- trimws(unlist(strsplit(transformRule, "[;]")))
    #     # untransform logic for divided by 100 which is used in Riyad bank. THis should move into a separate utility function of its own
    #
    #
    #     report_predicted_df$predicted_values_transformed <-
    #       report_predicted_df$predicted_values
    #
    #     for (i in 1:length(report_predicted_df$predicted_values)) {
    #       for (rule in rules) {
    #         report_predicted_df$predicted_values_transformed[i] <-
    #           do.call(rule,
    #                   list(
    #                     report_predicted_df$predicted_values_transformed[i]
    #                   ))
    #       }
    #
    #     }
    #   }
    # }
    
    
    
    # Full Report consolidation -------------
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
        scale_locationPlot,
        acfPlot,
        scenario_list,
        selectedModelScenariosCharts,
        scenarioMEVCharts,
        ModelSensitivities_df_list,
        asc_order_of_scenarios
        
      )
    
    names(report_details) <- c(
      "modelName",
      "report_summary",
      "report_selectedModelDiagnostics",
      "report_predicted_df",
      "report_pred_plot",
      "report_dynamic_predicted_df",
      "report_pred_plot_dynamic",
      "residualsVsFittedPlot",
      "normalQQPlot",
      "scale_locationPlot",
      "acfPlot",
      "scenario_list",
      "selectedModelScenariosCharts",
      "scenarioMEVCharts",
      "ModelSensitivities_df_list",
      "asc_order_of_scenarios"
    )
    
    report_details
    
  }
