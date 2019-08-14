# mdata<-readxl::read_excel("data/MacroDataAC.xlsx")
validationSampler(macrodata,1:29,30:33,1:48)



vars<-c("ECI_yoy_ch_3QMA_lag_4", "avg_oil_pri_barrel_3QMA", "Rl_est_Dub_q_yoy_ch_lag_1", "avg_oil_pri_barrel_3QMA_lag_2", "Non_oil_ECI_yoy_ch_3QMA_lag_3", "Non_oil_ECI_yoy_ch_6QMA", "avg_oil_pri_barrel", "avg_oil_pri_barrel_3QMA_lag_1", "avg_oil_pri_barrel_6QMA_lag_1", "avg_oil_pri_barrel_lag_2", "avg_oil_pri_barrel_lag_3")
allModels<-modelDeveloper("DR",vars, trainData = train_df,no_of_vars = 3)
allModelsDiagnostics<-modelDiagnostics(allModels)
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)
allModelEvaluated[FinalResults=="PASS",]



validationSampler(macrodata,1:29,30:33,1:48)
model_1<- "DR ~ ECI_yoy_ch_3QMA_lag_4+avg_oil_pri_barrel_3QMA+avg_oil_pri_barrel_3QMA_lag_2"

reporter(model_1)

###############Individial Analysis

model_LHS <- trimws(unlist(strsplit(model_1, "[~]"))[[1]])
RHS_combined <- trimws(unlist(strsplit(model_1, "[~]"))[[2]])
model_RHS <- trimws(unlist(strsplit(RHS_combined, "[+]")))
allModels <- modelDeveloper(model_LHS, model_RHS,multiple=FALSE, no_of_vars = length(model_RHS))
allModelsDiagnostics <- modelDiagnostics(allModels)
allModelEvaluated <- modelEvaluator(allModelsDiagnostics)
selectedModel <- allModelEvaluated$model
selectedModelObject <- allModels[[selectedModel]]
report_summary <- selectedModelRegressionResults(selectedModel,
                                                 allModelEvaluated, direction_config = macrometa, pvalue_threshold = 0.05)
report_selectedModelDiagnostics <- selectedModelDiagnostics(selectedModel,
                                                            allModelEvaluated)
report_predicted_df <- selectedModelForecaster(selectedModel,
                                               selectedModelObject, allModelEvaluated)
report_pred_plot <- selectedModelCharter(selectedModel, selectedModelObject,
                                         allModelEvaluated)
report_details <- list(report_summary, report_selectedModelDiagnostics,
                       report_predicted_df, report_pred_plot)
names(report_details) = c("report_summary", "report_selectedModelDiagnostics",
                          "report_predicted_df", "report_pred_plot")
report_details


