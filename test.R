# mdata<-readxl::read_excel("data/MacroDataAC.xlsx")
validationSampler(macrodata,1:29,30:33,1:48)



vars<-c("ECI_yoy_ch_3QMA_lag_4", "avg_oil_pri_barrel_3QMA", "Rl_est_Dub_q_yoy_ch_lag_1", "avg_oil_pri_barrel_3QMA_lag_2", "Non_oil_ECI_yoy_ch_3QMA_lag_3", "Non_oil_ECI_yoy_ch_6QMA", "avg_oil_pri_barrel", "avg_oil_pri_barrel_3QMA_lag_1", "avg_oil_pri_barrel_6QMA_lag_1", "avg_oil_pri_barrel_lag_2", "avg_oil_pri_barrel_lag_3")
allModels<-modelDeveloper("DR",vars, trainData = train_df,no_of_vars = 2)
allModelsDiagnostics<-modelDiagnostics(allModels)
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)
allModelEvaluated[FinalResults=="PASS",]



validationSampler(macrodata,1:29,30:33,1:48)
model_1<- list(LHS="DR", RHS=c("avg_oil_pri_barrel_lag_3","avg_oil_pri_barrel_lag_2"))
modelDeveloper(model_1$LHS,model_1$RHS,no_of_vars = 2,multiple = FALSE) -> allModels

debugonce(modelDiagnostics)
allModelsDiagnostics<-modelDiagnostics(allModels)
vars = model_1$RHS

allModelEvaluated<- modelEvaluator(allModelsDiagnostics)
debugonce(modelEvaluator)

selectedModel <-
  allModelEvaluated$model # have chosen a model that passes all tests

selectedModelObject <- allModels[[selectedModel]]
selectedModelDiagnostics(selectedModel, allModelEvaluated)
predicted_df<-selectedModelForecaster(selectedModel,allModelEvaluated)
predicted_df


selectedModelCharter(selectedModel,allModelEvaluated)

selectedMOdelDf<-allModelsDiagnostics[model==selectedModel]
debugonce(selectedModelDiagnostics)
selectedModelDiagnostics(selectedModel, allModelEvaluated)

selectedModelCharter(selectedModel,allModelEvaluated)
