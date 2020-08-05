def_cols <-c("ODR_Overall", "ODR_MMAS", "ODR_Overall_1", "ODR_MMAS_1", "ODR_Overall_ln", "ODR_MMAS_ln", "ODR_Overall_ln_1", "ODR_MMAS_ln_1", "ODR_Overall_logit", "ODR_MMAS_logit", "ODR_Overall_logit_1")

vars<-colnames(macrodata)[!colnames(macrodata) %in% def_cols]
selectedVars <-
  variableSelector(
    LHS_vars = c("ODR_Overall"),
    RHS_vars = vars,
    trainData = train_df,
    R2Threshold = 0.5
  )

allModels <-
  modelDeveloper(
    LHS_vars = c("ODR_Overall"),
    RHS_vars = selectedVars,
    trainData = train_df,
    no_of_vars = 2
  )
selectedModelObjs <- modelFilter(allModels,adj.r.squaredThreshold =0.75)
allModelsDiagnostics <- modelDiagnostics(selectedModelObjs)
allModelEvaluated<-modelEvaluator(allModelsDiagnostics)
table(allModelEvaluated$FinalResults)


ODR_1

model<- "ODR_MMAS_ln ~ ODR_Overall_ln_1 + cp_.Crude_Oil__petroleum__lag1"

