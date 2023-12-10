test_that("Model Selection on the basis of significance ", {
  
  macrodata<-fread("~/automod/OLSModelSelector/tests/testthat/bsfMacroData.csv")
  setDT(macrodata)
  list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
  # create macrometa 
  # 
  macrometa<-data.table(Variable=names(macrodata),Type=-1)
  
  list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
  
  validationSampler(macroecodata = macrodata, 21:62, 63:66, 21:135)
  
  models <- c("Z_index ~ cp_General_government_debt_to_GDP_ratio_percenatge_NSA_qoq_lag3+Z_index_lag1")
  model<- modelDeveloper(modelsNamesList = models)

  expect_equal(summary(model)[,"Class"], "lm")
}
)