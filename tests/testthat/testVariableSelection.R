library(testthat)
test_that("Test Variable Selection on the basis of Pvalue ", {
  
  macrodata <- copy(ST.auto.1::macrodata)
  
  list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
  macrometa<-data.table(Variable=names(macrodata),Type=-1)
  
  list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
  validationSampler(macrodata, 1:29, 30:33, 1:48)
  
  
  responseVar <- c("DR")
  xVars <- names(macrodata)[!names(macrodata) %in% c(responseVar,"Date")]
  
  varsSelectedOnBasisOfRsq<-variableSelector(LHS_vars = responseVar,RHS_vars =xVars, trainData = train_df,R2Threshold = 0.9)
  expect_equal(length(varsSelectedOnBasisOfRsq),2)

  browser()
  varsSelectedOnBasisOfPvalue<-variableSelector(LHS_vars = responseVar,RHS_vars =xVars, trainData = train_df,R2Threshold = 0,pValueThreshold = 0.1)
  expect_equal(length(varsSelectedOnBasisOfPvalue),13)
  
  varsSelectedOnBasisOfPvalueAndRSquared<-variableSelector(LHS_vars = responseVar,RHS_vars =xVars, trainData = train_df,R2Threshold = 0.9,pValueThreshold = 0.1)
  expect_equal(length(varsSelectedOnBasisOfPvalueAndRSquared),13)
  # 
})
