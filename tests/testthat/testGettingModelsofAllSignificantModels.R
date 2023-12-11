

test_that("Model Selection on the basis of significance ", {
  
  macrodata <- copy(ST.auto.1::macrodata)
  
  list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
  macrometa<-data.table(Variable=names(macrodata),Type=-1)
  
  list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
  validationSampler(macrodata, 1:29, 30:33, 1:48)
  
  # first identify the baseVariables
  # After looking at the macrovariables one can see that these are the base variables, and all other variables are combinarions of these
  baseVars<-c("avg_oil_pri_barrel","Non_oil_ECI","ECI_yoy_ch")
  allFormulae<-ST.auto.1::getUnRelatedVariableCombinations("DR",baseVars,train_df,3 )
  
  expect_equal(length(allFormulae), 14)

  formualeFiltered<- getFormulaofAllSignificantModels(allFormulae,train_df )
  expect_equal(length(formualeFiltered),2)

})
# allFormulae
# [1] "DR ~ avg_oil_pri_barrel_3QMA + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"              
# [2] "DR ~ avg_oil_pri_barrel_3QMA_lag_2 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"        
# [3] "DR ~ avg_oil_pri_barrel + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"                   
# [4] "DR ~ avg_oil_pri_barrel_3QMA_lag_1 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"        
# [5] "DR ~ avg_oil_pri_barrel_6QMA_lag_1 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"        
# [6] "DR ~ avg_oil_pri_barrel_lag_2 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"             
# [7] "DR ~ avg_oil_pri_barrel_lag_3 + Non_oil_ECI_yoy_ch_3QMA_lag_3 + ECI_yoy_ch_3QMA_lag_4"  
# Youll find that all combinations are such that they dont contain any related variables
# })