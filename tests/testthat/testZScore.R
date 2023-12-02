library(testthat)
library(data.table)
library(ST.auto.1)
test_that("Testing Zscore ", {
  macrodata<-readxl::read_excel("~/automod/OLSModelSelector/tests/testthat/candidate1.xlsx.xlsx",sheet = "macrodata")
  # macrodata<-readxl::read_excel("~/automod/OLSModelSelector/tests/testthat/BSF_RegressionData_for_Modeling.xlsx",sheet = "FYZ_Restructuring_DPD_M")
  
macrodata<-fread("~/automod/OLSModelSelector/tests/testthat/bsfMacroData.csv")
  setDT(macrodata)
  list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
  # create macrometa 
  # 
  macrometa<-data.table(Variable=names(macrodata),Type=-1)
  
  list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
  
  validationSampler(macroecodata = macrodata, 21:62, 63:66, 21:135)
  
  models <- c("Z_index ~ cp_General_government_debt_to_GDP_ratio_percenatge_NSA_qoq_lag3+Z_index_lag1")
  scenario_colors <- c()
  scenarios <- c()
  scenario_names <- c()
  sensitivity <- c()
  rm(configDynamic_Df,envir = .GlobalEnv)
  # excelDetails <- reporter(models,untransform=TRUE,scenarios=scenarios,scenario_names=scenario_names,scenario_colors=scenario_colors,sensitivity=sensitivity,report_type = 'unformatted')
  if (exists("configDynamic_Df", envir = .GlobalEnv)){
    rm(configDynamic_Df)
  }
  if ("configDynamic_Df" %in% ls()){
    rm(configDynamic_Df)
    
  }
  excelDetails <- reporter(models, report_type = "unformatted")
  		
  # 0.660	0.643	0.687
  
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$R2), 0.6604, tolerance = 0.0001)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$adjR2), 0.6430, tolerance = 0.0001)
  expect_equal(as.numeric(excelDetails$report_summary$otherStats_excel$MAPE), 0.6868, tolerance = 0.0001)
  
  expect_equal(excelDetails$report_predicted_df[Date=="01/06/2022",predicted_values],-0.185377,tolerance = 0.0001)
  
  expect_equal(excelDetails$report_predicted_df[Date=="01/09/2022",predicted_values],-1.60078,tolerance = 0.0001)
  


  expect_equal(max(which(!is.na(excelDetails$report_predicted_df$predicted_values))),47)
  configDynamic("Z_index","Z_index_lag1",1,46)

  excelDetails_with_configDynamic <- reporter(models, report_type = "unformatted")
  expect_equal(max(which(!is.na(excelDetails_with_configDynamic$report_predicted_df$predicted_values))),115)
 forecast<- excelDetails_with_configDynamic$report_predicted_df
 expect_equal( forecast$predicted_values[nrow(forecast)],0.23412,tolerance=0.0001) #this checks for the dynamically forecasted value, this value is present in 
 expect_equal(forecast[Date=="01/09/2022",predicted_values],-1.60078,tolerance = 0.0001)
 expect_equal(forecast[Date=="01/12/2022",predicted_values],-0.95327,tolerance = 0.0001)
 
 
 
 # /home/ashleyubuntu/automod/OLSModelSelector/tests/testthat , sheet Z_index ~ cp_General_governm.1, cell I135 where the values are formula driven
 
 
})