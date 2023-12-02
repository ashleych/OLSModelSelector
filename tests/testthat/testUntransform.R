library(testthat)
library(data.table)
library(ST.auto.1)


test_that("Testing Untransform ", {
  
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

  # transformConfig  <-  data.table(
  #   stringsAsFactors = FALSE,
  #   varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit", 'DR_log'),
  #   baseVarName = c("DR", "DR", "DR", "DR"),
  #   order = c(1L, 2L, 1L, 1L),
  #   type = c("logit", "difference", "logit", "log"),
  #   lag = c(0L, 1L, 0L, 0L),
  #   differences = c(0L, 1L, 0L, 0L)
  # )
  untransform_config <- data.table(
    stringsAsFactors = FALSE,
    varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit", "DR_log","DR","def_rate_log"),
    baseVarName = c("DR", "DR", "DR", "DR","DR","def_rate"),
    order = c(1L, 2L, 1L, 1L,1L,1L),
    type = c("logit", "difference", "logit", "log","multiply","log"),
    lag = c(0L, 1L, 0L, 0L,0L,0L),
    differences = c(0L, 1L, 0L, 0L,0L,0L),
    multiplyFactor= c(0L, 0L, 0L, 0L,1L,0L)
  )

  rho<-0.02128422
  # call_excel(models,report_title = "new_testing")
  browser()
  reporter(
    models,
    # scenarios = list(upturn, downturn),
    # scenario_names = list('upturn', 'downturn'),
    unTransformFunction=untransformZscore,
    originalUntransformedValues=macrodata$PIT_PD,
    rho = rho
  )
  
})
