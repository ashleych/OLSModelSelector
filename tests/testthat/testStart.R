test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("loading works",{
  macrodata<-copy(macrodata)
  macrodata[,DR_logit := logit(DR)]
  macrodata[,DR_logit_FD := DR_logit - shift(DR_logit, n = 1, type = 'lag')]
  validationSampler(macrodata,1:29,30:33,1:48)
  transformConfig  <-  data.table(stringsAsFactors=FALSE,
                                  varName = c("DR_logit_FD", "DR_logit_FD", "DR_logit"),
                                  baseVarName = c("DR", "DR", "DR"),
                                  order = c(1L, 2L, 1L),
                                  type = c("logit", "difference", "logit"),
                                  lag = c(0L, 1L, 0L),
                                  differences = c(0L, 1L, 0L))
  
  models <- c(" DR_logit_FD ~ avg_oil_pri_barrel_lag_3")
  # call_excel(models,report_title = "new_testing")
  call_excel(models,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn'),report_title = "new") # 
  
  expect_equal(models,models)
})


test_that("scenario_check",{
  
  upturn=copy(macrodata)
  
  downturn=copy(macrodata)
  
  sen=1.05 #mult factor  to be applied to generate upturn and downturn
  
  mevs<-colnames(macrodata)[!colnames(macrodata) %in% "Date"] #identofy all the MEV columns
  
  upturn<-upturn[,c(mevs):=.SD*sen,.SDcols = mevs] # One needs to read scenario data from csv or excel. for now creating dummy data
  
  downturn<-downturn[,c(mevs):=.SD*0.95,.SDcols = mevs] # One needs to read scenario data from csv or excel, for now creating dummy data
  
  model=c(  "DR ~ad_hot_occ +avg_oil_pri_barr ","DR ~ad_hot_occ ")
  call_excel(models,scenarios=list(upturn,downturn),scenario_names=list('upturn','downturn')) # 
})