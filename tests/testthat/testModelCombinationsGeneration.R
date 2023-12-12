test_that("Get all combination of formulae that dont have related variables ", {
  
macrodata <- copy(ST.auto.1::macrodata)

list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
macrometa<-data.table(Variable=names(macrodata),Type=-1)

list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
validationSampler(macrodata, 1:29, 30:33, 1:48)

# first identify the baseVariables
# After looking at the macrovariables one can see that these are the base variables, and all other variables are combinarions of these
baseVars<-c("avg_oil_pri_barrel","Non_oil_ECI","ECI_yoy_ch")
allFormulae<-getUnRelatedVariableCombinations("DR",baseVars,train_df,3,TRUE,'startsWith')

expect_equal(length(allFormulae), 14)


allFormulaeWithCheckAnyPattern<-getUnRelatedVariableCombinations("DR",baseVars,train_df,3,TRUE,'checkany')
expect_equal(length(allFormulaeWithCheckAnyPattern), 42)

# test for non exact match of max number of variables
allFormulae<-getUnRelatedVariableCombinations("DR",baseVars,train_df,3,FALSE )

expect_equal(length(allFormulae), 47)
formulaeDT<-data.table(formulae=allFormulae)
splitTemp <- formulaeDT[,  tstrsplit(formulae, "[+]", fill = NA)]
formulaeDT<-cbind(formulaeDT,splitTemp)
expect_equal(formulaeDT[,uniqueN(formulae)] , formulaeDT[,.N])

numberOfVariables<-3
indexStart<-1
duplicates<-formulaeDT[duplicated(formulaeDT, by=c("formulae"))]

expect_equal(nrow(duplicates),0)

baseVars<-c("avg_oil_pri_barrel_3QMA_","Non_oil_ECI","ECI_yoy_ch","avg_oil_pri_barrel_lag")

})


test_that("Get all combination of formulae that dont have related variables when 4 variables are involved", {
  
  macrodata <- copy(ST.auto.1::macrodata)
  
  list2env(list(macrodata = macrodata), envir = .GlobalEnv) ## put this in the global environment
  macrometa<-data.table(Variable=names(macrodata),Type=-1)
  
  list2env(list(macrometa = macrometa), envir = .GlobalEnv) ## put this in the global environment
  validationSampler(macrodata, 1:29, 30:33, 1:48)
  
  # first identify the baseVariables
  # After looking at the macrovariables one can see that these are the base variables, and all other variables are combinarions of these
  baseVars<-c("avg_oil_pri_barrel_3QMA_","Non_oil_ECI","ECI_yoy_ch","avg_oil_pri_barrel_lag")
  no_of_vars<-4
  allFormulae<-getUnRelatedVariableCombinations("DR",baseVars,train_df,no_of_vars,strictMax =  TRUE,patternType = 'startsWith')

  checkVariableCountOfFormulae <- function(allFormulae) {
    formulaeDT<-data.table(formulae=allFormulae)
    splitTemp <- formulaeDT[,  tstrsplit(formulae, "[+]", fill = NA)]
    formulaeDT<-cbind(formulaeDT,splitTemp)
    meltedDT <-
      data.table::melt(formulaeDT,
                       id = "formulae",
                       variable.factor = FALSE)
    meltedDT<-meltedDT[!is.na(value),]
    meltedDT$variable <- NULL
   varCountDT<- meltedDT[,.N,by=formulae]
   varCountDT$Count<-varCountDT$N
   
    return(varCountDT[,.N,by=Count][order(N)])
  }

  countOfFormulaWithNVariables<-checkVariableCountOfFormulae(allFormulae = allFormulae)
  expect_equal(countOfFormulaWithNVariables[Count==4,N],8)

  
  # test for non exact match of max number of variables
  allFormulaeWithNotStrictMax<-getUnRelatedVariableCombinations("DR",baseVars,train_df,no_of_vars,FALSE )
  countOfFormulaWithNVariables_notStrict<-checkVariableCountOfFormulae(allFormulae = allFormulaeWithNotStrictMax)
  expect_equal(countOfFormulaWithNVariables_notStrict[Count==3,N],20)
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