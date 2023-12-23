test_that("Get all combination of formulae when large number of baseVars are involved", {
  
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