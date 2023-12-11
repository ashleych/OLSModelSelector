#' Get Unrelated variables combinations, to avoid using variable combinations in the RHS of the formula that are transformations of the same variable
#'
#' @param LHS_vars Left hand side variable name, the response variable as a vector eg c("DR")
#' @param baseVariables Names of the untransformed variables that you want to use in the RHS, so if there is oil_price and oil_price_ln, provide just oil_price, it will identify that oil_price_ln is a transformation using "like" operator
#' @param train_df Train df - this is used to get the list of all macrovariables that have a likeness to each variable in baseVariables
#' @return Returns a list of all formulae that do not have related variables in the same formula
#' @export
#' @examples
#' getUnRelatedVariableCombinations(c("DR"),c("oil_price",'gdp_ratio'),train_df)
getUnRelatedVariableCombinations <- function(LHS_vars,baseVariables,train_df,numberOfVariables,strictMax=TRUE) {
  # Check if all basevariables have some column with tht name
  colsAvailable<-names(train_df)
  
  for (b in baseVariables) {
    if(length(colsAvailable[grep(b, colsAvailable)])==0){
      print(paste0(b,"or a column with a similar name is not found in train_df"))
      stop("Some columns in basevariables not found in the train_df")
    }
  }
  allFormulae<-c()
  
  
  numberOfVariables<-3
  indexStart<-1
  if(strictMax){
    indexStart<-numberOfVariables
  }

  allCombosOfBaseVars<- sapply(indexStart:numberOfVariables, function(x) {
    combn(baseVariables,x,simplify = FALSE,FUN=c)
  })
  
  allCombosOfBaseVars<-combn(baseVariables,numberOfVariables,simplify = FALSE,FUN=c)
  getFormulaeForParticularBaseVarCombo <-function(LHS_vars,baseVariablesCombination,train_df){
    
    
    relatedVarsList = list()
    for (b in baseVariablesCombination) {
      relatedVarsList[[b]] <- names(train_df)[names(train_df) %like% b]
    }
    
    
    combinations <- do.call(expand.grid, relatedVarsList)
    LHS_all <- paste0(LHS_vars, " ~ ")
    RHS_all <-
      apply(combinations, 1, function(row)
        paste(row, collapse = " + "))
    allFormulae <-
      unlist(lapply(LHS_all, function(x) {
        paste0(x, RHS_all)
      }))
    
    return(allFormulae)
  }
  
  
  for (baseVarCombo in allCombosOfBaseVars) {
    formulae<-getFormulaeForParticularBaseVarCombo(LHS_vars,baseVarCombo,train_df)
    allFormulae<-c(allFormulae,formulae)
  }
  return(allFormulae)
}

