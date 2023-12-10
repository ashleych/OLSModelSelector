#' Get Unrelated variables combinations, to avoid using variable combinations in the RHS of the formula that are transformations of the same variable
#'
#' @param LHS_vars Left hand side variable name, the response variable as a vector eg c("DR")
#' @param baseVariables Names of the untransformed variables that you want to use in the RHS, so if there is oil_price and oil_price_ln, provide just oil_price, it will identify that oil_price_ln is a transformation using "like" operator
#' @param train_df Train df - this is used to get the list of all macrovariables that have a likeness to each variable in baseVariables
#' @return Returns a list of all formulae that do not have related variables in the same formula
#' @export
#' @examples
#' getUnRelatedVariableCombinations(c("DR"),c("oil_price",'gdp_ratio'),train_df)
getUnRelatedVariableCombinations <- function(LHS_vars,baseVariables,train_df) {
  relatedVarsList = list()
  for (b in baseVariables) {
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
