#' Checks if transformConfig data.table exists
#'
#' Helper function to check if transformation rules are available in the environment
#' @param var name of the response variable that is to be untransformed
#' #' @param transformConfig dataframe that is either sent by the user or found in the global environment
#' @param orderList returns the orderList in the form that can be used to create a transformOrderClass
#' @examples
#' transformConfigCheck("DR_logit_FD")

transformConfigCheck <- function(var,transformConfig) {

  setDT(transformConfig)
  if(! (var %in% unique(transformConfig$varName))){
  
    stop(sprintf("No rules found for untransform for variable : %s. Check if an entry is present in transformConfig for %s  else make an entry ",var))
 
    
  }

  
  orderList <- transformConfig[varName == var, ]
  # stopifnot(unique(transformConfig$baseVarName) %in% colnames(macrodata))
  # stopifnot(unique(transformConfig$varName) %in% colnames(macrodata))
  return(orderList)
}

