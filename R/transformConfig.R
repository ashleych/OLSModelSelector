#' Checks if transformConfig data.table exists
#'
#' Helper function to check if transformation rules are available in the environment
#' @param var name of the response variable that is to be untransformed
#' @param orderList returns the orderList in the form that can be used to create a transformOrderClass
#' @examples
#' transformConfigCheck("DR_logit_FD")

transformConfigCheck <- function(var) {
  stopifnot(exists("transformConfig") &
              is.data.frame(transformConfig))
  setDT(transformConfig)
  stopifnot(var %in% unique(transformConfig$varName))
  orderList <- transformConfig[varName == var, ]
  # stopifnot(unique(transformConfig$baseVarName) %in% colnames(macrodata))
  # stopifnot(unique(transformConfig$varName) %in% colnames(macrodata))
  return(orderList)
}

