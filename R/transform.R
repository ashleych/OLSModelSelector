#' Identify the response variable, and associated transformation rule
#'
#' @param depVar Dependent variable
#' @param transformRule transformation rule
#' @param noofLags No of lags
#' @param trainIndex index till which actual data is present
#' @export
#' @examples

transformRuleAdd<- function(depVar, transformRule) {

  if (!exists("transformConfig_Df")) {
    transformConfig_Df <- data.table(
      depVar = depVar,
      transformRule = transformRule
    )
  } else {
    new<- data.table(
      depVar = depVar,
      transformRule = transformRule

    )
    transformConfig_Df <- rbind(
      transformConfig_Df,new)
  }
  list2env(list(transformConfig_Df=transformConfig_Df), envir = .GlobalEnv)
}





