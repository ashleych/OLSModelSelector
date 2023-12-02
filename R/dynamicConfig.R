#' Identify the response variable, the laggedvariable name and number of lags to be used.
#'
#' @param depVar Dependent variable
#' @param lagVar Independent variable
#' @param noofLags No of lags
#' @param trainIndex index till which actual data is present
#' @export
#' @examples

configDynamic<- function(depVar, lagVar, noofLags, trainIndex) {
# Make unique entries only in this df
  if (!exists("configDynamic_Df")) {
    configDynamic_Df <- data.table(
      depVar = depVar,
      lagVar = lagVar,
      noofLags = noofLags,
      trainIndex = trainIndex
    )
  } else {
   new<- data.table(
      depVar = depVar,
      lagVar = lagVar,
      noofLags = noofLags,
      trainIndex = trainIndex
    )
  configDynamic_Df <- rbind(
    configDynamic_Df,new)
  }
  # make the rows unique
  configDynamic_Df<-unique(configDynamic_Df)
  
  list2env(list(configDynamic_Df=configDynamic_Df), envir = .GlobalEnv)
}


#' Check if dynamic model or not
#'
#' @param depVar Dependent variable
#' @param lagVar Independent variable
#' @param noofLags No of lags
#' @param trainIndex index till which actual data is present
#' @export
#' @examples

dynamicCheck <- function(selectedModel) {
  model_LHS <- trimws(unlist(strsplit(selectedModel, "[~]"))[[1]])
  RHS_combined <- trimws(unlist(strsplit(selectedModel, "[~]"))[[2]])
  model_RHS <- trimws(unlist(strsplit(RHS_combined, '[+]')))

  if(exists("configDynamic_Df",envir = .GlobalEnv)){
  dynamic <- configDynamic_Df[depVar == model_LHS & lagVar %in% model_RHS, ]
  }
  else
    data.table()
}


