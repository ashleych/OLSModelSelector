#' getFormulaofAllSignificantModels
#'
#' @param modelsList List of strings containing all formulae or model names
#' @param trainingData ususally train_df - this is used to build the models, ie the train data
#' @param pvalueThreshold the significance level usually 0.05 or 0.1
#' @return Returns a list of all HAC adjusted p values
#' @export
#' @examples
#' checkHACCorrectedPValues(allModelEvaluated_f$model,train_df)

checkHACCorrectedPValues <- function(modelsList,trainingData,pvalueThreshold=0.05){
  
  library(sandwich)
  library(lmtest)
  # HACPassValues<-c()
  HACAdjustedPvalue<-c()
  for (model in modelsList) {
    modelObj<-modelDeveloper(modelsNamesList = c(model),trainData = trainingData)
    hacObj<-broom::tidy(coeftest(modelObj[[1]], vcov. = vcovHAC(modelObj[[1]])))
    setDT(hacObj)
    
    maxPvalue<-hacObj[term!="(Intercept)",max(abs(p.value))]
    HACAdjustedPvalue<-c(HACAdjustedPvalue,maxPvalue)
  }
  
  return(HACAdjustedPvalue)#return models list that passed HACValues
  
}
