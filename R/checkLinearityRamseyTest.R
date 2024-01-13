#' checkLinearityRamseyTest
#'
#' @param modelsList List of strings containing all formulae or model names
#' @param trainingData ususally train_df - this is used to build the models, ie the train data
#' @param pvalueThreshold the significance level usually 0.05 or 0.1
#' @return Returns a list of all  adjusted p values from Ramsey test
#' @export
#' @examples
#' checkHACCorrectedPValues(allModelEvaluated_f$model,train_df)

checkLinearityRamseyTest <- function(modelsList,trainingData,pvalueThreshold=0.05){
  
  library(lmtest)
  # HACPassValues<-c()
  pvalue<-c()
  for (model in modelsList) {

    p <-resettest(as.formula(model),power = 2:3, type=c("fitted"),data = train_df)[["p.value"]]
    pvalue<-c(pvalue,p)
  }
  
  return(pvalue)#return models list that passed HACValues
  
}
