#' Custom UnTransformFunction function
#' @param transformFunction Function that is to be called to transform
#' @param ... other paramaters as necessary for the custom transform function
#' @return Vector of converted values
#' @export
#' @examples
#' model_1<- customTransform(zScoreCompute,...)

UnTransformFunctionWrapper <- function(unransformFunction, ...){
  values <- unransformFunction(...)
  return(values)
  
}

#' Custom transform function
#' @param zScoreCompute Function that is to be called to transform to zscores from PDs
#' @param pds orignal pd values that are to be transformed
#' @param rho Correlation value that is to be passed
#' @return List of transformed vales, the avg zindex, mean pd and rho
#' @export
#' @examples
#' model_1<- zScoreCompute(c(0.1,0.2),0.023)

zScoreCompute <- function(pds,rho,...) {
  avg<-mean(pds,na.rm=TRUE)
  zindex<-(qnorm(avg) - sqrt(1 - rho) * qnorm(pds)) / sqrt(rho)
  avgZindex<-mean(zindex,na.rm=TRUE)
  transformedValues=zindex-avgZindex
  return(list(transformedValues=transformedValues,avgZindex=avgZindex,meanPd=avg,rho=rho))
  
}

#' Zscore untransform function
#' @param zScoreCompute Function that is to be called to transform to zscores from PDs
#' @param originalUntransformedPDs orignal pd values that were transformed to zscores, this is needed to get the mean of zscores which is used for untranssforming. for example in z_index ~ mev1+mev2,these are the values(actuals usually) used to obtain the z_index 
#' @param zScoresTobeUntransformed Values to be untransformed to PDs, usually this is the output of regression for example in z_index ~ mev1+mev2,  this is the predicted z-index
#' @return List of untransformed vales, usually represent the PDs
#' @export
#' @examples
#' model_1<- untransformZscore(c(0.1,0.2),c(0.1,0.2),0.023)
untransformZscore <- function(valuesToBeTransformed,rho,originalUntransformedValues ){
  zScoreCompute <- zScoreCompute(originalUntransformedValues,rho = rho)
  rho<-zScoreCompute$rho
  avgZindex<-zScoreCompute$avgZindex
  # zScoresTobeUntransformed+avgZindex to bring it back to non standardised form
  avgAddedBack<-valuesToBeTransformed+avgZindex
  
  untransformedValues<- pnorm((qnorm(mean(originalUntransformedValues,na.rm=TRUE)) - sqrt(rho) * avgAddedBack) / sqrt(1 - rho))
  return(untransformedValues)
  
}