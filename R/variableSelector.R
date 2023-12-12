#' Filter variable list by running single variable regressions and selecting models based on R-squared higher than threshold (default threshold is 5%). This
#' runs the modelDeveloper function with no_of_vars  =  1
#'
#' @param LHS_vars LHS of the regression function of the form Y ~ X1 + X2. This is the vector of response variable. Example default rates, or PIT PDs for default rate forecasting
#' @param RHS_vars Right hand side of the regression equationof the form Y ~ X1 + X2. A vector of dependent variables,  macroeconomic variable names for instance. These should be available in the data provided to  validationSampler.
#' @param trainData Training dataframe on which model is to be built
#' @param R2Threshold Threshold for Rsquared -default at 0.1
#' @return Returns a vector with list of selected Variables
#' @export
#' @examples
#' variableSelector(LHS_vars = c("DR_logit_FD"),RHS_vars = vars,trainData =train_df)

variableSelector <- function(LHS_vars ,
                             RHS_vars ,
                             trainData = train_df,
                             R2Threshold = 0,
                             pValueThreshold = 0) {
  
  
  selectedVarsOnBasisofRSquared<-c()
  selectedVarsOnBasisofPValue<-c()
  allUnivariateModels <-
    modelDeveloper(
      LHS_vars = LHS_vars,
      RHS_vars = RHS_vars,
      trainData = train_df,
      no_of_vars = 1
    )
  
  if (R2Threshold > 0) {
    R2       <- sapply(allUnivariateModels, function(x)
      summary(x)$r.squared)
    names(R2) <- names(allUnivariateModels)
    
    selectedModelNamesOnBasisofRSquared <-
      names(R2[(R2 > R2Threshold)])
    selectedVarsOnBasisofRSquared <-
      unique(trimws(sapply(strsplit(selectedModelNamesOnBasisofRSquared, "[~]"), function(x)
        x[[2]])))
  }
  
  
  # Selected models on the basis of pvalues
  if (pValueThreshold > 0) {
    
    modelSelectionsBasedOnPvalueBoolean <- sapply(allUnivariateModels,
                                                  function(model) {
                                                    return(names(model))
                                                  })
    modelSelectionsBasedOnPvalueBoolean <- sapply(allUnivariateModels,
                                                  function(model) {
                                                    return(is_significant(model,pValueThreshold =pValueThreshold))
                                                  })
    
    selectedModelNamesOnBasisofPValue <-
      names(allUnivariateModels)[modelSelectionsBasedOnPvalueBoolean]
    selectedVarsOnBasisofPValue <-
      unique(trimws(sapply(strsplit(selectedModelNamesOnBasisofPValue, "[~]"), function(x)
        x[[2]])))
  }
  
  
  if ((length(selectedVarsOnBasisofRSquared) > 0) & (length(selectedVarsOnBasisofPValue) > 0)) {
    print("Returning values that are selected on the basis of both pvalue and rsquared")
    return(intersect(selectedVarsOnBasisofPValue, selectedVarsOnBasisofRSquared) )
    
  }
  if (length(selectedVarsOnBasisofPValue) > 0){
    print("Returning values that are selected on the basis of  pvalue ")
    
    return(selectedVarsOnBasisofPValue)
    
  }
  print("Returning values that are selected on the basis of  rsquared ")

  return(selectedVarsOnBasisofRSquared)
  
  }
