#' getFormulaofAllSignificantModels
#'
#' @param allFormulae List of strings containing all formulae or model names
#' @param train_df Train df - this is used to build the models, ie the train data
#' @return Returns a list of all formulae of models that are significant, the models themselves arent passed as this can be a very long list depending on the number of variables
#' @export
#' @examples
#' getFormulaofAllSignificantModels(-c("z_index ~ cp_general_government_debt_to_gdp_ratio_fd",,train_df)

getFormulaofAllSignificantModels <- function(allFormulae,train_df){
  
  is_significant <- function(model, pValueThreshold = 0.1) {
    pValues <- coef(summary(model))[, "Pr(>|t|)"]
    pValues_withoutIntercept <-
      pValues[!names(pValues) %in% '(Intercept)']
    
    # Find the highest absolute value
    max_abs_value <- max(abs(pValues_withoutIntercept))
    if (max_abs_value > pValueThreshold) {
      return(FALSE)
    } else {
      return(TRUE)
    }
    
  }
  
  modelSelections <- lapply(allFormulae,
                            function(x) {
                              model <- lm(as.formula(x), data = train_df)
                              return(is_significant(model))
                            })
  significantModelsFormule<-allFormulae[unlist(modelSelections)]
  return(significantModelsFormule)
}