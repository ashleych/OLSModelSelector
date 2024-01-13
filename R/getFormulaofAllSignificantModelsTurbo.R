#' getFormulaofAllSignificantModelsTurbo
#'
#' @param allFormulae List of strings containing all formulae or model names
#' @param train_df Train df - this is used to build the models, ie the train data
#' @return Returns a list of all formulae of models that are significant, the models themselves arent passed as this can be a very long list depending on the number of variables
#' @export
#' @examples
#' getFormulaofAllSignificantModels(-c("z_index ~ cp_general_government_debt_to_gdp_ratio_fd",,train_df)

getFormulaofAllSignificantModelsTurbo <- function(allFormulae,train_df, pValueThreshold = 0.1,ncores=8){

  modelSelections <- mclapply(allFormulae,
                            function(x) {
                              model <- lm(as.formula(x), data = train_df)
                              return(is_significant(model,pValueThreshold ))
                            }, mc.cores = getOption("mc.cores", ncores))

  
  significantModelsFormule<-allFormulae[unlist(modelSelections)]
  return(significantModelsFormule)
}