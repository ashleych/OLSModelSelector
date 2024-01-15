#' getFormulaofAllSignificantModelsTurbo
#'
#' @param allFormulae List of strings containing all formulae or model names
#' @param train_df Train df - this is used to build the models, ie the train data
#' @return Returns a list of all formulae of models that are significant, the models themselves arent passed as this can be a very long list depending on the number of variables
#' @export
#' @examples
#' getFormulaofAllSignificantModels(-c("z_index ~ cp_general_government_debt_to_gdp_ratio_fd",train_df)

getFormulaofAllSignificantModelsTurbo <- function(allFormulae,train_df, pValueThreshold = 0.1,ncores=8){
library(pbmcapply)
  checkModelSignificanceAnon <- function(f,train_df) {
    print(f)
    
    flag <-FALSE
    tryCatch(
      {
        

      },
      error = function(cond) {
        flag <-FALSE
      },
      warning = function(cond) {
        flag <-FALSE
      }
    )
    
    return(flag)
  }
  
  # modelSelections <- pbmclapply(allFormulae,
  #                           function(x) {
  #                             model <- lm(as.formula(x), data = train_df)
  #                             return(is_significant(model,pValueThreshold ))
  #                           }, mc.cores = getOption("mc.cores", ncores),mc.preschedule = TRUE)
  # mdl <- RcppArmadillo::fastLmPure(cbind(1,as.matrix(data[,..rhs])), as.matrix(data[,..LHS_var]))
  # pvalue <- 2*pt(abs(mdl$coefficients/mdl$stderr), mdl$df.residual, lower.tail=FALSE)
  # max(pvalue)>pvaluethreshold 
  
  modelSelections <- pbmclapply(1:length(allFormulae),
                                function(i) {
                                  model <- lm(as.formula(allFormulae[[i]]), data = train_df)
                                  return(is_significant(model,pValueThreshold ))
                                }, mc.cores = getOption("mc.cores", ncores),mc.preschedule = TRUE)
  # modelSelections <-lapply(allFormulae,
  #                           function(x,train_df=train_df) {
  #                             checkModelSignificanceAnon(x,train_df=train_df)
  #                           }, mc.cores = getOption("mc.cores", ncores))
  # modelSelections <-pbmclapply(allFormulae,
  #                          function(x,train_df=train_df) {
  #                            model <- lm(as.formula(x), data = train_df)
  #                           
  #                           return(is_significant(model,pValueThreshold ))
  #                          },)
  
  significantModelsFormule<-allFormulae[unlist(modelSelections)]
  return(significantModelsFormule)
}
