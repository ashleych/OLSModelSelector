#' getFormulaofAllSignificantModelsTurboFastLM
#'
#' @param allFormulae List of strings containing all formulae or model names
#' @param yIndex Index of the response variable column in train_df
#' @param train_df Train df - this is used to build the models, ie the train data
#' @return Returns a list of all formulae of models that are significant, the models themselves arent passed as this can be a very long list depending on the number of variables
#' @export
#' @examples
#' getFormulaofAllSignificantModels(-c("z_index ~ cp_general_government_debt_to_gdp_ratio_fd",2,train_df)

getFormulaofAllSignificantModelsTurboFastLM <-
  function(combinationsMatrix,
           yIndex,
           train_df,
           pValueThreshold = 0.1,
           ncores = 4) {
    noOfCombos <- dim(allFormuleRHSIndexes)[[1]]
    

    maxPvalues <- pbmclapply(1:noOfCombos, function(i) {
      dataMat <-
        as.matrix(na.omit(train_df[, .SD, .SDcols = c(yIndex, combinationsMatrix[i,])])) #concatenating teh yindex data as the first column
      
      # mdl <-
      #   RcppArmadillo::fastLmPure(
      #     dataMat[,-1], dataMat[, 1]) # all columns apart from the first will be the RHS, and the first will be the RHS
      #
      #     pvalue <-
      #       2 * pt(
      #         abs(mdl$coefficients / mdl$stderr),
      #         mdl$df.residual,
      #         lower.tail = FALSE
      #       )
      #     return(max(pvalue[-1])) #remove pvalues for teh intercept
      
      
      pvalue <- tryCatch({
        mdl <-
          RcppArmadillo::fastLmPure(cbind(1,dataMat[, -1]), dataMat[, 1]) # all columns apart from the first will be the RHS, and the first will be the RHS
        #fastLMPure does zero intercept by default, so we need to cbind 1 to get it to generate with intercept
        
        pvalue <-
          2 * pt(abs(mdl$coefficients / mdl$stderr),
                 mdl$df.residual,
                 lower.tail = FALSE)
        
      },
      error = function(cond) {
        NULL
      },
      warning = function(cond) {
        NULL
      })
      
      
      if (!is.null(pvalue)) {
        return(max(pvalue[-1])) #Remove hte one pertaining to the intercept
      }
      return(NULL)
    },mc.preschedule = TRUE,mc.cores = ncores)
    
    index_of_true <- which(maxPvalues < pValueThreshold)
    
    # # Select elements from RHS_All using the index_of_true
    selectedRHSCombos <- combinationsMatrix[index_of_true,]
    # 
    # lhs<-paste0(LHS_var," ~ ")
    # selectedRhs <-lapply(selectedRHS,function(x) paste0(x,collapse = " + "))
    # 
    # selectedModels<- paste0(lhs, selectedRhs)
    # print(paste0("No of models created : ",length(RHS_all)))
    # print(paste0("No of models found significant :",length(selectedModels)))
    return(selectedRHSCombos)
    
  }