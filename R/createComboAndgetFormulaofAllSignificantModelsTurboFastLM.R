#' createComboAndgetFormulaofAllSignificantModelsTurboFastLM
#'
#' @param baseVariables Names of the untransformed variables that you want to use in the RHS, so if there is oil_price and oil_price_ln, provide just oil_price, it will identify that oil_price_ln is a transformation using "like" operator
#' @param numberOfVariables Number of variables to be used in each formula. See strictMax for 'upto' vs 'exact' match
#' @param yIndex Index of the response variable column in train_df
#' @param train_df Train df - this is used to get the list of all macrovariables that have a likeness to each variable in baseVariables
#' @return Returns a list of all formulae of models that are significant, the models themselves arent passed as this can be a very long list depending on the number of variables
#' @export
#' @examples
#' createComboAndgetFormulaofAllSignificantModelsTurboFastLM(-c("z_index ~ cp_general_government_debt_to_gdp_ratio_fd",2,train_df)

createComboAndgetFormulaofAllSignificantModelsTurboFastLM <-
  function(baseVars,
           numberOfVariables,
           yIndex,
           train_df,
           pValueThreshold = 0.1,
           ncores = 4,saveToDisk=TRUE) {
    
    checkSignificance <- function(combinationsMatrix) {
      noOfCombos <- dim(combinationsMatrix)[[1]]
      
      
      maxPvalues <- pbmclapply(1:noOfCombos, function(i) {
        dataMat <-
          as.matrix(na.omit(train_df[, .SD, .SDcols = c(yIndex, combinationsMatrix[i, ])])) #concatenating teh yindex data as the first
        
        pvalue <- tryCatch({
          mdl <-
            RcppArmadillo::fastLmPure(cbind(1, dataMat[, -1]), dataMat[, 1]) # all columns apart from the first will be the RHS, and the first will be the RHS
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
      }, mc.preschedule = TRUE, mc.cores = ncores)
      
      index_of_true <- which(maxPvalues < pValueThreshold)
      
      # # Select elements from RHS_All using the index_of_true
      selectedRHSCombos <- combinationsMatrix[index_of_true, ]
      #
      # lhs<-paste0(LHS_var," ~ ")
      # selectedRhs <-lapply(selectedRHS,function(x) paste0(x,collapse = " + "))
      #
      # selectedModels<- paste0(lhs, selectedRhs)
      # print(paste0("No of models created : ",length(RHS_all)))
      # print(paste0("No of models found significant :",length(selectedModels)))
      #
      return(selectedRHSCombos)
    }
    
    indexStart <- numberOfVariables
    
    allCombosOfBaseVars <-
      pbmclapply(indexStart:numberOfVariables, function(x) {
        combn(baseVars, x, simplify = FALSE)
      }, mc.cores = ncores, mc.preschedule = TRUE)
    allCombosOfBaseVars <- allCombosOfBaseVars[[1]]
    
    print(paste0(
      " Number of baseVar combinations :",
      length(allCombosOfBaseVars)
    ))
    
    # Get all related variables against named lists
    relatedVarsList <- list()
    for (b in baseVars) {
      relatedVarsList[[b]] <-
        grep(paste0("^", b), names(train_df), value = FALSE)
    }
    combinations_df_list <-
      pbmclapply(1:length(allCombosOfBaseVars), function(i) {
        relatedVarsListForThatCombo <-
          relatedVarsList[allCombosOfBaseVars[[i]]]
        
        combination <-
          unname(as.matrix(expand.grid(relatedVarsListForThatCombo)))
        selectedRHSCombos <-
          checkSignificance(combinationsMatrix = combination)
        if(saveToDisk){
          str1 <- paste0('selectedRHSCombos_',i,'.rds')
          fn<-  paste0(sub('\\..*', '', str1), format(Sys.time(),'_%Y%m%d_%H%M%S'), '.rds')
         fn<- file.path(getwd(),"result",fn)
          saveRDS(selectedRHSCombos, file = fn)
          return(fn)
        } else {
          return(selectedRHSCombos)
        }
   
        
      }, mc.cores = 1)
    
    return(combinations_df_list)
  }