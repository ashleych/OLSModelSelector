#' Format model results of model eval
#'
#' @param allModelEvaluated model list with diagnostic tests
#' @return condensedModelEval Condensed model eval
#' @export
#' @examples
#' formatModelEvalResults(allModelEvaluated)

formatModelEvalResults <- function(allModelEvaluated) {
  renameAsOrderedVariables <- function (row) {
    #function to rename the mec
    renamedVar <- NA
    model <- row$model
    model_LHS <- trimws(unlist(strsplit(model, "[~]"))[[1]])
    RHS_combined <- trimws(unlist(strsplit(model, "[~]"))[[2]])
    model_RHS <- trimws(unlist(strsplit(RHS_combined, '[+]')))
    for (index in 1:length(model_RHS)) {
      pattern <- model_RHS[index]
      text <- row$variable
      replacement <- paste0("Variable", index)
      if (grepl(paste0(pattern, "$"), text))
      {
        row$variableRenamed <- gsub(pattern, replacement, text)
        renamedVar <- gsub(pattern, replacement, text)
      }
      if (!is.na(renamedVar)) {
        break
      }
    }
    
    return(renamedVar)
    
  }
  orderColumns <- function(DT) {
    firstSet <- c('model', 'Intercept')
    secondSetPattern <- "^Var" # estimates
    thirdSetPattern <- "^se.Var"
    fourthSetPattern <- "^t.Var"
    fifthSetPattern <- "^p.Var"
    sixthSetPattern <- "^VIF"
    lastSet <-
      c(
        "R2",
        "adjR2",
        "MAPE_test",
        "MAPE_training",
        "RMSE",
        "pF",
        "BP_statistic",
        "BP_pvalue",
        "dw_statistic",
        "dw_pvalue",
        "shapiro_statistic",
        "shapiro_pvalue",
        "adf_statistic",
        "adf_pvalue",
        "KPSS_statistic",
        "KPSS_pvalue",
        "dfdirectioncheck",
        "bg_statistic",
        "bg_pvalue",
        "Heteroskedasticity",
        "BP_results",
        "Normality",
        "shapiro_results",
        "Autocorrelation_bg",
        "bg_results",
        "Autocorrelation",
        "dw_results",
        "Stationarity",
        "KPSS_results",
        "adf_results",
        "significant",
        "FinalResults"
      )
    
    # Get column names of the data table
    cols <- colnames(DT)
    
    # Create a function to sort columns based on the patterns
    sortColumns <- function(pattern) {
      pattern_cols <- grep(pattern, cols, value = TRUE)
      pattern_cols_sorted <-
        pattern_cols[order(factor(pattern_cols, levels = cols))]
      return(pattern_cols_sorted)
    }
    
    # Arrange columns based on sets and patterns
    ordered_cols <- c(
      firstSet,
      sortColumns(secondSetPattern),
      sortColumns(thirdSetPattern),
      sortColumns(fourthSetPattern),
      sortColumns(fifthSetPattern),
      sortColumns(sixthSetPattern),
      lastSet
    )
    print(ordered_cols)
    # Rearrange the columns in the data table
    DT <- DT[, ..ordered_cols]
    return(DT)
  }
  colnames(allModelEvaluated) <-
    gsub("\\(|\\)", "", colnames(allModelEvaluated)) # to remove paranthesis from intercept column names
  ids = c( "model", 'Intercept', "VifMax", "se.Intercept", "t.Intercept", "p.Intercept", "R2", "adjR2", "MAPE_test", "MAPE_training", "RMSE", "pF", "BP_statistic", "BP_pvalue", "dw_statistic", "dw_pvalue", "shapiro_statistic", "shapiro_pvalue", "adf_statistic", "adf_pvalue", "KPSS_statistic", "KPSS_pvalue", "dfdirectioncheck", "bg_statistic", "bg_pvalue", "Heteroskedasticity", "BP_results", "Normality", "shapiro_results", "Autocorrelation_bg", "bg_results", "Autocorrelation", "dw_results", "Stationarity", "KPSS_results", "adf_results", "significant", "FinalResults" ) #melt the table
  #
  meltedEval <-
    data.table::melt(allModelEvaluated,
                     id = ids,
                     variable.factor = FALSE)
  meltedEval <- meltedEval[!is.na(value)] #remvoe NAs
  
  meltedEval$variableRenamed <- NA
  for (row in 1:nrow(meltedEval)) {
    meltedEval$variableRenamed[row] <-
      check_variable_order(meltedEval[row]) #rename variables as Var1, var 2 etc
  }
  
  
  formula <-
    paste0(paste(ids, collapse = "+"), " ~ ", "variableRenamed")
  wideModelEval <-
    dcast(meltedEval, formula = formula , value.var = "value") #make into wide format
  
  
  condensedModelEval <- orderColumns(wideModelEval)
  return (condensedModelEval)
}