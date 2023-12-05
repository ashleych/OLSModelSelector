#' Format model results of model eval
#'
#' @param allModelEvaluated model list with diagnostic tests
#' @return condensedModelEval Condensed model eval
#' @export
#' @examples
#' formatModelEvalResults(allModelEvaluated)

formatModelEvalResults <- function(allModelEvaluated) {
  evaluatedModels <- copy(allModelEvaluated)
  
  
  orderColumns <- function(DT) {
    firstSet <- c('model', 'Intercept')
    secondSetPattern <- "^Var" 
    estimatesPattern<-"^Estimate"
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
      sortColumns(estimatesPattern),
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
  # split into response variable and independent variables
  evaluatedModels[, `:=`(c("ResponseVar", "IndepVar"),
                         tstrsplit(model, "[~]", fill = NA))]
  
  # split into independente variables into other columns and save it in a different DT for instance mev1 + Mev2 wil be split into two columns
  # the + sign is removed
  
  splitTemp <- evaluatedModels[,  tstrsplit(IndepVar, "[+]", fill = NA)]
  
  colnames(splitTemp) <-
    paste0("Variable", 1:length(names(splitTemp))) #rename these are variable1,2 etc
  splitTemp[, names(splitTemp) := lapply(.SD, trimws), .SDcols = names(splitTemp)] # remove whitespaces if any
  variablesNamesSequence <- colnames(splitTemp)
  
  evaluatedModels <- cbind(evaluatedModels, splitTemp)
  evaluatedModels$IndepVar <- NULL
  
  colnames(evaluatedModels) <-
    gsub("\\(|\\)", "", colnames(evaluatedModels)) # to remove paranthesis from intercept column names
  ids = c(
    "model",
    "ResponseVar",
    'Intercept',
    "VifMax",
    "VIF_error",
    "se.Intercept",
    "t.Intercept",
    "p.Intercept",
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
  
  # TODO: see that vif_error is not available in some one Simrans run of the function.  TO investigate why vif_error didnt show up,. For now fixing this by subsetting the ids for only those ids that are colnames in the data
  ids <- ids[ids %in% colnames(evaluatedModels)]
  
  
  #melt the table
  ids <- c(ids, variablesNamesSequence)
  # these cols will not be 'melted'
  
  
  meltedEval <-
    data.table::melt(evaluatedModels,
                     id = ids,
                     variable.factor = FALSE)
  meltedEval <- meltedEval[!is.na(value)] #remove NAs
  
  meltedEval$variableRenamed <- NA
  
  
  
  splitVariablesDT <-
    meltedEval[,  tstrsplit(
      variable,
      "[.]",
      fill = NA,
      names = c('estimateType', 'variableSuffix')
    )] # split the variable names to remove the prefixes like t.Mev1, p.mev2,p.vif etc
  splitVariablesDT[, names(splitVariablesDT) := lapply(.SD, trimws), .SDcols = names(splitVariablesDT)]
  
  noEstimatesRows <- is.na(splitVariablesDT$variableSuffix)
  splitVariablesDT[noEstimatesRows, variableSuffix := estimateType] # copy teh variable suffix from EstimateType
  splitVariablesDT[noEstimatesRows, estimateType := NA] # make the estimate type for these NA
  meltedEval <- cbind(meltedEval, splitVariablesDT)
  meltedEval$NewVariableSuffix <- ""
  
  for (v in variablesNamesSequence) {
    meltedEval[variableSuffix == get(v), NewVariableSuffix := v]
  }
  
  #   variable  ,variableSuffix,variableRenamed
  meltedEval$variable <- NULL
  
  
  # merge NEwVariableSuffix and estimate tyoe
  meltedEval[, variable := ifelse(
    !is.na(estimateType),
    paste0(estimateType, ".", NewVariableSuffix),
    paste0("Estimate", ".", NewVariableSuffix)
  )]
  meltedEval$estimateType <- NULL
  meltedEval$NewVariableSuffix <- NULL
  
  
  formula <-
    paste0(paste(ids, collapse = "+"), " ~ ", "variable")
  wideModelEval <-
    dcast(meltedEval, formula = formula , value.var = "value") #make into wide format
  
  
  condensedModelEval <- orderColumns(wideModelEval)
  return(condensedModelEval)
}
