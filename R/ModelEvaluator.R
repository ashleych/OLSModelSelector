#' Evaluates and classifies models into pass or fail
#'
#'Different from modelDiagnoser in that the diagnoer only computed the diagnostics, it doesnt classify it
#' @param Heteroskedasticity_threshold Threshold for pvalue for BP test
#' @param normality_threshold Threshold for pvalue for Shapiro test
#' @param pvalue_threshold Threshold for tests of significance
#' @param Autocorrelation_threshold Threshold for DurbinWatson test
#' @param Stationarity_threshold thresholds for ADF tests
#' @return Consolidated model results with tests classified as PASS or fail
#' @export
#' @examples
#' modelEvaluator(allModelObjects) # allModelObjects created by modelDeveloper()
modelEvaluator <- function(allModelsDiagnostics,Heteroskedasticity_threshold=0.1,normality_threshold=0.1,
                           pvalue_threshold =0.05,
                           Autocorrelation_threshold=0.1, Stationarity_threshold =0.1){

  #setDT(allModelsDiagnostics)
  allModelsDiagnostics$Heteroskedasticity <-
    ifelse(
      allModelsDiagnostics$BP_pvalue < Heteroskedasticity_threshold,
      "Heteroskedasticity",
      "Homoskedasticity"
    )

  allModelsDiagnostics$BP_results<-allModelsDiagnostics$Heteroskedasticity

  allModelsDiagnostics$Normality <-
    ifelse(
      allModelsDiagnostics$shapiro_pvalue < normality_threshold,
      "Residuals Not Normally Distributed",
      "Residuals Normally distributed"
    )

  allModelsDiagnostics$shapiro_results <- allModelsDiagnostics$Normality


  allModelsDiagnostics$Autocorrelation <-
    ifelse(
      allModelsDiagnostics$dw_pvalue < Autocorrelation_threshold,
      "Auto-Correlation",
      "No Auto-Correlation"
    )

  allModelsDiagnostics$dw_results<-allModelsDiagnostics$Autocorrelation

  allModelsDiagnostics$Stationarity <-
    ifelse(allModelsDiagnostics$adf_pvalue > Stationarity_threshold,
           "Non-Stationary",
           "Stationary")

  allModelsDiagnostics$KPSS_TrendStationarity <-
    ifelse(allModelsDiagnostics$KPSS_pvalue < Stationarity_threshold,
           "Non-Stationary",
           "Stationary")

  allModelsDiagnostics$adf_results <- allModelsDiagnostics$Stationarity
  #check if statistically significant pvalue<0.05
  vars <-
    trimws(unique(unlist(allModelsDiagnostics[, tstrsplit(model, "[~]")][, V1 :=
                                                                           NULL][, strsplit(V2, "[+]")])))
  temp<-allModelsDiagnostics[,lapply(.SD,function(x){
    ifelse(x<pvalue_threshold,0,1)
  }),.SDcols= paste0("p.",vars)]
  allModelsDiagnostics$significant <- ifelse(rowSums(temp,na.rm = TRUE)==0,"Significant",'Not Significant')


  allModelsDiagnostics[, FinalResults := ifelse(
    ( dfdirectioncheck==1 &
        Heteroskedasticity == "Homoskedasticity" &
        Normality == "Residuals Normally distributed" &
        Autocorrelation == "No Auto-Correlation" &
        significant == "Significant" &
        Stationarity == "Stationary"
    ) ,
    "PASS",
    "FAIL"
  )]

  return(allModelsDiagnostics)
}

# consolidatedModelResults <- modelEvaluator(
#   summaryallModels,
#   Heteroskedasticity_threshold = 0.1,
#   normality_threshold = 0.1,
#   pvalue_threshold = 0.05,
#   Autocorrelation_threshold = 0.1,
#   Stationarity_threshold = 0.1
# )
