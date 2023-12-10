#' Splits into training, test and forecast datasets
#'
#' @param macroecodata A Dataset with macroeconomic variables and values and Default rates.
#' @param rn_train Rowindex or rownumbers of macroecodata as vector that forms training dataset.
#' @param rn_test Rowindex or rownumbers of macroecodata as vector that forms testing dataset.
#' @param rn_forecast Rowindex or rownumbers of macroecodata as vector that forms training dataset.
#' @return Training, test and forecast datasets in global Environment
#' @export
#' @examples
#' validationSampler(macrodata,1:29,30:33,1:48)

validationSampler <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
print("validation sampler being run")
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list(train_df=train_df,test_df=test_df,forecast_df=forecast_df), envir = .GlobalEnv)

}



#' generates ScenarioClass
#' @exportClass scenarioClass

setClass("scenarioClass", slots=list(scenario_name="character",scenario_input="data.frame",predictions="data.frame"))


