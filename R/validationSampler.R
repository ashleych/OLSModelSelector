#' Splits into training, test and forecast datasets
#'
#' @param macroecodata A Dataset with macroeconomic variables and values and Default rates.
#' @param rn_train Rowindex or rownumbers of macroecodata as vector that forms training dataset.
#' @param rn_test Rowindex or rownumbers of macroecodata as vector that forms testing dataset.
#' @param rn_forecast Rowindex or rownumbers of macroecodata as vector that forms training dataset.
#' @return Training, test and forecast datasets in global Environment
#' @export
#' @examples
#' validationSampler(macroecodata,1:29,30:33,1:48)

validationSampler <- function(macroecodata,rn_train,rn_test,rn_forecast){
setDT(macroecodata)
#Enter row numbers for trainnig, test and for prediction. please note that the rownumbers in Excel include headers.
train_df <- macroecodata[rn_train,]
test_df <- macroecodata[rn_test, ]
forecast_df <- macroecodata[rn_forecast, ]
list2env(list(train_df=train_df,test_df=test_df,forecast_df=forecast_df), envir = .GlobalEnv)
}


#' Generates all models using combination of dependent and independent variables provided
#'
#' @param LHS_vars LHS of the regression function of the form Y ~ X1 + X2. This is the vector of response variable. Example default rates, or PIT PDs for default rate forecasting
#' @param RHS_vars Right hand side of the regression equationof the form Y ~ X1 + X2. A vector of dependent variables,  macroeconomic variable names for instance. These should be available in the data provided to  validationSampler.
#' @param no_of_vars Maximum Number of dependent variables to be used in the model building.
#' @param trainData Training dataframe on which model is to be built
#' @param multiple Boolean to denote if a single model to be built using combinations of dependent variables or all combinations of dependent variables to be used. Default is \code{multiple = TRUE}
#' @return All model objects.Note that this is not a dataframe but S3 objects. If you want to explore particular models, then one can use the subset operator
#' @export

modelDeveloper <- function(LHS_vars, RHS_vars,multiple=TRUE,no_of_vars,trainData=train_df){

  LHS_all <- paste0(LHS_vars," ~ ")

  if (multiple== TRUE){len= 1} else {len=no_of_vars} # if multiple models are not needed, and we want to see a model with just the LHS and RHS
  RHS_all<-  unlist(sapply(len:no_of_vars, function(x) {
    unlist(combn(RHS_vars,x,simplify = FALSE, FUN=paste0,collapse="+"))
  }))

  allFormulae<-unlist(lapply(LHS_all, function(x) {paste0(x,RHS_all)}))

  allModelObjects <- lapply(allFormulae,
                             function(x) lm(as.formula(x), data = trainData)
  )
  names(allModelObjects)<- allFormulae

  return(allModelObjects)
}



